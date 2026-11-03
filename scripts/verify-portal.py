#!/usr/bin/env python3
import fcntl
import json
import os
from pathlib import Path
import pty
import shutil
import struct
import subprocess
import sys
import tempfile
import termios
import threading

from gi.repository import Gio, GLib


REPO = Path(__file__).resolve().parent.parent
SOURCE = Path(os.environ.get("FILECHOOSER_SOURCE", REPO)).resolve()


def main():
    if not os.environ.get("FILECHOOSER_PRIVATE_BUS"):
        env = dict(os.environ, FILECHOOSER_PRIVATE_BUS="1", GIO_USE_VFS="local")
        return subprocess.call(
            ["dbus-run-session", "--", sys.executable, __file__], env=env
        )

    fixture = Path(tempfile.mkdtemp(prefix="filechooser-portal-"))
    (fixture / "child").mkdir()
    for name in ("plain.txt", "image.png", "child/nested.png", "sp ace#?%ü.txt"):
        (fixture / name).write_text("fixture\n")

    compat = os.environ.get("COMPAT") or os.environ.get("COMPAT_DIR")
    if not compat:
        found = sorted(
            path
            for directory in (
                ".emacs.d/elpa",
                ".cache/emacs/elpa",
                ".config/emacs/elpa",
            )
            for path in (Path.home() / directory).glob("compat-[0-9]*")
        )
        compat = str(next((path for path in reversed(found) if path.is_dir()), ""))
    if not compat:
        raise SystemExit("Set COMPAT to the installed compat package directory")

    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 40, 140, 0, 0))
    env = dict(
        os.environ,
        TERM="xterm-256color",
        FILECHOOSER_TEST_ROOT=str(fixture),
        FILECHOOSER_TEST_SERVER=f"filechooser-verify-{os.getpid()}",
    )

    def terminal_session():
        os.setsid()
        fcntl.ioctl(slave, termios.TIOCSCTTY, 0)

    process = subprocess.Popen(
        [
            os.environ.get("EMACS", "emacs"),
            "-Q",
            "-nw",
            "-L",
            compat,
            "-L",
            str(SOURCE),
            "-l",
            str(REPO / "scripts/portal-driver.el"),
        ],
        stdin=slave,
        stdout=slave,
        stderr=slave,
        env=env,
        preexec_fn=terminal_session,
    )
    os.close(slave)
    terminal_output = bytearray()

    def drain():
        try:
            while True:
                chunk = os.read(master, 65536)
                if not chunk:
                    return
                terminal_output.extend(chunk)
        except OSError:
            return

    thread = threading.Thread(target=drain, daemon=True)
    thread.start()
    connection = Gio.bus_get_sync(Gio.BusType.SESSION, None)
    loop = GLib.MainLoop()
    failures = []
    results = []

    def array_path(path):
        return GLib.Variant("ay", os.fsencode(str(path)) + b"\0")

    base = {"current_folder": array_path(fixture)}
    multiple = dict(base, multiple=GLib.Variant("b", True))
    folder = dict(base, directory=GLib.Variant("b", True))
    filtered = dict(
        multiple,
        filters=GLib.Variant("a(sa(us))", [("PNG", [(0, "*.png")])]),
        current_filter=GLib.Variant("(sa(us))", ("PNG", [(0, "*.png")])),
    )
    cases = [
        ("nested", "OpenFile", multiple, [fixture / "child/nested.png"]),
        ("return", "OpenFile", multiple, [fixture / "plain.txt"]),
        ("confirm", "OpenFile", multiple, [fixture / "plain.txt"]),
        ("filtered-navigation", "OpenFile", filtered, [fixture / "child/nested.png"]),
        ("folder-invalid", "OpenFile", folder, [fixture / "child"]),
        ("folder-single", "OpenFile", folder, [fixture / "child"]),
        (
            "folder-multiple",
            "OpenFile",
            dict(folder, multiple=GLib.Variant("b", True)),
            [fixture / "child"],
        ),
        ("uri", "OpenFile", base, [fixture / "sp ace#?%ü.txt"]),
        (
            "savefiles",
            "SaveFiles",
            dict(base, files=GLib.Variant("aay", [b"first.txt\0", b"second.txt\0"])),
            [fixture / "child/first.txt", fixture / "child/second.txt"],
        ),
        ("cancel", "OpenFile", multiple, "cancel"),
        (
            "failure",
            "SaveFiles",
            dict(base, files=GLib.Variant("aay", [b"first.txt\0"])),
            "error",
        ),
    ]

    def run_cases(*_):
        try:
            for title, method, options, expected in cases:
                try:
                    reply = connection.call_sync(
                        "org.gnu.Emacs.FileChooser",
                        "/org/freedesktop/portal/desktop",
                        "org.freedesktop.impl.portal.FileChooser",
                        method,
                        GLib.Variant(
                            "(osssa{sv})",
                            ("/verification/request", "test.app", "", title, options),
                        ),
                        None,
                        Gio.DBusCallFlags.NONE,
                        7000,
                        None,
                    )
                    code, data = reply.unpack()
                    uris = data.get("uris", [])
                    paths = [Gio.File.new_for_uri(uri).get_path() for uri in uris]
                    if expected == "error":
                        ok = code == 2 and not uris
                    elif expected == "cancel":
                        ok = code == 1 and not uris
                    else:
                        ok = code == 0 and paths == [str(path) for path in expected]
                    result = dict(
                        case=title, pass_=ok, response=code, uris=uris, paths=paths
                    )
                except GLib.Error as error:
                    result = dict(case=title, pass_=False, error=str(error))
                results.append(result)
                print(
                    ("PASS" if result["pass_"] else "FAIL") + " " + json.dumps(result),
                    flush=True,
                )
                if not result["pass_"]:
                    failures.append(title)
                    break
            if failures:
                return False
            inspection = connection.call_sync(
                "org.gnu.Emacs.FileChooser",
                "/org/freedesktop/portal/desktop",
                "org.gnu.Emacs.FileChooser.Verification",
                "Inspect",
                None,
                None,
                Gio.DBusCallFlags.NONE,
                3000,
                None,
            ).unpack()[0]
            inspection = json.loads(inspection)
            print("INSPECT " + json.dumps(inspection), flush=True)
            if inspection["visited_files"] or not inspection["dired_kill_setting"]:
                failures.append("ordinary Dired state or file visits")
            if (
                Path(inspection["implementation"]).resolve()
                != SOURCE / "filechooser.el"
            ):
                failures.append("wrong implementation loaded")
        except Exception as error:
            failures.append(str(error))
            print("ERROR " + repr(error), flush=True)
        finally:
            loop.quit()
        return False

    def appeared(*_):
        GLib.idle_add(run_cases)

    watch = Gio.bus_watch_name_on_connection(
        connection,
        "org.gnu.Emacs.FileChooser",
        Gio.BusNameWatcherFlags.NONE,
        appeared,
        None,
    )

    def startup_timeout():
        if not results:
            failures.append("backend startup timed out")
            loop.quit()
        return False

    timeout = GLib.timeout_add_seconds(15, startup_timeout)
    try:
        loop.run()
    finally:
        Gio.bus_unwatch_name(watch)
        GLib.source_remove(timeout)
        process.terminate()
        try:
            process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()
        os.close(master)
        thread.join(timeout=2)
        log = Path(
            os.environ.get(
                "FILECHOOSER_TERMINAL_LOG",
                "/tmp/opencode/filechooser-portal-terminal.log",
            )
        )
        log.parent.mkdir(parents=True, exist_ok=True)
        log.write_bytes(terminal_output)
        shutil.rmtree(fixture)
    print(
        f"PORTAL {sum(r['pass_'] for r in results)}/{len(cases)} cases passed; failures={failures}"
    )
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
