#!/usr/bin/env python3
"""Send eval requests to a Swank server and print results.

Usage:
  swank-eval.py "(+ 1 2)"          # eval and print result
  swank-eval.py -s spacelang       # switch package to SPACELANG
  echo '(+ 1 2)' | swank-eval.py   # read from stdin
"""
import sys
import socket
import re

HOST = "127.0.0.1"
PORT = 4005
PACKAGE = "COMMON-LISP-USER"

def read_packet(sock):
    """Read a single length-prefixed Swank packet. Returns str or None."""
    try:
        length_hex = b""
        while len(length_hex) < 6:
            chunk = sock.recv(6 - len(length_hex))
            if not chunk:
                return None
            length_hex += chunk
        length = int(length_hex.decode("utf-8"), 16)
        body = b""
        while len(body) < length:
            chunk = sock.recv(length - len(body))
            if not chunk:
                return None
            body += chunk
        return body.decode("utf-8")
    except (ValueError, socket.timeout):
        return None

def swank_eval(code, package="COMMON-LISP-USER"):
    """Send swank:eval-and-grab-output. Returns (status, output, result)."""
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(30)
    try:
        sock.connect((HOST, PORT))

        # Escape code for embedding in a string literal
        escaped = code.replace('\\', '\\\\').replace('"', '\\"')

        # Build emacs-rex: (:emacs-rex FORM PACKAGE THREAD ID)
        msg = f'(:emacs-rex (swank:eval-and-grab-output "{escaped}") "{package}" t 42)'
        payload = msg.encode("utf-8")
        frame = f"{len(payload):06x}".encode("utf-8") + payload
        sock.sendall(frame)

        # Read response — may get async messages (:write-string etc) first
        while True:
            resp = read_packet(sock)
            if resp is None:
                return ("error", "", "connection closed before response")

            if resp.startswith("(:return"):
                # Parse (:return (:ok ("output" "result")) ID)
                m = re.search(r'\(:return\s+(\(:ok\s+\("(.*?)"\s*"((?:[^"\\]|\\.)*)"\)\))\s+\d+\)', resp)
                if m:
                    output = m.group(2)
                    result = m.group(3)
                    # Unescape the result string
                    result = result.replace('\\"', '"').replace('\\\\', '\\')
                    return ("ok", output, result)
                # Also try with numbers (some results may be printed as atoms)
                m2 = re.search(r'\(:return\s+(\(:ok\s+\("(.*?)"\s*(\d+)\)\))\s+\d+\)', resp)
                if m2:
                    return ("ok", m2.group(2), m2.group(3))
                return ("ok", "", resp)
            elif resp.startswith("(:debug"):
                return ("debug", "", resp)
            # Otherwise async message, continue reading
    finally:
        sock.close()

if __name__ == "__main__":
    args = sys.argv[1:]
    package = PACKAGE

    # Parse -p/--package flag
    i = 0
    code_parts = []
    while i < len(args):
        if args[i] in ("-p", "--package"):
            package = args[i + 1].upper()
            i += 2
        else:
            code_parts.append(args[i])
            i += 1

    code = " ".join(code_parts) if code_parts else sys.stdin.read().strip()

    if not code:
        print("Usage: swank-eval.py [-p PACKAGE] <code>", file=sys.stderr)
        sys.exit(1)

    status, output, result = swank_eval(code, package)
    if status == "ok":
        if output:
            print(output, end="")
        print(result)
    elif status == "debug":
        print(f"DEBUGGER: {result}", file=sys.stderr)
        sys.exit(1)
    else:
        print(f"ERROR: {result}", file=sys.stderr)
        sys.exit(1)
