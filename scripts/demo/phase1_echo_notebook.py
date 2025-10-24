#!/usr/bin/env python3
"""Exercise the Phase 1 kernel by sending a real execute_request via ZeroMQ.

The script mimics the client-side of the Jupyter protocol: it reads the
connection file, builds a signed execute_request message, sends it over the
shell socket, listens for the execute_reply, and prints the corresponding
IOPub stream output.
"""

import argparse
import hashlib
import hmac
import json
import pathlib
import sys
import time
import uuid
from typing import Dict, List

import zmq

FRAME_SEPARATORS = (",", ":")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Phase 1 execute echo harness")
    parser.add_argument(
        "--connection",
        required=True,
        help="Path to the Jupyter connection file emitted by the launcher",
    )
    parser.add_argument(
        "--code",
        default='print("ok")',
        help="Code snippet to execute",
    )
    parser.add_argument(
        "--identity",
        default="demo",
        help="Client identity used on the shell socket",
    )
    return parser.parse_args()


def load_connection(path: str) -> Dict[str, str]:
    data = json.loads(pathlib.Path(path).read_text())
    required = [
        "ip",
        "transport",
        "shell_port",
        "iopub_port",
        "hb_port",
        "control_port",
        "signature_scheme",
        "key",
    ]
    missing = [field for field in required if field not in data]
    if missing:
        raise ValueError(f"connection file missing fields: {missing}")
    return data


def canonical_json(value: Dict) -> bytes:
    return json.dumps(value, separators=FRAME_SEPARATORS, ensure_ascii=False).encode("utf-8")


def sign_frames(key: bytes, frames: List[bytes]) -> str:
    if not key:
        return ""
    mac = hmac.new(key, digestmod=hashlib.sha256)
    for frame in frames:
        mac.update(frame)
    return mac.hexdigest()


def build_execute_request(code: str, key: bytes) -> List[bytes]:
    msg_id = uuid.uuid4().hex
    header = {
        "msg_id": msg_id,
        "session": uuid.uuid4().hex,
        "username": "demo",
        "msg_type": "execute_request",
        "version": "5.3",
        "date": time.strftime("%Y-%m-%dT%H:%M:%S"),
    }
    parent = {}
    metadata = {}
    content = {
        "code": code,
        "silent": False,
        "store_history": True,
        "allow_stdin": False,
    }
    encoded = [
        canonical_json(header),
        canonical_json(parent),
        canonical_json(metadata),
        canonical_json(content),
    ]
    signature = sign_frames(key, encoded)
    return [b"", signature.encode("utf-8"), *encoded]


def main() -> None:
    args = parse_args()
    try:
        connection = load_connection(args.connection)
    except Exception as exc:  # pragma: no cover
        print(f"[demo] failed to read connection file: {exc}", file=sys.stderr)
        sys.exit(2)

    shell_endpoint = f"{connection['transport']}://{connection['ip']}:{connection['shell_port']}"
    iopub_endpoint = f"{connection['transport']}://{connection['ip']}:{connection['iopub_port']}"
    key = connection.get("key", "").encode("utf-8")

    ctx = zmq.Context.instance()
    shell = ctx.socket(zmq.DEALER)
    iopub = ctx.socket(zmq.SUB)
    heartbeat = ctx.socket(zmq.REQ)
    try:
        shell.setsockopt(zmq.IDENTITY, args.identity.encode("utf-8"))
        shell.connect(shell_endpoint)

        iopub.setsockopt(zmq.SUBSCRIBE, b"")
        iopub.connect(iopub_endpoint)

        hb_endpoint = f"{connection['transport']}://{connection['ip']}:{connection['hb_port']}"
        heartbeat.connect(hb_endpoint)

        # Allow sockets to settle
        time.sleep(0.2)

        frames = build_execute_request(args.code, key)
        shell.send_multipart(frames)
        print("[demo] execute_request sent. Waiting for reply...")

        reply = shell.recv_multipart()
        stream = iopub.recv_multipart()

        print_message("execute_reply", reply)
        print_message("iopub", stream)

        heartbeat.send(b"ping")
        pong = heartbeat.recv()
        print(f"[demo] heartbeat response: {pong!r}")
    finally:
        shell.close(0)
        iopub.close(0)
        heartbeat.close(0)
        ctx.term()


def print_message(label: str, frames: List[bytes]) -> None:
    try:
        idx = frames.index(b"")
    except ValueError:
        print(f"[demo] malformed {label} frames: {frames}")
        return
    signature = frames[idx + 1].decode("utf-8")
    header = json.loads(frames[idx + 2].decode("utf-8"))
    content = json.loads(frames[idx + 5].decode("utf-8"))
    print(f"[demo] {label} signature={signature}")
    print(f"         header.msg_type={header.get('msg_type')}")
    print(f"         content={content}")


if __name__ == "__main__":
    main()
