#!/usr/bin/env python3
"""
Test script to send kernel_info_request to the HsJupyter kernel.
"""
import json
import sys
import uuid
import hmac
import hashlib
import zmq
from datetime import datetime

def create_msg(msg_type, content, session_id, key):
    """Create a Jupyter protocol message."""
    header = {
        "msg_id": str(uuid.uuid4()),
        "session": session_id,
        "username": "test",
        "date": datetime.utcnow().isoformat() + "Z",
        "msg_type": msg_type,
        "version": "5.3"
    }
    
    parent_header = {}
    metadata = {}
    
    # Compute signature
    msg_list = [
        json.dumps(header).encode('utf-8'),
        json.dumps(parent_header).encode('utf-8'),
        json.dumps(metadata).encode('utf-8'),
        json.dumps(content).encode('utf-8'),
    ]
    
    if key:
        signature = hmac.new(key.encode('utf-8'), 
                           b"".join(msg_list),
                           hashlib.sha256).hexdigest()
    else:
        signature = ""
    
    return signature, header, parent_header, metadata, content

def send_kernel_info_request(conn_file):
    """Send kernel_info_request and wait for reply."""
    with open(conn_file) as f:
        cfg = json.load(f)
    
    ctx = zmq.Context()
    shell = ctx.socket(zmq.DEALER)  # Use DEALER to connect to Router
    shell.identity = b"test-client"
    
    url = f"{cfg['transport']}://{cfg['ip']}:{cfg['shell_port']}"
    print(f"Connecting to shell socket: {url}")
    shell.connect(url)
    
    session_id = str(uuid.uuid4())
    key = cfg.get('key', '')
    
    # Create kernel_info_request
    signature, header, parent_header, metadata, content = create_msg(
        "kernel_info_request", {}, session_id, key
    )
    
    # Send message (DEALER format: no identity frames needed)
    # Delimiter must be EMPTY frame, not <IDS|MSG>
    delimiter = b""
    msg_frames = [
        delimiter,
        signature.encode('utf-8'),
        json.dumps(header).encode('utf-8'),
        json.dumps(parent_header).encode('utf-8'),
        json.dumps(metadata).encode('utf-8'),
        json.dumps(content).encode('utf-8'),
    ]
    
    print(f"Sending kernel_info_request (session={session_id})")
    for i, frame in enumerate(msg_frames):
        print(f"  Frame {i}: {frame[:80]}")
    
    shell.send_multipart(msg_frames)
    
    # Wait for reply
    print("Waiting for reply (timeout 5s)...")
    if shell.poll(5000):
        reply_frames = shell.recv_multipart()
        print(f"Received {len(reply_frames)} frames:")
        for i, frame in enumerate(reply_frames):
            if i == 0:
                print(f"  Frame {i} (delimiter): {frame}")
            elif i == 1:
                print(f"  Frame {i} (signature): {frame.decode('utf-8')}")
            else:
                try:
                    data = json.loads(frame.decode('utf-8'))
                    print(f"  Frame {i}: {json.dumps(data, indent=2)}")
                except:
                    print(f"  Frame {i} (raw): {frame[:200]}")
        
        # Parse reply
        if len(reply_frames) >= 6:
            reply_header = json.loads(reply_frames[2].decode('utf-8'))
            reply_content = json.loads(reply_frames[5].decode('utf-8'))
            print(f"\n✓ Got {reply_header['msg_type']} reply!")
            print(f"  Protocol version: {reply_content.get('protocol_version')}")
            print(f"  Implementation: {reply_content.get('implementation')}")
            print(f"  Language: {reply_content.get('language_info', {}).get('name')}")
            return True
    else:
        print("✗ Timeout - no reply received")
        return False

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <connection_file>")
        sys.exit(1)
    
    success = send_kernel_info_request(sys.argv[1])
    sys.exit(0 if success else 1)
