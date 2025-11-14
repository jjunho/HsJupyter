#!/usr/bin/env python3
"""Simple test client for HsJupyter kernel"""
import json
import hmac
import hashlib
import zmq
import uuid
import time

def test_kernel():
    # Connection info
    connection = {
        "transport": "tcp",
        "ip": "127.0.0.1", 
        "shell_port": 5555,
        "iopub_port": 5556,
        "stdin_port": 5557,
        "control_port": 5558,
        "hb_port": 5559,
        "signature_scheme": "hmac-sha256",
        "key": "secret"
    }
    
    context = zmq.Context()
    
    # Create shell socket
    shell = context.socket(zmq.DEALER)
    shell.connect(f"tcp://{connection['ip']}:{connection['shell_port']}")
    
    # Create IOPub socket
    iopub = context.socket(zmq.SUB)
    iopub.setsockopt(zmq.SUBSCRIBE, b"")
    iopub.connect(f"tcp://{connection['ip']}:{connection['iopub_port']}")
    
    # Test Haskell expressions
    test_cases = [
        "2 + 3",                    # Simple arithmetic
        "length [1,2,3,4,5]",      # List operations
        "map (*2) [1,2,3]",        # Higher-order functions
        "let x = 42 in x * 2"      # Let bindings
    ]
    
    print("🧪 Testing HsJupyter Kernel with Phase 7 Features")
    print("=" * 50)
    
    for i, code in enumerate(test_cases, 1):
        print(f"\n📝 Test {i}: Evaluating `{code}`")
        
        # Send execute request
        msg_id = str(uuid.uuid4())
        
        header = {
            "msg_id": msg_id,
            "msg_type": "execute_request",
            "username": "test",
            "session": "test-session",
            "date": time.strftime("%Y-%m-%dT%H:%M:%S.%f")[:-3] + "Z",
            "version": "5.3"
        }

        content = {
            "code": code,
            "silent": False,
            "store_history": True,
            "user_expressions": {},
            "allow_stdin": False
        }
        
        header_bytes = json.dumps(header, separators=(",", ":"), ensure_ascii=False).encode("utf-8")
        parent_bytes = json.dumps({}, separators=(",", ":"), ensure_ascii=False).encode("utf-8")
        metadata_bytes = json.dumps({}, separators=(",", ":"), ensure_ascii=False).encode("utf-8")
        content_bytes = json.dumps(content, separators=(",", ":"), ensure_ascii=False).encode("utf-8")

        key_bytes = connection["key"].encode("utf-8")
        signature = hmac.new(
            key_bytes,
            header_bytes + parent_bytes + metadata_bytes + content_bytes,
            hashlib.sha256,
        ).hexdigest() if key_bytes else ""

        # Send message
        frames = [
            b"<IDS|MSG>",
            signature.encode("utf-8"),
            header_bytes,
            parent_bytes,
            metadata_bytes,
            content_bytes,
        ]
        
        shell.send_multipart(frames)
        print(f"   ✅ Request sent")
        
        # Try to receive reply (with timeout)
        poller = zmq.Poller()
        poller.register(shell, zmq.POLLIN)
        
        if poller.poll(5000):  # 5 second timeout
            reply = shell.recv_multipart()
            print(f"   📦 Reply received: {len(reply)} frames")
            
            # Parse reply content
            if len(reply) >= 6:
                reply_content = json.loads(reply[5].decode('utf-8'))
                status = reply_content.get('status', 'unknown')
                print(f"   📊 Status: {status}")
                
                if status == 'ok':
                    print(f"   🎉 Execution successful!")
                elif status == 'error':
                    print(f"   ❌ Error: {reply_content.get('ename', 'Unknown')}")
        else:
            print(f"   ⏰ Timeout waiting for reply")
    
    print(f"\n🏁 Testing complete!")
    shell.close()
    iopub.close()
    context.term()

if __name__ == "__main__":
    # test_kernel()
    pass
