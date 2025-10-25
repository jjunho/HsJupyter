#!/usr/bin/env python3
"""Simple test client for HsJupyter kernel"""
import json
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
        
        # Simplified message (no signatures for demo)
        header = {
            "msg_id": msg_id,
            "msg_type": "execute_request",
            "username": "test",
            "session": "test-session",
            "date": time.strftime("%Y-%m-%dT%H:%M:%S.%fZ"),
            "version": "5.3"
        }
        
        content = {
            "code": code,
            "silent": False,
            "store_history": True,
            "user_expressions": {},
            "allow_stdin": False
        }
        
        # Send message
        frames = [
            b"",  # delimiter
            b"",  # signature (empty for demo)
            json.dumps(header).encode('utf-8'),
            json.dumps({}).encode('utf-8'),   # parent_header
            json.dumps({}).encode('utf-8'),   # metadata
            json.dumps(content).encode('utf-8')
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
    test_kernel()