"""
Zoa Jupyter Kernel

A Jupyter kernel that connects to the Zoa optical design engine
via ZeroMQ. Each cell is sent as a command string; the engine's
text output is returned and displayed. Plot commands (e.g. FIE; GO)
produce inline PNG images.

The kernel manages a zoa_server subprocess automatically.
"""

import base64
import os
import signal
import subprocess
import sys
import time

import zmq
from ipykernel.kernelbase import Kernel


class ZoaKernel(Kernel):
    implementation = 'zoa'
    implementation_version = '0.1'
    language = 'zoa'
    language_version = '1.0'
    language_info = {
        'name': 'zoa',
        'mimetype': 'text/plain',
        'file_extension': '.zoa',
    }
    banner = 'Zoa Optical Design Engine'

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._server_proc = None
        self._zmq_ctx = None
        self._zmq_sock = None
        self._port = None
        self._start_server()

    def _find_server_executable(self):
        """Locate the zoa_server executable."""
        # Check ZOA_SERVER env var first
        env_path = os.environ.get('ZOA_SERVER')
        if env_path and os.path.isfile(env_path):
            return env_path

        # Check common build directories relative to this file
        kernel_dir = os.path.dirname(os.path.abspath(__file__))
        project_root = os.path.dirname(os.path.dirname(kernel_dir))

        candidates = [
            os.path.join(project_root, 'buildHB', 'zoa_server'),
            os.path.join(project_root, 'build', 'zoa_server'),
            os.path.join(project_root, 'builddir', 'zoa_server'),
        ]
        for path in candidates:
            if os.path.isfile(path):
                return path

        raise FileNotFoundError(
            'Cannot find zoa_server executable. '
            'Set ZOA_SERVER environment variable or build with meson.'
        )

    def _find_free_port(self):
        """Find a free TCP port."""
        import socket
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.bind(('', 0))
            return s.getsockname()[1]

    def _start_server(self):
        """Start the zoa_server subprocess and connect via ZMQ."""
        server_path = self._find_server_executable()
        self._port = self._find_free_port()
        endpoint = f'tcp://*:{self._port}'

        self._server_proc = subprocess.Popen(
            [server_path, endpoint],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
        )

        # Connect ZMQ client
        self._zmq_ctx = zmq.Context()
        self._zmq_sock = self._zmq_ctx.socket(zmq.REQ)
        self._zmq_sock.setsockopt(zmq.RCVTIMEO, 30000)  # 30s timeout
        self._zmq_sock.connect(f'tcp://localhost:{self._port}')

        # Wait for server to be ready
        for attempt in range(20):
            time.sleep(0.25)
            try:
                self._zmq_sock.send_string('__PING__')
                reply = self._zmq_sock.recv_string()
                if reply == 'PONG':
                    return
            except zmq.Again:
                continue

        raise RuntimeError('zoa_server did not respond to PING within 5 seconds')

    def _send_command(self, cmd):
        """Send a command to the server and return the reply."""
        self._zmq_sock.send_string(cmd)
        return self._zmq_sock.recv_string()

    def do_execute(self, code, silent, store_history=True,
                   user_expressions=None, allow_stdin=False):
        code = code.strip()
        if not code:
            return {
                'status': 'ok',
                'execution_count': self.execution_count,
                'payload': [],
                'user_expressions': {},
            }

        try:
            reply = self._send_command(code)
        except zmq.Again:
            reply = 'ERROR: Server timed out (30s). Command may be too slow or server crashed.'
        except Exception as e:
            reply = f'ERROR: {e}'

        if not silent and reply:
            lines = reply.split('\n')
            text_lines = []
            for line in lines:
                if line.startswith('__IMAGE__'):
                    png_path = line[len('__IMAGE__'):]
                    try:
                        with open(png_path, 'rb') as f:
                            png_data = f.read()
                        self.send_response(
                            self.iopub_socket,
                            'display_data',
                            {
                                'data': {
                                    'image/png': base64.b64encode(png_data).decode(),
                                },
                                'metadata': {},
                            },
                        )
                    except FileNotFoundError:
                        text_lines.append(f'[Plot file not found: {png_path}]')
                else:
                    text_lines.append(line)
            if text_lines:
                text = '\n'.join(text_lines)
                if text.strip():
                    self.send_response(
                        self.iopub_socket,
                        'stream',
                        {'name': 'stdout', 'text': text + '\n'},
                    )

        return {
            'status': 'ok',
            'execution_count': self.execution_count,
            'payload': [],
            'user_expressions': {},
        }

    def do_shutdown(self, restart):
        """Shut down the server subprocess."""
        if self._zmq_sock is not None:
            try:
                self._zmq_sock.setsockopt(zmq.RCVTIMEO, 2000)
                self._zmq_sock.send_string('__QUIT__')
                self._zmq_sock.recv_string()
            except Exception:
                pass
            self._zmq_sock.close()
            self._zmq_sock = None

        if self._zmq_ctx is not None:
            self._zmq_ctx.term()
            self._zmq_ctx = None

        if self._server_proc is not None:
            self._server_proc.terminate()
            try:
                self._server_proc.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self._server_proc.kill()
            self._server_proc = None

        return {'status': 'ok', 'restart': restart}
