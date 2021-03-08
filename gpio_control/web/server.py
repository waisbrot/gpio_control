from gpio_control.web.handler import HTTPRequestHandler
from threading import Thread
import http.server
import logging

log = logging.getLogger(__name__)

class Server(Thread):
    def __init__(self, port: int) -> None:
        super().__init__(group=None, target=None, name='web server', args=[], kwargs={}, daemon=True)
        self.port = port

    def run(self):
        server = http.server.ThreadingHTTPServer(('', self.port), HTTPRequestHandler)
        log.info(f'Listening on port {self.port}')
        server.serve_forever()

