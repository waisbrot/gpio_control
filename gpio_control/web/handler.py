from http.server import BaseHTTPRequestHandler
import jinja2
import logging
from gpio_control.control import power, virtual_switch

log = logging.getLogger(__name__)

class HTTPRequestHandler (BaseHTTPRequestHandler):
    tenv = jinja2.Environment(loader=jinja2.PackageLoader('gpio_control'), autoescape=jinja2.utils.select_autoescape(["html"]))

    def do_HEAD(self):
        self.send_response(200)
        self.end_headers()
        self.close_connection = True

    def do_GET(self):
        template = self.tenv.get_template("index.html")
        message = template.render(power=power)
        message = bytes(message, 'utf8')
        self.send_response(200)
        self.send_header("Content-Length", len(message))
        self.send_header("Content-Type", "text/html; charset=UTF-8")
        self.end_headers()
        self.wfile.write(message)
        self.close_connection = True

    def do_POST(self):
        virtual_switch.toggle()
        self.send_response(302)
        self.send_header("Content-Lenghth", 0)
        self.send_header("Location", "/")
        self.end_headers()
        self.close_connection = True
