import argparse
from gpio_control.control import Control
#from gpio_control.pins.thread import PinThread
from gpio_control.web.server import Server
from threading import Thread
from time import sleep
import logging
from signal import pause

log = logging.getLogger('main')

def parse_args():
    parser = argparse.ArgumentParser('GPIO controller')
    parser.add_argument('--port', type=int, help='port to listen on', default=8080)
    parser.add_argument('-v', action='count', default=0, help='logging verbosity')
    return parser.parse_args()

def configure_logging(verbosity):
    levels = {
        0: logging.ERROR,
        1: logging.WARNING,
        2: logging.INFO,
        3: logging.DEBUG
    }
    if verbosity > 3:
        verbosity = 3
    level = levels[verbosity]
    logging.basicConfig(level=level)
    log.info(f'Logging configured at level {level}')

def run():
    args = parse_args()
    configure_logging(args.v)

    web = Server(args.port)
    web.start()

    control = Control()
    control.start()

#print("started")

#pins.join()
#print("pins join")
#web.join()
#print("web join")
