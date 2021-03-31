from threading import Thread
from time import sleep
import os
import board
import adafruit_si7021
from socket import gethostname
from influxdb_client.client import write_api as write_influx

class Temperature(Thread):
    def __init__(self) -> None:
        super().__init__(group=None, name='temperature measure', daemon=False)
        self._bucket = os.getenv('INFLUX_BUCKET')
        self._org = os.getenv('INFLUX_ORG')
        self._token = os.getenv('INFLUX_TOKEN')
        self._hostname = gethostname()

    def run(self) -> None:
        sensor = adafruit_si7021.SI7021(board.I2C())
        while True:
            metrics = [
                f'temperature,host={self._hostname} degrees_c={sensor.temperature}',
                f'humidity,host={self._hostname} humidity_pct={sensor.relative_humidity}',
            ]
            write_influx.write(self._bucket, self._org, metrics)
            sleep(10)
