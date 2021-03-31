from threading import Thread
from time import sleep
import os
import board
import adafruit_si7021
from socket import gethostname
from influxdb_client import InfluxDBClient
from influxdb_client.client.write_api import SYNCHRONOUS

class Temperature(Thread):
    def __init__(self) -> None:
        super().__init__(group=None, name='temperature measure', daemon=False)
        self._bucket = os.getenv('INFLUX_BUCKET')
        org = os.getenv('INFLUX_ORG')
        token = os.getenv('INFLUX_TOKEN')
        self._hostname = gethostname()
        client = InfluxDBClient(url="http://tularemia.home.waisbrot.net:8086", token=token, org=org)
        self.write_influx = client.write_api(write_options=SYNCHRONOUS)

    def run(self) -> None:
        sensor = adafruit_si7021.SI7021(board.I2C())
        while True:
            metrics = [
                f'temperature,host={self._hostname} degrees_c={sensor.temperature}',
                f'humidity,host={self._hostname} humidity_pct={sensor.relative_humidity}',
            ]
            write_influx.write(bucket=self._bucket, record=metrics)
            sleep(10)
