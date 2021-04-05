import logging
from threading import Thread
from time import sleep
import os
import board
import adafruit_si7021
from socket import gethostname
from influxdb_client import InfluxDBClient
from influxdb_client.client.write_api import SYNCHRONOUS

log = logging.getLogger('main')

class Temperature(Thread):
    def __init__(self) -> None:
        super().__init__(group=None, name='temperature measure', daemon=False)
        self._bucket = os.getenv('INFLUX_BUCKET')
        self._org = os.getenv('INFLUX_ORG')
        self._token = os.getenv('INFLUX_TOKEN')
        self._url = os.getenv("INFLUX_URL")
        self._hostname = gethostname()

    def run(self) -> None:
        sensor = None
        influx = None
        while True:
            if not sensor:
                log.debug("Initializing temp sensor")
                sensor = adafruit_si7021.SI7021(board.I2C())
            if not influx:
                log.debug("Initializing Influx client")
                client = InfluxDBClient(url=self._url, token=self._token, org=self._org)
                influx = client.write_api(write_options=SYNCHRONOUS)

            metrics = [
                f'temperature,host={self._hostname} degrees_c={sensor.temperature}',
                f'humidity,host={self._hostname} humidity_pct={sensor.relative_humidity}',
            ]
            log.debug(f"Going to write to influx: {metrics}")
            try:
                influx.write(bucket=self._bucket, record=metrics)
            except:
                log.error("Failed to write to Influx. Will try to reconnect next time.")
                influx = None
            sleep(10)
