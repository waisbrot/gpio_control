from gpiozero.tools import _normalize, inverted, any_values
from gpiozero import LED, Button
from gpiozero.output_devices import DigitalOutputDevice
from threading import Thread, Lock
from signal import pause
from time import time
import logging

log = logging.getLogger(__name__)

class PowerSupply(DigitalOutputDevice):
    pass

class SoftSwitch:
    def __init__(self, start=False) -> None:
        self.value = start
        self.lock = Lock()

    def __iter__(self):
        return self

    def __next__(self) -> bool:
        with self.lock:
            return self.value

    def toggle(self) -> None:
        with self.lock:
            self.value = not self.value

    def set(self, value: bool) -> None:
        with self.lock:
            self.value = value

button = Button(15)
virtual_switch = SoftSwitch()
led = LED(14)
power = PowerSupply(18)

def toggled(values):
    values = _normalize(values)
    previous = 0
    value = 0
    for v in values:
        if v != previous:
            if v == 0:  # press and release
                value ^= 1
            previous = v
        yield value

def post_debounced(values, delay=0.1):
    '''Propagate a change, but they ignore further changes for a little while'''
    ignore_until = 0
    previous = next(values)
    for v in values:
        if v != previous and time() > ignore_until:
            yield v
            previous = v
            ignore_until = time() + delay
        else:
            yield previous

def either_changed(values1, values2):
    prev1 = None
    prev2 = None
    value = False
    for (v1,v2) in zip(values1, values2):
        if v1 != prev1:
            prev1 = v1
            value = v1
        if v2 != prev2:
            prev2 = v2
            value = v2
        yield value

class Control(Thread):
    def __init__(self) -> None:
        super().__init__(group=None, name='device control', daemon=False)

    def run(self) -> None:
        button_delay = time()
        def button_released():
            log.debug(f'button released')
            if time() > button_delay:
                log.debug(f'Button toggled')
                power.toggle()
                button_delay = time() + 3
            else:
                log.debug(f'ignoring button because timeout is not past')

        meta_button = toggled(button)
        debounced_button = post_debounced(meta_button, delay=3)
        power.source = either_changed(debounced_button, virtual_switch)
        led.source = inverted(power)
        pause()
