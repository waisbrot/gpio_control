from gpiozero.tools import _normalize, inverted, any_values
from gpiozero import LED, Button
from gpiozero.output_devices import DigitalOutputDevice
from threading import Thread, Lock
from signal import SIG_IGN, pause
from time import CLOCK_THREAD_CPUTIME_ID, time
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

def debounced_toggled(values, delay):
    values = _normalize(values)
    previous = 0
    value = 0
    ignore_until = 0
    for v in values:
        if v != previous:
            if v == 0:  # press and release
                if time() > ignore_until:
                    value ^= 1
                    log.debug(f"debounced_toggled: ignore time is past; toggling to {value}")
                    ignore_until = time() + delay
                else:
                    log.debug(f"debounced_toggled: this would be a toggle, but ignore time is not past. stay at {value}")
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
        diff = False
        if v1 != prev1:
            prev1 = v1
            log.debug(f'either_changed: v1 changed: {value} -> {v1}')
            value = v1
            diff = True
        if v2 != prev2:
            prev2 = v2
            log.debug(f'either_changed: v2 changed: {value} -> {v2}')
            value = v2
            diff = True
        if diff:
            log.debug(f'either_changed: output {value}')
        yield value

class Control(Thread):
    def __init__(self) -> None:
        super().__init__(group=None, name='device control', daemon=False)

    def run(self) -> None:
        debounced_button = debounced_toggled(button, delay=3)
        power.source = either_changed(debounced_button, virtual_switch)
        led.source = inverted(power)
        pause()
