from gpiozero.tools import _normalize, inverted, any_values
from gpiozero import LED, Button
from gpiozero.output_devices import DigitalOutputDevice
from threading import Thread, Lock
from signal import pause
from time import time

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

class Control(Thread):
    def __init__(self) -> None:
        super().__init__(group=None, name='device control', daemon=False)

    def _button_value(self):
        meta_button = any_values(toggled(button), virtual_switch)
        debounced = post_debounced(meta_button, delay=3)
        return debounced

    def run(self) -> None:
        button_delay = time()
        def button_released():
            if time() > button_delay:
                power.toggle()
                button_delay = time() + 3

        led.source = inverted(power)
        power.when_released = button_released
        pause()
