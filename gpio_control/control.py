from gpiozero.tools import _normalize, inverted, any_values, post_delayed
from gpiozero import LED, Button
from gpiozero.output_devices import DigitalOutputDevice
from gpiozero.mixins import ValuesMixin
from threading import Thread, Lock
from signal import pause
from time import time

class LockingIter:
    def __init__(self, it):
        self.it = it
        self.lock = Lock()

    def __iter__(self):
        return self

    def __next__(self):
        with self.lock:
            return next(self.it)
def generator_with_locking(f):
    return lambda *args, **kwargs: LockingIter(f(*args, **kwargs))

class PowerSupply(DigitalOutputDevice):
    pass

class SoftButton:
    def __init__(self, base=False) -> None:
        self.base = base
        self.value = base
        self.pushtimeout = 0
        self.lock = Lock()

    @property
    @generator_with_locking
    def values(self):
        while True:
            yield self.value
            if self.value != self.base:
                if time.time() > self.pushtime:
                    self.value = self.base

    def click(self, pushtime=0.01):
        self.value = not self.base
        self.pushtime = pushtime + time.time()

@generator_with_locking
def threadsafe(values):
    for v in values:
        yield v


button = Button(15)
virtual_button = SoftButton()
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

class Control(Thread):
    def __init__(self) -> None:
        super().__init__(group=None, name='device control', daemon=False)

    def run(self) -> None:
        meta_button = any_values(toggled(button), virtual_button.values)
        debounced = post_delayed(meta_button, 3)
        shared = threadsafe(debounced)
        led.source = inverted(shared)
        power.source = shared
        pause()
