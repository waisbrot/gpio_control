from gpiozero.tools import _normalize, inverted, any_values, post_delayed
from gpiozero import LED, Button
from gpiozero.output_devices import DigitalOutputDevice
from gpiozero.mixins import ValuesMixin
from threading import Thread
from signal import pause

class PowerSupply(DigitalOutputDevice):
    pass

class SoftButton(ValuesMixin):
    def __init__(self, base=False) -> None:
        self.base = base
        self.value = base

    @property
    def values(self):
        while True:
            yield self.value
            if self.value != self.base:
                self.value = self.base

    def click(self):
        self.value = not self.base


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
        meta_button = any_values(toggled(button), virtual_button)
        debounced = post_delayed(meta_button, 3)
        led.source = inverted(debounced)
        power.source = debounced
        pause()
