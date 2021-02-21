defmodule GpioControlTest do
  use ExUnit.Case
  doctest GpioControl

  test "greets the world" do
    assert GpioControl.hello() == :world
  end
end
