defmodule GpioControlWeb.PageController do
  use GpioControlWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
