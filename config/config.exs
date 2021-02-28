# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

# Configures the endpoint
config :gpio_control, GpioControlWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "cX4hH5Jj01jtcbL3CwGz+ZfG8KYV3dF4j1scLYqnhfT5p7YowwDLJypDia0EUb4C",
  render_errors: [view: GpioControlWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: GpioControl.PubSub,
  live_view: [signing_salt: "54N6iRVk"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
