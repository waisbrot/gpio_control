%%%-------------------------------------------------------------------
%% @doc gpio_control public API
%% @end
%%%-------------------------------------------------------------------

-module(gpio_control_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/health", health_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(http,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    gpio_control_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
