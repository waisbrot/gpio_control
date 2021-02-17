%%%-------------------------------------------------------------------
%% @doc gpio_control top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(gpio_control_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 10,
        period => 60},
    ChildSpecifications = [
        #{
            id => gpio_control,
            start => {gpio_control, start_link, []},
            restart => permanent, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [gpio_control]
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.

%% internal functions
