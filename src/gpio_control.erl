-module(gpio_control).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {mode, target, pins}).
-define(LAST_PIN, 40).          % Pins are numbered starting with 1
-define(CACHE_LIFE, 30000).     % Time to trust our internal cache
-define(UPDATE_DELAY, 1000).
-include_lib("kernel/include/logger.hrl").

-export([get_all/0, get/1, set/2]).

%% API
get_all() ->
    {ok, List} = gen_server:call(?MODULE, {get, all}, 1000),
    List.
get(Pin) when Pin > 0 andalso Pin =< ?LAST_PIN ->
    {ok, Value} = gen_server:call(?MODULE, {get, Pin}, 1000),
    Value.
set(Pin, Value) when (Pin > 0 andalso Pin =< ?LAST_PIN) andalso (Value == true orelse Value == false) ->
    gen_server:cast(?MODULE, {set, Pin, Value}),
    ok.

%% gen_server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{mode=read, target=1, pins=array:new(?LAST_PIN, [{default, false}, fixed])}, 0}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({get, all}, _From, State) ->
    {reply, {ok, array:to_list(State#state.pins)}, State};
handle_call({get, Pin}, _From, State) when Pin > 0 andalso Pin =< ?LAST_PIN ->
    {reply, {ok, array:get(Pin - 1, State#state.pins)}, State};
handle_call({get, _}, _From, State) ->
    {reply, {error, invalid_pin}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, invalid_request}, State}.

handle_cast({set, Pin, Value}, State) when Pin > 0 andalso Pin =< ?LAST_PIN andalso (Value == true orelse Value == false) ->
    write_pin(Pin, Value),
    NewPins = array:set(Pin - 1, Value, State#state.pins),
    {noreply, State#state{pins=NewPins}};
handle_cast(Msg, State) ->
    logger:warning("Ingoring unknown cast ~p", [Msg]),
    {noreply, State}.

handle_info(timeout, State = #state{target=false}) ->
    % When we're not refreshing and we hit a timeout, it's time to refresh
    {noreply, State#state{target = 1}, 0};
handle_info(timeout, State = #state{target = Pin}) when Pin > ?LAST_PIN ->
    % When we've processed the last pin we can stop
    {noreply, State#state{mode = read, target = false}, ?CACHE_LIFE};
handle_info(timeout, State = #state{mode = read, target = Pin, pins = Pins}) ->
    % Update the cache with this pin's current value
    PinValue = read_pin(Pin),
    Pins1 = array:set(Pin-1, PinValue, Pins),
    {noreply, State#state{target = Pin+1, pins = Pins1}, ?UPDATE_DELAY};
handle_info(timeout, State = #state{mode = write, target = Pin, pins = Pins}) ->
    % Write the value in our cache out to the pin, so that our view of the world is correct
    PinValue = array:get(Pin-1, Pins),
    write_pin(Pin, PinValue),
    {noreply, State#state{target = Pin+1}, ?UPDATE_DELAY};
handle_info(Msg, State) ->
    logger:warning("Ignoring unknown message to info ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal
read_pin(Pin) ->
    logger:notice("Reading pin ~b", [Pin]),
    ReadCommand = io_lib:format(
        "python -c '"
        "import RPi.GPIO as g;"
        "g.setwarnings(False);"
        "g.setmode(g.BCM);"
        "g.setup(~B,g.IN);"
        "print(g.input(~B));"
        "g.cleanup(~B)'",
        [Pin, Pin, Pin]),
    Result = os:cmd(ReadCommand),
    Stripped = string:trim(Result),
    logger:notice("Read ~b = ~p", [Pin, Stripped]),
    {Int, []} = string:to_integer(Stripped),
    Int.

write_pin(Pin, Value) ->
    logger:notice("Writing pin ~b = ~b", [Pin, Value]),
    io_lib:format(
        "python -c '"
        "import RPi.GPIO as g;"
        "g.setwarnings(False);"
        "g.setmode(g.BCM);"
        "g.setup(~B,g.OUT);"
        "g.output(~B, ~B));"
        "g.cleanup(~B)'",
        [Pin, Pin, Value, Pin]),
    logger:notice("Wrote ~b = ~b", [Pin, Value]),
    ok.
