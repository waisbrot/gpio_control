-module(health_handler).
-behaviour(cowboy_handler).
-export([init/2, content_types_provided/2]).
-export([health_to_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, health_to_json}
	], Req, State}.

health_to_json(Req, State) ->
	Body = jsone:encode(#{healthy => true}),
	{Body, Req, State}.
