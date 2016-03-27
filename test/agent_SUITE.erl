-module(agent_SUITE).

-export([all/0]).
-export([
    start_link/1,
    start/1
]).

all() -> [
    start_link,
    start
].

start_link(_Config) ->
    ct:comment("State is initialized properly"),
    {ok, Agent} = agent:start_link(fun() -> 123 end),
    123 == agent:get(Agent).

start(_Config) ->
    ct:comment("State is initialized properly"),
    {ok, Agent} = agent:start(fun() -> 123 end),
    123 == agent:get(Agent).

