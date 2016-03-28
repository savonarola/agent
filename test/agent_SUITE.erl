-module(agent_SUITE).

-export([all/0]).
-export([
    start_link/1,
    start/1,
    get/1,
    get_and_update/1,
    update/1,
    update_async/1,
    stop/1
]).

all() -> [
    start_link,
    start,
    get,
    get_and_update,
    update,
    update_async,
    stop
].

expect_timeout(Fun) ->
    try
        Fun(),
        ok
    catch exit:{timeout, _Reason} ->
        timeout
    end.


start_link(_Config) ->
    ct:comment("State is initialized properly"),
    {ok, Agent} = agent:start_link(fun() -> 123 end),
    123 = agent:get(Agent),

    agent:stop(Agent).

start(_Config) ->
    ct:comment("State is initialized properly"),
    {ok, Agent} = agent:start(fun() -> 123 end),
    123 = agent:get(Agent),

    agent:stop(Agent).

get(_Config) ->
    {ok, Agent} = agent:start_link(fun() -> 123 end),

    ct:comment("Simple get"),
    123 = agent:get(Agent),

    ct:comment("Get with lambda"),
    124 = agent:get(Agent, fun(S) -> S+1 end),

    ct:comment("Get with lambda and timeout"),
    timeout = expect_timeout(fun() ->
        agent:get(Agent, fun(_) -> timer:sleep(10) end, 5)
    end),
    124 = agent:get(Agent, fun(S) -> S+1 end, 10),

    agent:stop(Agent).


get_and_update(_Config) ->
    {ok, Agent} = agent:start_link(fun() -> 123 end),
    ct:comment("Get and update"),
    124 = agent:get_and_update(Agent, fun(S) -> {S+1, S+2} end),
    125 = agent:get(Agent),

    ct:comment("Get and update with lambda and timeout"),
    126 = agent:get_and_update(Agent, fun(S) -> {S+1, S+2} end, 10),
    127 = agent:get(Agent),

    timeout = expect_timeout(fun() ->
        agent:get_and_update(Agent, fun(S) -> timer:sleep(10), {S, S} end, 5)
    end),

    agent:stop(Agent).


update(_Config) ->
    {ok, Agent} = agent:start_link(fun() -> 123 end),
    ct:comment("Update"),
    ok = agent:update(Agent, fun(S) -> S+1 end),
    124 = agent:get(Agent),

    ct:comment("Update with lambda and timeout"),
    ok = agent:update(Agent, fun(S) -> S+1 end, 10),
    125 = agent:get(Agent),

    timeout = expect_timeout(fun() ->
        agent:update(Agent, fun(_) -> timer:sleep(10) end, 5)
    end),

    agent:stop(Agent).

update_async(_Config) ->
    {ok, Agent} = agent:start_link(fun() -> 123 end),
    ct:comment("Update async"),
    ok = agent:update_async(Agent, fun(S) -> S+1 end),
    timer:sleep(10),
    124 = agent:get(Agent),

    agent:stop(Agent).

stop(_Config) ->
    ct:comment("Stop"),
    {ok, Agent0} = agent:start_link(fun() -> 123 end),
    ok = agent:stop(Agent0),
    undefined = erlang:process_info(Agent0),

    ct:comment("Stop with reason"),
    {ok, Agent1} = agent:start_link(fun() -> 123 end),
    process_flag(trap_exit, true),
    ok = agent:stop(Agent1, wow),
    receive {'EXIT', Agent1, wow} -> ok end,
    process_flag(trap_exit, false),
    undefined = erlang:process_info(Agent1),

    ct:comment("Stop with reason and timeout"),
    {ok, Agent2} = agent:start_link(fun() -> 123 end),
    process_flag(trap_exit, true),
    ok = agent:stop(Agent2, wow, 10),
    receive {'EXIT', Agent2, wow} -> ok end,
    process_flag(trap_exit, false),
    undefined = erlang:process_info(Agent2).

