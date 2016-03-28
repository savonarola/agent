agent
=====

[![Build Status](https://travis-ci.org/savonarola/agent.svg?branch=master)](https://travis-ci.org/savonarola/agent)

Implementation of agent primitive for simple state menagement.

Build
-----

    $ make

Tests
-----

    $ make test

Usage
-----

```erlang
{ok, Agent} = agent:start_link(fun() -> 123 end),

123 = agent:get(Agent),
246 = agent:get(Agent, fun(S) -> S * 2 end),

124 = agent:get_and_update(Agent, fun(S) -> {S+1, S+2} end),
127 = agent:get(Agent),

ok = agent:update(Agent, fun(S) -> S + 5 end),
132 = agent:get(Agent),

ok = agent:update_async(Agent, fun(S) -> S + 5 end),
timer:sleep(10),
137 = agent:get(Agent),

agent:stop(Agent).
```
