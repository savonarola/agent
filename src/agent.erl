-module(agent).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([start_link/1, start_link/2,
         start/1, start/2,
         get/1, get/2, get/3,
         get_and_update/2, get_and_update/3,
         update/2, update/3,
         update_async/2,
         stop/1, stop/2, stop/3]).

-type on_start() :: {ok, pid()} | {error, {already_started, pid()} | term()}.
-type name() :: atom() | {global, term()} | {via, module(), term()}.
-type agent() :: pid() | {atom(), node()} | name().
-type state() :: term().
-type options() :: list().

-type init_fun() :: fun(() -> state()).
-type get_fun() :: fun((state()) -> term()).
-type get_and_update_fun() :: fun((state()) -> {term(), state()}).
-type update_fun() :: fun((state()) -> state()).

-spec start_link(init_fun()) -> on_start().
start_link(Fun) when is_function(Fun, 0) ->
    start_link(Fun, []).

-spec start_link(init_fun(), options()) -> on_start().
start_link(Fun, Options) when is_function(Fun, 0) and is_list(Options) ->
    gen_server:start_link(?MODULE, Fun, Options).

-spec start(init_fun()) -> on_start().
start(Fun) when is_function(Fun, 0) ->
    start(Fun, []).

-spec start(init_fun(), options()) -> on_start().
start(Fun, Options) when is_function(Fun, 0) and is_list(Options)  ->
    gen_server:start(?MODULE, Fun, Options).

-spec get_and_update(agent(), get_and_update_fun()) -> term().
get_and_update(Agent, Fun) when is_function(Fun, 1)->
    gen_server:call(Agent, {get_and_update, Fun}).

-spec get_and_update(agent(), get_and_update_fun(), timeout()) -> term().
get_and_update(Agent, Fun, Timeout) when is_function(Fun, 1) ->
    gen_server:call(Agent, {get_and_update, Fun}, Timeout).

-spec get(agent()) -> term().
get(Agent) ->
    ?MODULE:get(Agent, fun(State) -> State end).

-spec get(agent(), get_fun()) -> term().
get(Agent, Fun) when is_function(Fun, 1)->
    get_and_update(Agent, fun(State) -> {Fun(State), State} end).

-spec get(agent(), get_fun(), timeout()) -> term().
get(Agent, Fun, Timeout) when is_function(Fun, 1) ->
    get_and_update(Agent, fun(State) -> {Fun(State), State} end, Timeout).

-spec update(agent(), update_fun()) -> ok.
update(Agent, Fun) when is_function(Fun, 1)->
    get_and_update(Agent, fun(State) -> {ok, Fun(State)} end).

-spec update(agent(), update_fun(), timeout()) -> ok.
update(Agent, Fun, Timeout) when is_function(Fun, 1) ->
    get_and_update(Agent, fun(State) -> {ok, Fun(State)} end, Timeout).

-spec update_async(agent(), update_fun()) -> ok.
update_async(Agent, Fun) when is_function(Fun, 1)->
    gen_server:cast(Agent, {update, Fun}).

-spec stop(agent()) -> ok.
stop(Agent) ->
    stop(Agent, normal).

-spec stop(agent(), term()) -> ok.
stop(Agent, Reason) ->
    stop(Agent, Reason, infinity).

-spec stop(agent(), term(), timeout()) -> ok.
stop(Agent, Reason, Timeout) ->
    gen_server:stop(Agent, Reason, Timeout).


init(Fun) ->
    {ok, Fun()}.

handle_call({get_and_update, Fun}, _From, State) ->
    case Fun(State) of
        {Reply, NewState} -> {reply, Reply, NewState};
        Other -> {stop, {bad_return_value, Other}, State}
    end.

handle_cast({update, Fun}, State) ->
    {noreply, Fun(State)}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
