-module(echo).

-behavior(python_port).

-export([start_link/0, echo/1]).

-export([init/1]).

start_link() ->
    python_port:start_link(?MODULE, [], [registered]).

init([]) ->
    {ok, [{module, "echo"}]}.

echo(Msg) ->
    python_port:call_port(?MODULE, {echo, Msg}).
