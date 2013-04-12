-module(arith).

-behavior(python_port).

-export([start_link/0, add/3, subtract/3, multiply/3, divide/3]).

-export([init/1]).

start_link() ->
    python_port:start_link(?MODULE, [], []).

init([]) ->
    {ok, [{module, "arith"}]}.

add(Math, X, Y) ->
    python_port:call_port(Math, {add, X, Y}).

subtract(Math, X, Y) ->
    python_port:call_port(Math, {subtr, X, Y}).

multiply(Math, X, Y) ->
    python_port:call_port(Math, {mult, X, Y}).

divide(Math, X, Y) ->
    python_port:call_port(Math, {'div', X, Y}).
