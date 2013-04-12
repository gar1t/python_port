# python_port

[erlport](http://erlport.org) is a terrific Python library for creating Erlang
port servers. python_port is a simple OTP application that provides a behavior
for easily creating erlport base services.

There's a dependency on e2, which I apologize for! I would typically not foist
a dependency like this for a single module application. BUT this is a copy from
an app used in a larger application and I'm too lazy to pull the service impl
support out (e2 has some convenient functions to implement service behaviors).

I would happily make this a pure gen_server rather than an e2_service to avoid
the dependency. It's a simple maneuver, but alas takes more time than I care to
invest.

## Example

Python code:

``` python
from erlport import Port, Protocol

class Echo(Protocol):

    def handle_echo(self, msg):
        return msg

if __name__ == '__main__':
    Echo().run(Port(packet=4, use_stdio=True))
```

Erlang code:

``` erlang
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
```

See [examples](https://github.com/gar1t/python_port/tree/master/examples) for
more examples.

To play around with examples, use "make shell". For example:

```
$ cd examples
$ make shell
1> {ok, A} = arith:start_link().
2> 3 = arith:add(A, 1, 2).
```
