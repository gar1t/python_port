-module(python_port).

-behaviour(e2_service).

-export([start_link/3, call_port/2, call_port/3]).

-export([init/1, handle_msg/3]).

-record(state, {mod, mod_state, port}).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{init, 1}].

%%%===================================================================
%%% Public API
%%%===================================================================

start_link(Module, ModArgs, Options) ->
    ServiceOpts = e2_service_impl:service_options(Module, Options),
    e2_service:start_link(?MODULE, [Module, ModArgs], ServiceOpts).

call_port(PortServer, Msg) ->
    call_port(PortServer, Msg, infinity).

call_port(PortServer, Msg, Timeout) ->
    e2_service:call(PortServer, {call_port, Msg, Timeout}).

%%%===================================================================
%%% e2_service callbacks
%%%===================================================================

init([Mod, ModArgs]) ->
    case Mod:init(ModArgs) of
        {ok, PortInfo} ->
            handle_init(Mod, PortInfo, []);
	{ok, PortInfo, ModState} ->
            handle_init(Mod, PortInfo, ModState);
	{stop, Reason} ->
            {stop, Reason}
    end.

handle_init(Mod, PortInfo, ModState) ->
    process_flag(trap_exit, true),
    DefaultPyMod = {priv_py, atom_to_list(Mod)},
    PrivDir = priv_dir(Mod),
    Port = start_port([{default_module, DefaultPyMod},
                       {priv_dir, PrivDir}|PortInfo]),
    {ok, #state{mod=Mod,
                mod_state=ModState,
                port=Port}}.

handle_msg({call_port, Msg, Timeout}, _From, #state{port=Port}=State) ->
    port_command(Port, term_to_binary(Msg)),
    receive
	{Port, {data, Reply}} -> {reply, binary_to_term(Reply), State}
    after
	Timeout -> {stop, port_timeout, State}
    end;
handle_msg({'EXIT', Port, Reason}, noreply, #state{port=Port}=State) ->
    {stop, {port_terminated, Reason}, State};
handle_msg(Msg, From, #state{mod=Module, mod_state=ModState0}=State) ->
    {Result, ModState} =
        e2_service_impl:dispatch_handle_msg(Module, Msg, From, ModState0),
    e2_service_impl:handle_msg_result(Result, State#state{mod_state=ModState}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_port(Opts) ->
    Python = find_python(Opts),
    PyMod = find_python_mod(Opts),
    PyArgs = proplists:get_value(arguments, Opts, []),
    Env = [{"PYTHONPATH", python_path(Opts)}],
    PortOpts = [{args, ["-u", PyMod|PyArgs]},
                {packet, 4},
                binary,
                {env, Env}],
    open_port({spawn_executable, Python}, PortOpts).

python_path(Opts) ->
    join_path([erlport_path()|python_path_option(Opts)]).

erlport_path() ->
    filename:join(priv_dir(?MODULE), "py").

python_path_option(Opts) ->
    proplists:get_value(python_path, Opts, []).

join_path(Parts) ->
    string:join(Parts, ":").

priv_dir(Mod) ->
    ModPath =
        case code:which(Mod) of
            non_existing -> erlang:error({invalid_module, Mod});
            Path -> Path
        end,
    filename:join(filename:dirname(filename:dirname(ModPath)), "priv").

find_python(_Opts) ->
    case os:find_executable("python2") of
	false ->
	    case os:find_executable("python") of
		false -> erlang:error(cant_find_python);
		P -> P
	    end;
	P2 -> P2
    end.

find_python_mod(Opts) ->
    DefaultMod = proplists:get_value(default_module, Opts),
    case proplists:get_value(module, Opts, DefaultMod) of
	{priv_py, PyMod} ->
	    find_priv_python_mod(PyMod, Opts);
	{path, Path} ->
	    find_path_mod(Path);
        Module ->
            find_mod(Module)
    end.

find_priv_python_mod(PyMod, Opts) ->
    PrivDir = proplists:get_value(priv_dir, Opts),
    find_mod(filename:join([PrivDir, "py", PyMod ++ ".py"])).

find_path_mod(Path) ->
    case filelib:is_file(Path) of
	true -> Path;
	false -> erlang:error({missing_module, Path})
    end.

find_mod(Mod) ->
    find_path_mod(mod_path(Mod)).

mod_path(Mod) ->
    [re:replace(Mod, "\\.", "/"), ".py"].
