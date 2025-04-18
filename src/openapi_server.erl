-module(openapi_server).
-moduledoc """
There is a new description.
""".

-define(DEFAULT_LOGIC_HANDLER, openapi_logic_handler).

-export([start/2]).
-ignore_xref([start/2]).

-spec start(term(), #{transport      => tcp | ssl,
                      transport_opts => ranch:opts(),
                      protocol_opts  => cowboy:opts(),
                      service_routes => cowboy_router:routes(),
                      logic_handler  => module()}) ->
    {ok, pid()} | {error, any()}.
start(ID, Params) ->
    Transport = maps:get(transport, Params, tcp),
    TransportOpts = maps:get(transport_opts, Params, #{}),
    ProtocolOpts = maps:get(procotol_opts, Params, #{}),
    LogicHandler = maps:get(logic_handler, Params, ?DEFAULT_LOGIC_HANDLER),
    CowboyOpts = get_cowboy_config(LogicHandler, ProtocolOpts),
    ServiceRoutes = maps:get(service_routes, Params, []),
    CowboyOpts = get_cowboy_config(LogicHandler, ProtocolOpts, ServiceRoutes),
    case Transport of
        ssl ->
            cowboy:start_tls(ID, TransportOpts, CowboyOpts);
        tcp ->
            cowboy:start_clear(ID, TransportOpts, CowboyOpts)
    end.

get_cowboy_config(LogicHandler, ExtraOpts, ServiceRoutes) ->
    DefaultOpts = get_default_opts(LogicHandler, ServiceRoutes),
    maps:fold(fun do_get_cowboy_config/3, DefaultOpts, ExtraOpts).

do_get_cowboy_config(env, #{dispatch := _Dispatch} = Env, AccIn) ->
    AccIn#{env => Env};
do_get_cowboy_config(env, NewEnv, #{env := OldEnv} = AccIn) ->
    Env = maps:merge(OldEnv, NewEnv),
    AccIn#{env => Env};
do_get_cowboy_config(Key, Value, AccIn) ->
    AccIn#{Key => Value}.

get_default_dispatch(LogicHandler, ServiceRoutes) ->
    Paths = openapi_router:get_paths(LogicHandler, ServiceRoutes),
    #{dispatch => cowboy_router:compile(Paths)}.

get_default_opts(LogicHandler, ServiceRoutes) ->
    #{env => get_default_dispatch(LogicHandler, ServiceRoutes)}.
