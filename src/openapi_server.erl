-module(openapi_server).
-moduledoc """
There is a new description.
""".

-define(DEFAULT_LOGIC_HANDLER, openapi_logic_handler).

-export([start/2]).
-ignore_xref([start/2]).

-spec start(term(), #{transport      => tcp | ssl,
                      transport_opts => ranch:opts(),
                      swagger_json_handler => openapi_swagger:swagger_json_handler(),
                      protocol_opts  => cowboy:opts(),
                      service_routes => cowboy_router:routes(),
                      logic_handler  => module()}) ->
    {ok, pid()} | {error, any()}.
start(ID, Params) ->
    Transport = maps:get(transport, Params, tcp),
    TransportOpts = maps:get(transport_opts, Params, #{}),
    ProtocolOpts = maps:get(procotol_opts, Params, #{}),
    LogicHandler = maps:get(logic_handler, Params, ?DEFAULT_LOGIC_HANDLER),
    ServiceRoutes = maps:get(service_routes, Params, []),
    SwaggerHandler = maps:get(swagger_json_handler, Params, undefined),
    CowboyOpts = get_cowboy_config(LogicHandler, ProtocolOpts, ServiceRoutes, SwaggerHandler),
    case Transport of
        ssl ->
            cowboy:start_tls(ID, TransportOpts, CowboyOpts);
        tcp ->
            cowboy:start_clear(ID, TransportOpts, CowboyOpts)
    end.

get_cowboy_config(LogicHandler, ExtraOpts, ServiceRoutes, SwaggerHandler) ->
    DefaultOpts = get_default_opts(LogicHandler, ServiceRoutes, SwaggerHandler),
    maps:fold(fun do_get_cowboy_config/3, DefaultOpts, ExtraOpts).

do_get_cowboy_config(env, #{dispatch := _Dispatch} = Env, AccIn) ->
    AccIn#{env => Env};
do_get_cowboy_config(env, NewEnv, #{env := OldEnv} = AccIn) ->
    Env = maps:merge(OldEnv, NewEnv),
    AccIn#{env => Env};
do_get_cowboy_config(Key, Value, AccIn) ->
    AccIn#{Key => Value}.

get_default_dispatch(LogicHandler, ServiceRoutes, SwaggerHandler) ->
    Paths = openapi_router:get_paths(LogicHandler, ServiceRoutes, SwaggerHandler),
    #{dispatch => cowboy_router:compile(Paths)}.

get_default_opts(LogicHandler, ServiceRoutes, SwaggerHandler) ->
    #{env => get_default_dispatch(LogicHandler, ServiceRoutes, SwaggerHandler)}.
