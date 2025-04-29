-module(openapi_router).

-export([get_paths/3]).

-type method() :: binary().
-type operations() :: #{method() => openapi_api:operation_id()}.
-type init_opts()  :: {operations(), module()}.

-export_type([init_opts/0]).

-spec get_paths(
        LogicHandler :: module(),
        ServiceRoutes :: cowboy_router:routes(),
        SwaggerHandler :: openapi_swagger:swagger_json_handler()
    ) -> cowboy_router:routes().
get_paths(LogicHandler, ServiceRoutes, SwaggerHandler) ->
    PreparedPaths = maps:fold(
                      fun(Path, #{operations := Operations, handler := Handler}, Acc) ->
                              [{Path, Handler, Operations} | Acc]
                      end, [], group_paths()
                     ),
    {RootServiceRoutes, NonRootServiceRoutes} =
        lists:partition(fun(R) -> element(1, R) =:= '_' end, ServiceRoutes),
    RootServicePaths = lists:flatten([R || {_, R} <- RootServiceRoutes]),
    CompiledPaths = [{P, H, {O, LogicHandler}} || {P, H, O} <- PreparedPaths],
    SwaggerPaths = [
        {"/api-docs/openapi.json", openapi_swagger, #{swagger_json_handler => SwaggerHandler}},
        {"/api-docs/swagger/", cowboy_static, {priv_file, openapi, "swagger/index.html"}},
        {"/api-docs/swagger/[...]", cowboy_static, {priv_dir, openapi, "swagger"}}
    ],
    [{'_', SwaggerPaths ++ CompiledPaths ++ RootServicePaths} | NonRootServiceRoutes].

group_paths() ->
    maps:fold(
      fun(OperationID, #{servers := Servers, base_path := BasePath, path := Path,
                         method := Method, handler := Handler}, Acc) ->
              FullPaths = build_full_paths(Servers, BasePath, Path),
              merge_paths(FullPaths, OperationID, Method, Handler, Acc)
      end, #{}, get_operations()).

build_full_paths([], BasePath, Path) ->
    [lists:append([BasePath, Path])];
build_full_paths(Servers, _BasePath, Path) ->
    [lists:append([Server, Path]) || Server <- Servers ].

merge_paths(FullPaths, OperationID, Method, Handler, Acc) ->
    lists:foldl(
      fun(Path, Acc0) ->
              case maps:find(Path, Acc0) of
                  {ok, PathInfo0 = #{operations := Operations0}} ->
                      Operations = Operations0#{Method => OperationID},
                      PathInfo = PathInfo0#{operations => Operations},
                      Acc0#{Path => PathInfo};
                  error ->
                      Operations = #{Method => OperationID},
                      PathInfo = #{handler => Handler, operations => Operations},
                      Acc0#{Path => PathInfo}
              end
      end, Acc, FullPaths).

get_operations() ->
    #{ 
       'preorder_check_get' => #{
            servers => [],
            base_path => "/api",
            path => "/v1.0/preorder_margin/:account_id",
            method => <<"GET">>,
            handler => 'openapi_preorder_handler'
        },
       'preorder_check_post' => #{
            servers => [],
            base_path => "/api",
            path => "/v1.0/preorder_margin/:account_id",
            method => <<"POST">>,
            handler => 'openapi_preorder_handler'
        }
    }.