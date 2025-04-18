-module(openapi_router).

-export([get_paths/1]).

-type method() :: binary().
-type operations() :: #{method() => openapi_api:operation_id()}.
-type init_opts()  :: {operations(), module()}.

-export_type([init_opts/0]).

-spec get_paths(LogicHandler :: module()) -> cowboy_router:routes().
get_paths(LogicHandler) ->
    PreparedPaths = maps:fold(
                      fun(Path, #{operations := Operations, handler := Handler}, Acc) ->
                              [{Path, Handler, Operations} | Acc]
                      end, [], group_paths()
                     ),
    [{'_', [{P, H, {O, LogicHandler}} || {P, H, O} <- PreparedPaths]}].

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
       'addPet' => #{
            servers => [],
            base_path => "",
            path => "/pet",
            method => <<"POST">>,
            handler => 'openapi_pet_handler'
        },
       'deletePet' => #{
            servers => [],
            base_path => "",
            path => "/pet/:petId",
            method => <<"DELETE">>,
            handler => 'openapi_pet_handler'
        },
       'findPetsByStatus' => #{
            servers => [],
            base_path => "",
            path => "/pet/findByStatus",
            method => <<"GET">>,
            handler => 'openapi_pet_handler'
        },
       'findPetsByTags' => #{
            servers => [],
            base_path => "",
            path => "/pet/findByTags",
            method => <<"GET">>,
            handler => 'openapi_pet_handler'
        },
       'getPetById' => #{
            servers => [],
            base_path => "",
            path => "/pet/:petId",
            method => <<"GET">>,
            handler => 'openapi_pet_handler'
        },
       'updatePet' => #{
            servers => [],
            base_path => "",
            path => "/pet",
            method => <<"PUT">>,
            handler => 'openapi_pet_handler'
        },
       'updatePetWithForm' => #{
            servers => [],
            base_path => "",
            path => "/pet/:petId",
            method => <<"POST">>,
            handler => 'openapi_pet_handler'
        },
       'uploadFile' => #{
            servers => [],
            base_path => "",
            path => "/pet/:petId/uploadImage",
            method => <<"POST">>,
            handler => 'openapi_pet_handler'
        }
    }.
