-module(openapi_pet_handler).
-moduledoc """
Exposes the following operation IDs:

- `POST` to `/pet`, OperationId: `addPet`:
Add a new pet to the store.


- `DELETE` to `/pet/:petId`, OperationId: `deletePet`:
Deletes a pet.


- `GET` to `/pet/findByStatus`, OperationId: `findPetsByStatus`:
Finds Pets by status.
Multiple status values can be provided with comma separated strings

- `GET` to `/pet/findByTags`, OperationId: `findPetsByTags`:
Finds Pets by tags.
Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

- `GET` to `/pet/:petId`, OperationId: `getPetById`:
Find pet by ID.
Returns a single pet

- `PUT` to `/pet`, OperationId: `updatePet`:
Update an existing pet.


- `POST` to `/pet/:petId`, OperationId: `updatePetWithForm`:
Updates a pet in the store with form data.


- `POST` to `/pet/:petId/uploadImage`, OperationId: `uploadFile`:
uploads an image.


""".

-behaviour(cowboy_rest).

%% Cowboy REST callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_resource/2]).
-export([forbidden/2]).
-export([valid_content_headers/2]).
-export([handle_type_accepted/2, handle_type_provided/2]).

-ignore_xref([handle_type_accepted/2, handle_type_provided/2]).

-export_type([class/0, operation_id/0]).

-type class() :: 'pet'.

-type operation_id() ::
    'addPet' %% Add a new pet to the store
    | 'deletePet' %% Deletes a pet
    | 'findPetsByStatus' %% Finds Pets by status
    | 'findPetsByTags' %% Finds Pets by tags
    | 'getPetById' %% Find pet by ID
    | 'updatePet' %% Update an existing pet
    | 'updatePetWithForm' %% Updates a pet in the store with form data
    | 'uploadFile'. %% uploads an image


-record(state,
        {operation_id :: operation_id(),
         accept_callback :: openapi_logic_handler:accept_callback(),
         provide_callback :: openapi_logic_handler:provide_callback(),
         validate_response :: openapi_logic_handler:validate_response() | undefined,
         forbidden_callback :: openapi_logic_handler:forbidden_callback() | undefined,
         context = #{} :: openapi_logic_handler:context()}).

-type state() :: #state{}.

-spec init(cowboy_req:req(), openapi_router:init_opts()) ->
    {cowboy_rest, cowboy_req:req(), state()}.
init(Req, {Operations, Module}) ->
    Method = cowboy_req:method(Req),
    OperationID = maps:get(Method, Operations, undefined),
    State = #state{operation_id = OperationID,
                   accept_callback = fun Module:accept_callback/4,
                   provide_callback = fun Module:provide_callback/4,
                   validate_response = maybe_exported_fun(Module, validate_response, 5),
                   forbidden_callback = maybe_exported_fun(Module, forbidden_callback, 4)},
    {cowboy_rest, Req, State}.


-spec forbidden(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
forbidden(Req, #state{operation_id = OperationID,
                      forbidden_callback = undefined,
                      context = Context} = State) ->
    handle_result(openapi_logic_handler:forbidden_callback(pet, OperationID, Req, Context), State);
forbidden(Req, #state{operation_id = OperationID,
                      forbidden_callback = Handler,
                      context = Context} = State) ->
    handle_result(Handler(pet, OperationID, Req, Context), State).

-spec allowed_methods(cowboy_req:req(), state()) ->
    {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, #state{operation_id = 'addPet'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'deletePet'} = State) ->
    {[<<"DELETE">>], Req, State};
allowed_methods(Req, #state{operation_id = 'findPetsByStatus'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'findPetsByTags'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'getPetById'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'updatePet'} = State) ->
    {[<<"PUT">>], Req, State};
allowed_methods(Req, #state{operation_id = 'updatePetWithForm'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'uploadFile'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, State) ->
    {[], Req, State}.

-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, #state{operation_id = 'addPet'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted},
      {<<"application/xml">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'deletePet'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'findPetsByStatus'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'findPetsByTags'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'getPetById'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'updatePet'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted},
      {<<"application/xml">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'updatePetWithForm'} = State) ->
    {[
      {<<"application/x-www-form-urlencoded">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'uploadFile'} = State) ->
    {[
      {<<"multipart/form-data">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, State) ->
    {[], Req, State}.

-spec valid_content_headers(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
valid_content_headers(Req, #state{operation_id = 'addPet'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'deletePet'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'findPetsByStatus'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'findPetsByTags'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'getPetById'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'updatePet'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'updatePetWithForm'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'uploadFile'} = State) ->
    {true, Req, State};
valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, #state{operation_id = 'addPet'} = State) ->
    {[
      {<<"application/xml">>, handle_type_provided},
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'deletePet'} = State) ->
    {[], Req, State};
content_types_provided(Req, #state{operation_id = 'findPetsByStatus'} = State) ->
    {[
      {<<"application/xml">>, handle_type_provided},
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'findPetsByTags'} = State) ->
    {[
      {<<"application/xml">>, handle_type_provided},
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'getPetById'} = State) ->
    {[
      {<<"application/xml">>, handle_type_provided},
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'updatePet'} = State) ->
    {[
      {<<"application/xml">>, handle_type_provided},
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'updatePetWithForm'} = State) ->
    {[], Req, State};
content_types_provided(Req, #state{operation_id = 'uploadFile'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, State) ->
    {[], Req, State}.

-spec delete_resource(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
delete_resource(Req, State) ->
    {Res, Req1, State1} = handle_type_accepted(Req, State),
    {true =:= Res, Req1, State1}.

-spec handle_type_accepted(cowboy_req:req(), state()) ->
    { openapi_logic_handler:accept_callback_return(), cowboy_req:req(), state()}.
handle_type_accepted(Req, #state{operation_id = OperationID,
                                 accept_callback = Handler,
                                 validate_response = ValidateHandler,
                                 context = Context} = State) ->
    ValidatorState = openapi_api:prepare_validator(),
    case openapi_api:populate_request(OperationID, Req, ValidatorState) of
        {ok, Model, Req1} ->
            Context1 = maps:merge(Context, Model),
            {Code, Res, Req2, State2} = handle_result(Handler(pet, OperationID, Req1, Context1), State),
            validate_response(ValidateHandler, OperationID, Code, Res, ValidatorState),
            process_response(Code, Res, Req2, State2);
        {error, Reason, ErrReq} ->
            process_response(400, format_error(Reason), ErrReq, State)
    end.

-spec handle_type_provided(cowboy_req:req(), state()) ->
    { openapi_logic_handler:provide_callback_return(), cowboy_req:req(), state()}.
handle_type_provided(Req, #state{operation_id = OperationID,
                                 provide_callback = Handler,
                                 validate_response = ValidateHandler,
                                 context = Context} = State) ->
    ValidatorState = openapi_api:prepare_validator(),
    case openapi_api:populate_request(OperationID, Req, ValidatorState) of
        {ok, Model, Req1} ->
            Context1 = maps:merge(Context, Model),
            {Code, Res, Req2, State2} = handle_result(Handler(pet, OperationID, Req1, Context1), State),
            validate_response(ValidateHandler, OperationID, Code, Res, ValidatorState),
            openapi_api:validate_response(OperationID, Code, Res, ValidatorState),
            process_response(Code, Res, Req2, State2);
        {error, Reason, ErrReq} ->
            process_response(400, format_error(Reason), ErrReq, State)
    end.

validate_response(undefined, _OperationID, _Code, _Res, _ValidatorState) ->
    ok;
validate_response(Handler, OperationID, Code, Res, ValidatorState) ->
    Handler(pet, OperationID, Code, Res, ValidatorState).

format_error({wrong_param, Name, Val, Rule, Info}) ->
    #{
        message => <<"Bad params">>,
        description => openapi_api:format_error_description(Name, Val, Rule, Info)
    }.

handle_result({Res, Req, Context}, State) ->
    {Res, Req, State#state{context = Context}};
handle_result({Code, Res, Req, Context}, State) ->
    {Code, Res, Req, State#state{context = Context}}.


-spec process_response(pos_integer(), openapi_logic_handler:provide_callback_return(), cowboy_req:req(), state()) -> 
    {stop, cowboy_req:req(), state()}.
process_response(Code, Res, Req, State) ->
    Req2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req),
    Res3 = cowboy_req:set_resp_body(json:encode(Res), Req2),
    ReqR = cowboy_req:reply(Code, Res3),
    {stop, ReqR, State}.

maybe_exported_fun(Module, Function, Arity) ->
    case erlang:function_exported(Module, Function, Arity) of
        true ->
            fun Module:Function/Arity;
        false ->
            undefined
    end.
