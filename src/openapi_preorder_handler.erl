-module(openapi_preorder_handler).
-moduledoc """
Exposes the following operation IDs:

- `GET` to `/preorder/:account_id`, OperationId: ``:
Estimate trade margin for one order.


- `POST` to `/preorder/:account_id`, OperationId: ``:
Estimate trade margin for list of orders.


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

-type class() :: 'preorder'.

-type operation_id() ::
    '' %% Estimate trade margin for one order
    | ''. %% Estimate trade margin for list of orders


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
allowed_methods(Req, #state{operation_id = ''} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = ''} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, State) ->
    {[], Req, State}.

-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, #state{operation_id = ''} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = ''} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, State) ->
    {[], Req, State}.

-spec valid_content_headers(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
valid_content_headers(Req, #state{operation_id = ''} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = ''} = State) ->
    {true, Req, State};
valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, #state{operation_id = ''} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = ''} = State) ->
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
            {Code, Res, Req2, State2} = handle_result(Handler(preorder, OperationID, Req1, Context1), State),
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
            {Code, Res, Req2, State2} = handle_result(Handler(preorder, OperationID, Req1, Context1), State),
            validate_response(ValidateHandler, OperationID, Code, Res, ValidatorState),
            openapi_api:validate_response(OperationID, Code, Res, ValidatorState),
            process_response(Code, Res, Req2, State2);
        {error, Reason, ErrReq} ->
            process_response(400, format_error(Reason), ErrReq, State)
    end.

validate_response(undefined, _OperationID, _Code, _Res, _ValidatorState) ->
    ok;
validate_response(_Handler, _OperationID, _Code, stop, _ValidatorState) ->
    ok;
validate_response(Handler, OperationID, Code, Res, ValidatorState) ->
    Handler(preorder, OperationID, Code, Res, ValidatorState).

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
process_response(_Code, stop, Req, State) ->
    {stop, Req, State};
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
