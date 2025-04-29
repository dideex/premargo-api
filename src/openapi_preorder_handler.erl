-module(openapi_preorder_handler).
-moduledoc """
Exposes the following operation IDs:

- `GET` to `/v1.0/preorder_margin/:account_id`, OperationId: `preorder_check_get`:
Estimate trade margin for one order.


- `POST` to `/v1.0/preorder_margin/:account_id`, OperationId: `preorder_check_post`:
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
-export([handle_type_accepted/2]).

-ignore_xref([handle_type_accepted/2]).

-export_type([operation_id/0]).

-type operation_id() ::
    'preorder_check_get' %% Estimate trade margin for one order
    | 'preorder_check_post'. %% Estimate trade margin for list of orders


-record(state,
        {operation_id :: operation_id(),
         handle_callback :: openapi_logic_handler:handle_callback(),
         custom_type_validator :: openapi_logic_handler:custom_type_validator() | undefined,
         validate_response :: openapi_logic_handler:validate_response() | undefined,
         forbidden_callback :: openapi_logic_handler:forbidden_callback() | undefined,
         response_encoder :: openapi_logic_handler:response_encoder() | undefined,
         context = #{} :: openapi_logic_handler:context()}).

-type state() :: #state{}.

-type context() :: openapi_logic_handler:context().
-type opts() :: openapi_logic_handler:opts().
-type code() :: openapi_logic_handler:code().
-type response() :: openapi_logic_handler:handle_callback_return().

-spec init(cowboy_req:req(), openapi_router:init_opts()) ->
    {cowboy_rest, cowboy_req:req(), state()}.
init(Req, {Operations, Module}) ->
    Method = cowboy_req:method(Req),
    OperationID = maps:get(Method, Operations, undefined),
    State = #state{operation_id = OperationID,
                   handle_callback = fun Module:handle_callback/3,
                   custom_type_validator = maybe_exported_fun(Module, custom_type_validator, 3),
                   validate_response = maybe_exported_fun(Module, validate_response, 5),
                   response_encoder = maybe_exported_fun(Module, response_encoder, 2),
                   forbidden_callback = maybe_exported_fun(Module, forbidden_callback, 3)},
    {cowboy_rest, Req, State}.


-spec forbidden(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
forbidden(Req, #state{operation_id = OperationID,
                      forbidden_callback = undefined,
                      context = Context} = State) ->
    handle_result(openapi_logic_handler:forbidden_callback(OperationID, Context, make_opts(Req)), State);
forbidden(Req, #state{operation_id = OperationID,
                      forbidden_callback = Handler,
                      context = Context} = State) ->
    handle_result(Handler(OperationID, Context, make_opts(Req)), State).

-spec allowed_methods(cowboy_req:req(), state()) ->
    {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, #state{operation_id = 'preorder_check_get'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'preorder_check_post'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, State) ->
    {[], Req, State}.

-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, #state{operation_id = 'preorder_check_get'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'preorder_check_post'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, State) ->
    {[], Req, State}.

-spec valid_content_headers(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
valid_content_headers(Req, #state{operation_id = 'preorder_check_get'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'preorder_check_post'} = State) ->
    {true, Req, State};
valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, #state{operation_id = 'preorder_check_get'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'preorder_check_post'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
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
                                 handle_callback = Handler,
                                 validate_response = ValidateHandler,
                                 response_encoder = ResponseEndcoder,
                                 custom_type_validator = CustomValidator,
                                 context = Context} = State) ->
    ValidatorState = openapi_api:prepare_validator(),
    case openapi_api:populate_request(OperationID, Req, ValidatorState, CustomValidator) of
        {ok, Model, Req1} ->
            Context1 = maps:merge(Context, Model),
            {Code, Res, Req2, State2} = handle_result(Handler(OperationID, Context1, make_opts(Req1)), State),
            HttpCode = http_code_mapper(Code),
            validate_response(ValidateHandler, OperationID, HttpCode, Res, ValidatorState),
            Body = encode(ResponseEndcoder, OperationID, Res),
            process_response(HttpCode, Body, Req2, State2);
        {error, Reason, ErrReq} ->
            process_response(400, format_error(Reason), ErrReq, State)
    end.

validate_response(undefined, _OperationID, _Code, _Res, _ValidatorState) ->
    ok;
validate_response(_Handler, _OperationID, _Code, stop, _ValidatorState) ->
    ok;
validate_response(Handler, OperationID, Code, Res, ValidatorState) ->
    Handler(OperationID, Code, Res, ValidatorState).

format_error(Error) ->
    json:encode(#{
        message => <<"Bad params">>,
        description => error_description(Error)
    }).

error_description({wrong_param, Name, Val, Rule, Info}) ->
    openapi_api:format_error_description(Name, Val, Rule, Info);
error_description({custom_error, Msg}) ->
    Msg.

-spec handle_result({boolean(), context(), opts()}, state()) ->
        {boolean(), cowboy_req:req(), state()};
    ({code(), response(), context(), opts()}, state()) ->
        {code(), response(), cowboy_req:req(), state()}.
handle_result({Res, Context, #{req := Req}}, State) ->
    {Res, Req, State#state{context = Context}};
handle_result({Code, Res, Context, #{req := Req}}, State) ->
    {Code, Res, Req, State#state{context = Context}}.


-spec process_response(pos_integer(), iodata() | stop, cowboy_req:req(), state()) -> 
    {stop, cowboy_req:req(), state()}.
process_response(_Code, stop, Req, State) ->
    {stop, Req, State};
process_response(Code, Body, Req, State) ->
    Req2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req),
    Res3 = cowboy_req:set_resp_body(Body, Req2),
    ReqR = cowboy_req:reply(Code, Res3),
    {stop, ReqR, State}.

-spec encode(openapi_logic_handler:response_encoder() | undefined, operation_id(), response()) ->
    iodata() | stop.
encode(_Handler, _OperationID, stop) ->
    stop;
encode(undefined, _OperationID, Res) ->
    json:encode(Res);
encode(Handler, OperationID, Res) ->
    Handler(OperationID, Res).

maybe_exported_fun(Module, Function, Arity) ->
    case erlang:function_exported(Module, Function, Arity) of
        true ->
            fun Module:Function/Arity;
        false ->
            undefined
    end.

-spec http_code_mapper(openapi_logic_handler:code()) -> pos_integer().
http_code_mapper(ok) -> 200;
http_code_mapper(created) -> 201;
http_code_mapper(accepted) -> 202;
http_code_mapper(moved_permanently) -> 301;
http_code_mapper(found) -> 302;
http_code_mapper(see_other) -> 303;
http_code_mapper(bad_request) -> 400;
http_code_mapper(unauthorized) -> 401;
http_code_mapper(forbidden) -> 403;
http_code_mapper(not_found) -> 404;
http_code_mapper(method_not_allowed) -> 405.

-spec make_opts(cowboy_req:req()) -> opts().
make_opts(Req) -> #{req => Req}.
