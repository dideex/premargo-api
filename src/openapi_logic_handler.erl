-module(openapi_logic_handler).

-include_lib("kernel/include/logger.hrl").

-type code() :: code2xx() | code3xx() | code4xx().
-type code2xx() :: ok | created | accepted.
-type code3xx() :: moved_permanently | found | see_other.
-type code4xx() :: bad_request | unauthorized | forbidden | not_found | method_not_allowed.
-export_type([code/0]).

-type opts() :: #{
    req := cowboy_req:req()
}.
-export_type([opts/0]).

-type response() :: map() | list().
-type operation_id() :: openapi_api:operation_id().
-type rule() :: openapi_api:rule().
-type request_param() :: openapi_api:request_param().

-type handle_callback_return() :: stop | response().
-type handle_callback() ::
    fun((operation_id(), context(), opts()) ->
            {code(), handle_callback_return(), context(), opts()}).
-type forbidden_callback() ::
    fun((operation_id(), context(), opts()) ->
            {boolean(), context(), opts()}).
-type validate_response() ::
    fun((operation_id(), code(), response(), jesse_state:state()) ->
            ok | no_return()).
-type response_encoder() ::
    fun((operation_id(), response()) ->
            iodata()).
-type custom_type_validator() :: fun((rule(), term(), request_param()) ->
    ok | {ok, term()} | default | {wrong_param, rule(), term(), request_param()}) | {error, iodata()}.
-type context() :: map().

-export_type([context/0, response_encoder/0, custom_type_validator/0,
              handle_callback_return/0, validate_response/0,
              handle_callback/0, forbidden_callback/0]).

-optional_callbacks([forbidden_callback/3, validate_response/4, custom_type_validator/3, response_encoder/2]).

-callback handle_callback(operation_id(), context(), opts()) ->
    {code(), handle_callback_return(), context(), opts()}.

-callback forbidden_callback(operation_id(), context(), opts()) ->
    {boolean(), context(), opts()}.

-callback validate_response(operation_id(), code(), response(), jesse_state:state()) ->
    ok | no_return().

-callback response_encoder(operation_id(), response()) ->
    iodata().

-callback custom_type_validator(rule(), Value, Name) ->
    ok | {ok, Value} | default | {wrong_param, rule(), Value, Name} | {error, iodata()}
    when Value :: term(), Name :: request_param().

-export([handle_callback/3, forbidden_callback/3]).
-ignore_xref([handle_callback/3, forbidden_callback/3]).

-spec forbidden_callback(operation_id(), context(), opts()) ->
    {boolean(), context(), opts()}.
forbidden_callback(_OperationID, Context, Opts) ->
    {false, Context, Opts}.

-spec handle_callback(operation_id(), context(), opts()) ->
    {code(), handle_callback_return(), context(), opts()}.
handle_callback(OperationID, Context, Opts) ->
    ?LOG_ERROR(#{what => "Got not implemented request to process",
                 operation_id => OperationID,
                 opts => Opts,
                 context => Context}),
    {bad_request, stop, Context, Opts}.
