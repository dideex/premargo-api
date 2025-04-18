-module(openapi_logic_handler).

-include_lib("kernel/include/logger.hrl").

-type code() :: 200..599.
-export_type([code/0]).

-type response() :: map() | list().

-type accept_callback_return() ::
        stop
        | response().
-type provide_callback_return() ::
        stop
        | response().
-type accept_callback() ::
    fun((openapi_api:class(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
            {code(), accept_callback_return(), cowboy_req:req(), context()}).
-type provide_callback() ::
    fun((openapi_api:class(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
            {code(), cowboy_req:resp_body(), cowboy_req:req(), context()}).
-type forbidden_callback() ::
    fun((openapi_api:class(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
            {boolean(), cowboy_req:req(), context()}).
-type validate_response() ::
    fun((openapi_api:class(), openapi_api:operation_id(), code(), response(), jesse_state:state()) ->
            ok | no_return()).
-type context() :: map().

-export_type([context/0,
              accept_callback_return/0, provide_callback_return/0, validate_response/0,
              accept_callback/0, provide_callback/0, forbidden_callback/0]).

-optional_callbacks([forbidden_callback/4, validate_response/5]).

-callback accept_callback(openapi_api:class(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
    {code(), accept_callback_return(), cowboy_req:req(), context()}.

-callback provide_callback(openapi_api:class(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
    {code(), provide_callback_return(), cowboy_req:req(), context()}.

-callback forbidden_callback(openapi_api:class(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
    {boolean(), cowboy_req:req(), context()}.

-callback validate_response(openapi_api:class(), openapi_api:operation_id(), code(), response(), jesse_state:state()) ->
    ok | no_return().

-export([accept_callback/4, provide_callback/4, forbidden_callback/4]).
-ignore_xref([accept_callback/4, provide_callback/4, forbidden_callback/4]).

-spec forbidden_callback(openapi_api:class(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
    {boolean(), cowboy_req:req(), context()}.
forbidden_callback(_Class, _OperationID, Req, Context) ->
    {false, Req, Context}.

-spec accept_callback(openapi_api:class(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
    {code(), accept_callback_return(), cowboy_req:req(), context()}.
accept_callback(Class, OperationID, Req, Context) ->
    ?LOG_ERROR(#{what => "Got not implemented request to process",
                 class => Class,
                 operation_id => OperationID,
                 request => Req,
                 context => Context}),
    {false, Req, Context}.

-spec provide_callback(openapi_api:class(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
    {code(), response(), cowboy_req:req(), context()}.
provide_callback(Class, OperationID, Req, Context) ->
    ?LOG_ERROR(#{what => "Got not implemented request to process",
                 class => Class,
                 operation_id => OperationID,
                 request => Req,
                 context => Context}),
    {<<>>, Req, Context}.
