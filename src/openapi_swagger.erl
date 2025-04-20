-module(openapi_swagger).
-moduledoc """
JSON Swagger handler
Updates the generated opanapi json schema
""".

-behaviour(cowboy_rest).

%% Cowboy REST callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([valid_content_headers/2]).
-export([handle_type_provided/2]).

-type swagger_json_handler() ::
    fun((openapi_api:swagger_json()) -> openapi_api:swagger_json()) | undefined.
-export_type([swagger_json_handler/0]).

-type init_opts() ::#{swagger_json_handler => swagger_json_handler()}.
-export_type([init_opts/0]).

-record(state, {swagger_json_handler :: swagger_json_handler()}).

-type state() :: #state{}.

-spec init(cowboy_req:req(), init_opts()) ->
    {cowboy_rest, cowboy_req:req(), state()}.
init(Req, Opts) ->
    SwaggerHandler = maps:get(swagger_json_handler, Opts, undefined),
    State = #state{swagger_json_handler = SwaggerHandler},
    {cowboy_rest, Req, State}.

-spec allowed_methods(cowboy_req:req(), state()) ->
    {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, State) ->
    {[], Req, State}.

-spec valid_content_headers(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
valid_content_headers(Req, State) ->
    {true, Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[{binary() | {binary(), binary(), '*' | [{binary(), binary()}]}, atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, handle_type_provided}
    ], Req, State}.

-spec handle_type_provided(cowboy_req:req(), state()) ->
    {stop , cowboy_req:req(), state()}.
handle_type_provided(Req, #state{swagger_json_handler = Handler} = State) ->
    Body =
        case Handler of
            undefined -> openapi_api:swagger_schema();
            H -> H(openapi_api:swagger_schema())
        end,

    Req2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req),
    Res3 = cowboy_req:set_resp_body(json:encode(Body), Req2),
    ReqR = cowboy_req:reply(200, Res3),
    {stop, ReqR, State}.
