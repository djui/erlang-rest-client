-module(rest_client).

-compile({no_auto_import, [ error/1 ]}).

%%%_* Exports ==========================================================
-export([ request/1
        , request/2
        , request/3
        ]).

%%%_* Types ============================================================
-type method() :: options
                | get
                | head
                | post
                | put
                | delete
                | trace
                | connect.
-type kvlist() :: [{_, _}].

%%%_* Defines ==========================================================
-define(SERVER_TIMEOUT, 30000).
%%%_* Code =============================================================
%%%_* API --------------------------------------------------------------
%% @doc Sends a GET request to an URI.
-spec request(Path::string()) -> kvlist() | {error, _}.

request(Path) ->
  request(get, Path).

%% @doc Sends a request to an URI with the given method.
-spec request(Method::method(), Path::string()) -> kvlist() | {error, _}.

request(Method, Path) ->
  Request  = {Path, []},
  Response = send_request(Method, Request),
  parse_response(Response).

%% @doc Sends a request to an URI with the given method and data for a payload.
-spec request(Method::method(), Path::string(), Data::kvlist()) ->
                 kvlist() | {error, _}.

request(Method, Path, Data) ->
  Payload  = percent:url_encode(Data),
  Request  = {Path, [], "application/x-www-form-urlencoded", Payload},
  Response = send_request(Method, Request),
  parse_response(Response).

%%%_* Internals --------------------------------------------------------
send_request(Method, {Url, []}) ->
  httpc:request(Method, {Url, []}, [{timeout, ?SERVER_TIMEOUT}], []).
 
parse_response({ok, {{"HTTP/1.1", 200, "OK"}, Headers, Body}}) ->
  case kf("content-type", Headers) of
    "application/xml" ->
      {ok, Payload} = erlsom:simple_form(Body),
      {ok, Payload};
    "application/json" ->
      Payload = mochijson2:decode(Body, [{format, proplist}]),
      {ok, Payload};
    _ ->
      error("unsupported content-type")
  end;
parse_response({ok, {{"HTTP/1.1", 404, "Not Found"}, Headers, Body}}) ->
  case kf("content-type", Headers) of
    "application/json" ->
      Payload = mochijson2:decode(Body, [{format, proplist}]),
      Message = kf(b("message"), Payload),
      error(Message);
    _ ->
      error("invalid method")
  end;
parse_response({error, _Reason}=Error) -> error(Error).

error({error, Error}) -> erlang:throw(Error);
error(Error)          -> error({error, Error}).

%%%_* Helpers ----------------------------------------------------------
kf(Key, List) ->
  {Key, Value} = lists:keyfind(Key, 1, List),
  Value.

b(B) when is_binary(B) -> unicode:characters_to_list(B);
b(S) when is_list(S)   -> unicode:characters_to_binary(S).

%%% Mode: Erlang
%%% End.
