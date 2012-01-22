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
  Payload = create_query(Data),
  Request  = {Path, [], "application/x-www-form-urlencoded", Payload},
  Response = send_request(Method, Request),
  parse_response(Response).

%%%_* Internals --------------------------------------------------------
create_query(Data) ->
  Fun = fun({K,V}) -> percent:url_encode(K)++"="++ percent:url_encode(V) end,
  string:join(lists:map(Fun, Data), "&").

send_request(Method, Request) ->
  httpc:request(Method, Request, [{timeout, ?SERVER_TIMEOUT}], []).
 
parse_response({ok, {{"HTTP/1.1", 200, "OK"}, Headers, Body}}) ->
  case kf("content-type", Headers) of
    "application/xml" ->
      {ok, _Payload} = erlsom:simple_form(Body);
    "application/json" ->
      Payload = mochijson2:decode(Body, [{format, proplist}]),
      {ok, Payload};
    _ ->
      error("Unsupported content-type")
  end;
parse_response({ok, {{"HTTP/1.1", 201, "Created"}, Headers, Body}}) ->
  case kf("content-type", Headers) of
    "application/xml" ->
      {ok, _Payload} = erlsom:simple_form(Body);
    "application/json" ->
      Payload = mochijson2:decode(Body, [{format, proplist}]),
      {ok, Payload};
    _ ->
      error("Unsupported content-type")
  end;
parse_response({ok, {{"HTTP/1.1", 204, "No Content"}, _Headers, _Body}}) ->
  ok;
parse_response({ok, {{"HTTP/1.1", 404, "Not Found"}, Headers, Body}}) ->
  case kf("content-type", Headers) of
    "application/json" ->
      Payload = mochijson2:decode(Body, [{format, proplist}]),
      Message = kf(b("message"), Payload, "No error message provided"),
      error(Message);
    _ ->
      error("Unknown REST call")
  end;
parse_response({ok, {{"HTTP/1.1", ErrCode, ErrText}, _Headers, _Body}}) ->
  error(fmt("Unknown response method: ~p ~s", [ErrCode, ErrText]));
parse_response({ok, {{HTTP, _ErrCode, _ErrText}, _Headers, _Body}}) ->
  error("Invalid HTTP version: " ++ HTTP);
parse_response({error, _Reason}=Error) -> error(Error).

error({error, _}=Error) -> Error;
error(Error)            -> error({error, Error}).

%%%_* Helpers ----------------------------------------------------------
kf(Key, List) -> kf(Key, List, undefined).

kf(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    {Key, Value} -> Value;
    false        -> Default
  end.

b(B) when is_binary(B) -> unicode:characters_to_list(B);
b(S) when is_list(S)   -> unicode:characters_to_binary(S).

fmt(Format, Data) -> lists:flatten(io_lib:format(Format, Data)).

%%% Mode: Erlang
%%% End.
