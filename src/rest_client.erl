-module(rest_client).

%%%_* Exports ==========================================================
-export([ request/1
        , request/2
        , request/3
        ]).

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
      {ok, Payload} = erlsom:simple_form(Xml),
      {ok, PAyload};
    "application/json" ->
      Payload = mochijson2:decode(Body, [{format, proplist}]),
      {ok, Payload};
    _ ->
      parse_response({error, unsupported_content_type})
  end;
parse_response({ok, {{"HTTP/1.1", 404, "Not Found"}, Headers, Body}}) ->
  case kf("content-type", Headers) of
    "application/json" ->
      Payload = mochijson2:decode(Body, [{format, proplist}]),
      Message = kf(b("message"), Payload),
      {error, Message};
    _ ->
      parse_response({error, invalid_method})
  end;
parse_response({error, _Reason}=Error) -> Error.

%%%_* Helpers ----------------------------------------------------------
kf(Key, List) ->
  {Key, Value} = lists:keyfind(Key, 1, List),
  Value.

b(B) when is_binary(B) -> unicode:characters_to_list(B);
b(S) when is_list(S)   -> unicode:characters_to_binary(S).

%% Can't use code:priv_dir/1 as the application is usually not loaded.
priv_file(Filename) ->
  filename:join([ filename:dirname(code:which(?MODULE)), "../priv", Filename]).

%%% Mode: Erlang
%%% End.
