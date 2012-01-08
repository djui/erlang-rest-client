-module(percent).

-export([url_encode/1, url_decode/1, uri_encode/1, uri_decode/1]).

-define(is_alphanum(C), C >= $A, C =< $Z; C >= $a, C =< $z; C >= $0, C =< $9).

%%
%% Percent encoding/decoding as defined by the application/x-www-form-urlencoded
%% content type (http://www.w3.org/TR/html4/interact/forms.html#h-17.13.4.1).
%%

url_encode(Str) when is_list(Str) ->
  url_encode(lists:reverse(Str, []), []).

url_encode([X | T], Acc) when ?is_alphanum(X); X =:= $-; X =:= $_; X =:= $. ->
  url_encode(T, [X | Acc]);
url_encode([32 | T], Acc) ->
  url_encode(T, [$+ | Acc]);
url_encode([X | T], Acc) ->
  NewAcc = [$%, hexchr_encode(X bsr 4), hexchr_encode(X band 16#0f) | Acc],
  url_encode(T, NewAcc);
url_encode([], Acc) ->
  Acc.

url_decode(Str) when is_list(Str) ->
  url_decode(Str, []).

url_decode([$+ | T], Acc) ->
  url_decode(T, [32 | Acc]);
url_decode([$%, A, B | T], Acc) ->
  Char = (hexchr_decode(A) bsl 4) + hexchr_decode(B),
  url_decode(T, [Char | Acc]);
url_decode([X | T], Acc) ->
  url_decode(T, [X | Acc]);
url_decode([], Acc) ->
  lists:reverse(Acc, []).

%%
%% Percent encoding/decoding as defined by RFC 3986 (http://tools.ietf.org/html/rfc3986).
%%

uri_encode(Str) when is_list(Str) ->
  uri_encode(lists:reverse(Str, []), []).

uri_encode([X | T], Acc) when ?is_alphanum(X); X =:= $-; X =:= $_; X =:= $.; X =:= $~ ->
  uri_encode(T, [X | Acc]);
uri_encode([X | T], Acc) ->
  NewAcc = [$%, hexchr_encode(X bsr 4), hexchr_encode(X band 16#0f) | Acc],
  uri_encode(T, NewAcc);
uri_encode([], Acc) ->
  Acc.

uri_decode(Str) when is_list(Str) ->
  uri_decode(Str, []).

uri_decode([$%, A, B | T], Acc) ->
  Char = (hexchr_decode(A) bsl 4) + hexchr_decode(B),
  uri_decode(T, [Char | Acc]);
uri_decode([X | T], Acc) ->
  uri_decode(T, [X | Acc]);
uri_decode([], Acc) ->
  lists:reverse(Acc, []).

%%
%% Helper functions.
%%

-compile({inline, [{hexchr_encode, 1}, {hexchr_decode, 1}]}).

hexchr_encode(N) when N >= 10 andalso N < 16 ->
  N + $A - 10;
hexchr_encode(N) when N >= 0 andalso N < 10 ->
  N + $0.

hexchr_decode(C) when C >= $a andalso C =< $f ->
  C - $a + 10;
hexchr_decode(C) when C >= $A andalso C =< $F ->
  C - $A + 10;
hexchr_decode(C) when C >= $0 andalso C =< $9 ->
  C - $0.
