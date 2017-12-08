-module(erljson).

-export([json_decode/1]).
-export([json_encode/1]).

json_decode(Json) ->
  parser:parse(lexer:generate_tokens(Json)).

json_encode(Map) ->
  encoder:encode(Map).
