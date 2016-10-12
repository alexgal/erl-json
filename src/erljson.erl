-module(erljson).
-author("alexey").

%% API
-export([decode/1]).
-export([encode/1]).

decode(Binary) ->
  json_decoder:decode(Binary).

encode(Binary) ->
  json_encoder:encode(Binary).
