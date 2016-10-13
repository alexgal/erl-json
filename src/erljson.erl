-module(erljson).
-author("alexey").

%% API
-export([decode/1]).
-export([encode/1]).

decode(Binary) ->
  erljson_decoder:decode(Binary).

encode(Binary) ->
  erljson_encoder:encode(Binary).
