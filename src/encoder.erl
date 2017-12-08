-module(encoder).

-export([encode/1]).

encode(Map) when is_map(Map)->
  Binary = maps:fold(fun encode/3, <<"">>, Map),
  Binary1 = skip_trailing_comma(Binary),
  <<"{", Binary1/binary, "}">>;
encode(List = []) when is_list(List)->
  <<"[]">>;
encode(List) when is_list(List)->
  F = fun(V, AccIn) -> V1 = encode_value(V), <<AccIn/binary, V1/binary, "," >> end,
  BinaryList = lists:foldl(F ,<<>>, List),
  Binary1 = skip_trailing_comma(BinaryList),
  <<"[", Binary1/binary, "]">>.

%%==============================================================================
%% Internal functions
%%==============================================================================
encode(K, V, AccIn) when is_atom(K)->
  K1 = atom_to_binary(K, utf8),
  AccIn1 = encode(K1, V, AccIn),
  AccIn1;
encode(K, V, AccIn) -> % when is_integer(V) ->
  V1 = encode_value(V),
  AccIn1 = <<AccIn/binary, "\"" ,K/binary, "\":", V1/binary, "," >>,
  AccIn1.

encode_value(V) when is_integer(V) ->
  integer_to_binary(V);
encode_value(V) when is_float(V) ->
  float_to_binary(V,[{decimals, 9}, compact]);
encode_value(V) when is_boolean(V) ->
  atom_to_binary(V, latin1);
encode_value(undefined) ->
  <<"null">>;
encode_value(V) when is_map(V) orelse is_list(V) ->
  encode(V);
encode_value(V) when is_atom(V) ->
  V1 = atom_to_binary(V, utf8),
  encode_value(V1);
encode_value(V) when is_binary(V) ->
  <<$",V/binary, $">>.

skip_trailing_comma(<<>>) ->
  <<>>;
skip_trailing_comma(Binary) ->
  case binary:last(Binary) =:= $, of
    true ->
      binary_part(Binary, {0, byte_size(Binary)-1});
    false ->
      Binary
  end.
