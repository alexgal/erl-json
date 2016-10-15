-module(erljson_encoder).
-author("alexey").

%% API
-export([encode/1]).

encode([])                           ->
  <<"[]">>;
encode(List) when is_list(List)      ->
  value(List);
encode(Map) when map_size(Map) =:= 0 ->
  <<"{}">>;
encode(Map)                          ->
  Pairs = maps:fold(fun glue_key_value/3, <<>>, Map),
  <<"{", Pairs/binary, "}">>.

value(Value) when is_binary(Value)                            ->
  <<"\"", Value/binary, "\"">>;
value(Value) when is_integer(Value)                           ->
  integer_to_binary(Value);
value(Value) when is_float(Value)                             ->
  list_to_binary(io_lib:print(Value));
value(Value) when is_boolean(Value) orelse (Value =:= null)   ->
  atom_to_binary(Value, latin1);
value(Value) when is_map(Value)                               ->
  encode(Value);
value(Value) when is_list(Value)                              ->
  Binary = list_to_binary(lists:map(fun value/1, Value)),
  <<"[",Binary/binary, "]">>.

glue_key_value(Key, Value, AccIn)                             ->
  glue_with_acc_in(Key, value(Value), AccIn).

glue_with_acc_in(Key, Value, <<>>)                            ->
  <<"\"",Key/binary,"\"", ":", Value/binary>>;
glue_with_acc_in(Key, Value, AccIn)                           ->
  <<AccIn/binary, ",\"",Key/binary,"\"", ":", Value/binary>>.