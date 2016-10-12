-module(erljson_encoder).
-author("alexey").

%% API
-export([encode/1]).

-include_lib("eunit/include/eunit.hrl").

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

test_can_encode_list_test_() ->
  ?_assert(<<"[{\"key\":\"value\"}]">> =:= encode([#{<<"key">>=><<"value">>}])).

test_can_encode_empty_map_test_() ->
  ?_assert(<<"{}">> =:= encode(#{})).

test_can_encode_binary_pair_test_() ->
  ?_assert(<<"{\"key\":\"value\"}">> =:= encode(#{<<"key">> => <<"value">>})).

test_can_encode_binary_pairs_test_() ->
  ?_assert(<<"{\"key\":\"value\",\"key1\":\"value1\"}">> =:= encode(#{<<"key">> => <<"value">>, <<"key1">> => <<"value1">>})).

test_can_encode_integer_test_() ->
  ?_assert(<<"{\"key\":123456}">> =:= encode(#{<<"key">> => 123456})).

test_can_encode_float_test_() ->
  ?_assert(<<"{\"key\":12.3456}">> =:= encode(#{<<"key">> => 12.3456})).

test_can_encode_boolean_test_() ->
  ?_assert(<<"{\"key\":true,\"key1\":false}">> =:= encode(#{<<"key">> => true, <<"key1">> => false})).

test_can_encode_null_test_() ->
  ?_assert(<<"{\"key\":null}">> =:= encode(#{<<"key">> => null})).

test_can_encode_recurse_object_test_() ->
  ?_assert(<<"{\"key\":{\"key1\":\"value1\"}}">> =:= encode(#{<<"key">> => #{<<"key1">>=><<"value1">>}})).

test_can_encode_recurse_array_value_test_() ->
  ?_assert(<<"{\"key\":[{\"key1\":\"value1\"}]}">> =:= encode(#{<<"key">> => [#{<<"key1">>=><<"value1">>}]})).

test_can_encode_recurse_array_root_test_() ->
  ?_assert(<<"[{\"key\":[{\"key1\":\"value1\"}]}]">> =:= encode([#{<<"key">> => [#{<<"key1">>=><<"value1">>}]}])).