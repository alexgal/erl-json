-module(encoder_tests).
-author("alexey").

-include_lib("eunit/include/eunit.hrl").

-import(erljson, [encode/1]).

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