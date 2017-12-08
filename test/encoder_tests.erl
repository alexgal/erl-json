-module(encoder_tests).

-include_lib("eunit/include/eunit.hrl").

base_test_empty_map_test() ->
  <<"{}">> = erljson:json_encode(#{}).

base_test_empty_list_test() ->
  <<"[]">> = erljson:json_encode([]).

base_test_single_map_with_binary_keys_test() ->
  Expected = <<"{\"a\":\"b\",\"c\":1,\"d\":0.4556,\"e\":true,\"f\":false,\"g\":null}">>,
  Actual = erljson:json_encode(
          #{
            <<"a">> => <<"b">>,
            <<"c">> => 1,
            <<"d">> => 0.4556,
            <<"e">> => true,
            <<"f">> => false,
            <<"g">> => undefined %resolves to null
          }),
  Expected = Actual.

base_test_map_of_maps_with_binary_keys_test() ->
  Expected = <<"{\"h\":{\"a\":\"b\",\"c\":1,\"d\":0.4556,\"e\":true,\"f\":false,\"g\":null}}">>,
  Actual = erljson:json_encode(
          #{
            <<"h">> =>
              #{
                <<"a">> => <<"b">>,
                <<"c">> => 1,
                <<"d">> => 0.4556,
                <<"e">> => true,
                <<"f">> => false,
                <<"g">> => undefined %resolves to null
              }
          }),
  Expected = Actual.

base_test_list_of_maps_with_binary_keys_test() ->
  Expected = <<"{\"h\":[{\"\a\":\"b\"},{\"a\":\"b1\",\"c\":11,\"d\":1.4556}]}">>,
  Actual = erljson:json_encode(
          #{
            <<"h">> => [
              #{
                <<"a">> => <<"b">>
              },
              #{
                <<"a">> => <<"b1">>,
                <<"c">> => 11,
                <<"d">> => 1.4556
              }
            ]
          }),
  Expected = Actual.
