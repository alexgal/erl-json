-module(decoder_tests).
-author("alexey").

-include_lib("eunit/include/eunit.hrl").

-import(erljson, [decode/1]).

json_input_example__test() ->
  Json =
    <<"{ \"glossary\": { \"title\": \"example glossary\", \"GlossDiv\": { \"title\": \"S\", \"GlossList\": [ { \"ID\": \"SGML\", \"SortAs\": \"SGML\", \"GlossTerm\": \"Standard Generalized Markup Language\", \"Acronym\": \"SGML\", \"Abbrev\": \"ISO 8879:1986\", \"GlossDef\": \"A meta-markup language, used to create markup languages such as DocBook.\", \"GlossSeeAlso\": [\"GML\", \"XML\", \"markup\"] } ] } } }">>,
  Expected =
    #{<<"glossary">> =>
      #{<<"GlossDiv">> =>
        #{<<"GlossList">> =>
          [#{ <<"Abbrev">> => <<"ISO 8879:1986">>,
              <<"Acronym">> => <<"SGML">>,
              <<"GlossDef">> => <<"A meta-markup language, used to create markup languages such as DocBook.">>,
              <<"GlossSeeAlso">> => [<<"GML">>,<<"XML">>,<<"markup">>],
              <<"GlossTerm">> => <<"Standard Generalized Markup Language">>,
              <<"ID">> => <<"SGML">>,
              <<"SortAs">> => <<"SGML">>
          }],
          <<"title">> => <<"S">>
        },
        <<"title">> => <<"example glossary">>
      }
    },
  Actual = decode(Json),
  ?_assert(Expected =:= Actual).

rfc_case2_test() ->
  ExampleRfcCase2 =
    <<"     [
        {
           \"precision\": \"zip\",
           \"Latitude\":  37.7668,
           \"Longitude\": -122.3959,
           \"Address\":   \"\",
           \"City\":      \"SAN FRANCISCO\",
           \"State\":     \"CA\",
           \"Zip\":       \"94107\",
           \"Country\":   \"US\"
        },
        {
           \"precision\": \"zip\",
           \"Latitude\":  37.371991,
           \"Longitude\": -122.026020,
           \"Address\":   \"\",
           \"City\":      \"SUNNYVALE\",
           \"State\":     \"CA\",
           \"Zip\":       \"94085\",
           \"Country\":   \"US\"
        }
      ]">>,
  Expected = [#{<<"Address">> => <<>>,
    <<"City">> => <<"SAN FRANCISCO">>,
    <<"Country">> => <<"US">>,
    <<"Latitude">> => 37.7668,
    <<"Longitude">> => -122.3959,
    <<"State">> => <<"CA">>,
    <<"Zip">> => <<"94107">>,
    <<"precision">> => <<"zip">>},
    #{<<"Address">> => <<>>,
      <<"City">> => <<"SUNNYVALE">>,
      <<"Country">> => <<"US">>,
      <<"Latitude">> => 37.371991,
      <<"Longitude">> => -122.02602,
      <<"State">> => <<"CA">>,
      <<"Zip">> => <<"94085">>,
      <<"precision">> => <<"zip">>}],
  Actual = decode(ExampleRfcCase2),
  ?_assert(Expected =:= Actual).

rfc_case1_test() ->
  ExampleRfcCase1 =
    <<"{
      \"Image\": {
        \"Width\":  800,
        \"Height\": 600,
        \"Title\":  \"View from 15th Floor\",
        \"Thumbnail\": {
          \"Url\":    \"http://www.example.com/image/481989943\",
          \"Height\": 125,
          \"Width\":  100
        },
        \"Animated\" : false,
        \"IDs\": [116, 943, 234, 38793]
      }
    }">>,
  Expected = #{<<"Image">> => #{<<"Animated">> => false,
    <<"Height">> => 600,
    <<"IDs">> => [116, 943, 234, 38793],
    <<"Thumbnail">> => #{<<"Height">> => 125,
      <<"Url">> => <<"http://www.example.com/image/481989943">>,
      <<"Width">> => 100},
    <<"Title">> => <<"View from 15th Floor">>,
    <<"Width">> => 800}},
  Actual = decode(ExampleRfcCase1),
  ?_assert(Actual =:= Expected).

non_empty_root_array_test() ->
  Expected = [123, 456, #{}, []],
  Actual = decode(<<"[123, 456, {}, []]">>),
  ?_assert(Expected =:= Actual).

empty_root_array_test() ->
  Expected = [],
  Actual = decode(<<"[]">>),
  ?_assert(Expected =:= Actual).

negative_float_value_test() ->
  Expected = #{<<"key1">>=>-348.4850},
  Actual = decode(<<"{ \"key1\" : -348.4850 }">>),
  ?_assert(Expected =:= Actual).

float_value_test() ->
  Expected = #{<<"key1">>=>348.4850},
  Actual = decode(<<"{ \"key1\" : 348.4850 }">>),
  ?_assert(Expected =:= Actual).

integer_value_test() ->
  Expected = #{<<"key1">>=>3484850},
  Actual = decode(<<"{ \"key1\" : 3484850 }">>),
  ?_assert(Expected =:= Actual).

non_empty_array_in_object_test() ->
  Expected = #{<<"key1">>=>true, <<"key2">>=>[true, false, <<"str">>, null, #{}]},
  Actual = decode(<<"{ \"key1\" : true, \"key2\" : [true, false, \"str\", null, {}]  }">>),
  ?_assert(Expected =:= Actual).

empty_array_in_object_test() ->
  Expected = #{<<"key1">>=>true, <<"key2">>=>[]},
  Actual = decode(<<"{ \"key1\" : true, \"key2\" : []  }">>),
  ?_assert(Expected =:= Actual).

recurse_object_test() ->
  Expected = #{<<"key1">>=>#{<<"key2">>=>null, <<"key3">>=>true, <<"key4">>=><<"I'm a string">>}},
  Actual = decode(<<"{ \"key1\" : { \"key2\":null, \"key3\":true, \"key4\":\"I'm a string\"      } }">>),
  ?_assert(Expected =:= Actual).

empty_object_test() ->
  Expected = #{<<"key1">>=>#{}},
  Actual = decode(<<"{ \"key1\" : {} }">>),
  ?_assert(Expected =:= Actual).

string_values_test() ->
  Expected = #{<<"key1">>=><<"Helo amigos!">>},
  Actual = decode(<<"{ \"key1\" : \"Helo amigos!\"}">>),
  ?_assert(Expected =:= Actual).

null_values_test() ->
  Expected = #{<<"key1">>=>null},
  Actual = decode(<<"{ \"key1\" : null}">>),
  ?_assert(Expected =:= Actual).

boolean_values_test() ->
  Expected = #{<<"key1">>=>true, <<"key2">>=>false},
  Actual = decode(<<"{ \"key1\" : true, \"key2\" : false  }">>),
  ?_assert(Expected =:= Actual).

empty_case_test() ->
  ?_assert(#{} =:= decode(<<>>)).

unicode_support_values_test() ->
  Expected = #{<<"key1">>=><<"юникод"/utf8>>},
  Actual = decode(<<"{ \"key1\" : \"юникод\"}">>),
  ?_assert(Expected =:= Actual).

