-module(erljson_decode).
-author("alexey").

%% API
-export([decode/1]).

-include_lib("eunit/include/eunit.hrl").

decode(_Binary = <<>>) ->
  #{};
decode(Binary = <<${, _Tail/binary>>)                     ->
  {Acc, _Remainder} = parse_object(ltrim(Binary), #{}),
  Acc;
decode(Binary = <<$[, _Tail/binary>>)                     ->
  {Acc, _Remainder} = parse_array(ltrim(Binary), []),
  Acc;
decode(<<H, Tail/binary>>)
  when
    (H =:= $\n) orelse
    (H =:= $\r) orelse
    (H =:= $\t) orelse
    (H =:= $\s)                                   ->
  decode(Tail).

parse_object(<<>>, Acc)                           ->
  {Acc, <<>>};
parse_object(<<${, Tail/binary>>, Acc)            ->
  {KV, Remainder} = parse_key_value(ltrim(Tail)),
  Acc1 = maps:merge(Acc, KV),
  parse_object(ltrim(Remainder), Acc1);
parse_object(<<$,, Tail/binary>>, Acc)            ->
  {KV, Remainder} = parse_key_value(ltrim(Tail)),
  Acc1 = maps:merge(Acc, KV),
  parse_object(ltrim(Remainder), Acc1);
parse_object(<<"}">>, Acc) -> %remove?
  {Acc, <<>>};
parse_object(<<$}, Tail/binary>>, Acc)            ->
  {Acc, Tail};
parse_object(Binary, _Acc) ->
  invalid_json(Binary).

parse_key_value(Binary = <<$", _Tail/binary>>)          ->
  {Key, Remainder} = fetch_key(ltrim(Binary)),
  {ok, Remainder1} = fetch_colon(ltrim(Remainder)),
  {Value, Remainder2} = fetch_value(ltrim(Remainder1)),
  {#{Key=>Value}, Remainder2};
parse_key_value(<<$\s, Tail/binary>>)                   ->
  parse_key_value(Tail);
parse_key_value(Binary = <<$}, _Tail/binary>>)          ->
  {#{}, Binary};
parse_key_value(Binary) ->
  invalid_json(Binary).

fetch_key(Binary) ->
  fetch_key(Binary, <<>>, out).

-spec fetch_key(binary(), binary(), atom()) -> {binary(), binary()}.
fetch_key(<<$", Tail/binary>>, Key, out) ->
  fetch_key(Tail, Key, in);
fetch_key(<<$", Tail/binary>>, Key, in)  ->
  {Key, Tail};
fetch_key(<<H, Tail/binary>>, Key, in)   ->
  Key1 = <<Key/binary, H>>,
  fetch_key(Tail, Key1, in).

fetch_colon(<<$:, Tail/binary>>)  ->
  {ok, Tail};
fetch_colon(Binary)               ->
  invalid_json(Binary).

parse_array(<<$}, Tail/binary>>, List)    ->
  {List, Tail};
parse_array(<<$], Tail/binary>>, List)    ->
  {List, Tail};
parse_array(<<$,, Tail/binary>>, List)    ->
  {Value, Remainder} = fetch_value(ltrim(Tail)),
  List1 = List ++ [Value],
  parse_array(ltrim(Remainder), List1);
parse_array(<<$[, Tail/binary>>, List)    ->
  Tail1 = ltrim(Tail),
  case binary:first(Tail1) =:= $] of
    false ->
      {Value, Remainder} = fetch_value(Tail1),
      List1 = List ++ [Value],
      parse_array(ltrim(Remainder), List1);
    true ->
      Tail2 = binary:part(Tail1, {1, (size(Tail1)-1)}),
      {List, Tail2}
  end;
parse_array(Binary, _List)                ->
  invalid_json(Binary).

invalid_json(Binary) ->
  throw({error, invalid_json, Binary}).

%% boolean values
fetch_value(<<"true", Tail/binary>>)  ->
  {true, Tail};
fetch_value(<<"false", Tail/binary>>) ->
  {false, Tail};
%%null value
fetch_value(<<"null", Tail/binary>>)  ->
  {null, Tail};
%%integer value
fetch_value(Binary = <<H, _Tail/binary>>) when (H =:= $-) orelse ((H >= $0) andalso (H =< $9)) ->
  fetch_number(Binary, <<>>, integer);
%%object
fetch_value(Binary = <<${, _Tail/binary>>) ->
  parse_object(ltrim(Binary), #{});
%%array
fetch_value(Binary = <<$[, _Tail/binary>>) ->
  parse_array(ltrim(Binary), []);
fetch_value(Binary = <<$], _Tail/binary>>) ->
  parse_array(ltrim(Binary), []);
%%string value
fetch_value(Binary = <<$", _Tail/binary>>) ->
  fetch_string(Binary, <<>>, out).

fetch_string(<<$\\,$", Tail/binary>>, Acc, out) ->
  fetch_string(Tail, Acc, in);
fetch_string(<<$", Tail/binary>>, Acc, out)     ->
  fetch_string(Tail, Acc, in);
fetch_string(<<$", Tail/binary>>, Acc, in)      ->
  {Acc, Tail};
fetch_string(<<H, Tail/binary>>, Acc, in)       ->
  Acc1 = <<Acc/binary, H>>,
  fetch_string(Tail, Acc1, in);
fetch_string(Binary, _Acc, _)                   ->
  invalid_json(Binary).

-spec fetch_number(binary(), binary(), Type)                                 -> {number(), binary()}
  when
  Type :: integer | float.
fetch_number(<<H, Tail/binary>>, Acc = <<>>, Type) when (H =:= $-)           ->
  Acc1 = <<Acc/binary, H>>,
  fetch_number(Tail, Acc1, Type);
fetch_number(<<H, Tail/binary>>, Acc, _Type) when (H =:= $.)                 ->
  Acc1 = <<Acc/binary, H>>,
  fetch_number(Tail, Acc1, float);
fetch_number(<<H, Tail/binary>>, Acc, Type) when (H >= $0) andalso (H =< $9) ->
  Acc1 = <<Acc/binary, H>>,
  fetch_number(Tail, Acc1, Type);
fetch_number(Binary, Acc, Type)                                              ->
  Number = parse_number(Acc, Type),
  {Number, Binary}.

parse_number(Acc, integer) ->
  binary_to_integer(Acc);
parse_number(Acc, float)   ->
  binary_to_float(Acc).


ltrim(<<$\n, Tail/binary>>) ->
  ltrim(Tail);
ltrim(<<$\s, Tail/binary>>) ->
  ltrim(Tail);
ltrim(<<$\r, Tail/binary>>) ->
  ltrim(Tail);
ltrim(<<$\t, Tail/binary>>) ->
  ltrim(Tail);
ltrim(Binary)               ->
  Binary.

%%Tests

test_linkedin_basic_profile_reply_test_() ->
  ExampleLinkedinBasicProfile =
    <<"{ \"glossary\": { \"title\": \"example glossary\", \"GlossDiv\": { \"title\": \"S\", \"GlossList\": [ { \"ID\": \"SGML\", \"SortAs\": \"SGML\", \"GlossTerm\": \"Standard Generalized Markup Language\", \"Acronym\": \"SGML\", \"Abbrev\": \"ISO 8879:1986\", \"GlossDef\": \"A meta-markup language, used to create markup languages such as DocBook.\", \"GlossSeeAlso\": [\"GML\", \"XML\", \"markup\"] } ] } } }">>,
  Expected =
    #{<<"glossary">> =>
        #{<<"GlossDiv">> =>
          #{<<"GlossList">> =>
            [#{<<"Abbrev">> => <<"ISO 8879:1986">>,
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
  Actual = decode(ExampleLinkedinBasicProfile),
  ?_assert(Expected =:= Actual).

test_rfc_case2_test_() ->
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

test_rfc_case1_test_() ->
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

test_non_empty_root_array_test_() ->
  Expected = [123, 456, #{}, []],
  Actual = decode(<<"[123, 456, {}, []]">>),
  ?_assert(Expected =:= Actual).

test_empty_root_array_test_() ->
  Expected = [],
  Actual = decode(<<"[]">>),
  ?_assert(Expected =:= Actual).

test_negative_float_value_test_() ->
  Expected = #{<<"key1">>=>-348.4850},
  Actual = decode(<<"{ \"key1\" : -348.4850 }">>),
  ?_assert(Expected =:= Actual).

test_float_value_test_() ->
  Expected = #{<<"key1">>=>348.4850},
  Actual = decode(<<"{ \"key1\" : 348.4850 }">>),
  ?_assert(Expected =:= Actual).

test_integer_value_test_() ->
  Expected = #{<<"key1">>=>3484850},
  Actual = decode(<<"{ \"key1\" : 3484850 }">>),
  ?_assert(Expected =:= Actual).

test_non_empty_array_in_object_test_() ->
  Expected = #{<<"key1">>=>true, <<"key2">>=>[true, false, <<"str">>, null, #{}]},
  Actual = decode(<<"{ \"key1\" : true, \"key2\" : [true, false, \"str\", null, {}]  }">>),
  ?_assert(Expected =:= Actual).

test_empty_array_in_object_test_() ->
  Expected = #{<<"key1">>=>true, <<"key2">>=>[]},
  Actual = decode(<<"{ \"key1\" : true, \"key2\" : []  }">>),
  ?_assert(Expected =:= Actual).

test_recurse_object_test_() ->
  Expected = #{<<"key1">>=>#{<<"key2">>=>null, <<"key3">>=>true, <<"key4">>=><<"I'm a string">>}},
  Actual = decode(<<"{ \"key1\" : { \"key2\":null, \"key3\":true, \"key4\":\"I'm a string\"      } }">>),
  ?_assert(Expected =:= Actual).

test_empty_object_test_() ->
  Expected = #{<<"key1">>=>#{}},
  Actual = decode(<<"{ \"key1\" : {} }">>),
  ?_assert(Expected =:= Actual).

test_string_values_test_() ->
  Expected = #{<<"key1">>=><<"Helo amigos!">>},
  Actual = decode(<<"{ \"key1\" : \"Helo amigos!\"}">>),
  ?_assert(Expected =:= Actual).

test_null_values_test_() ->
  Expected = #{<<"key1">>=>null},
  Actual = decode(<<"{ \"key1\" : null}">>),
  ?_assert(Expected =:= Actual).

test_boolean_values_test_() ->
  Expected = #{<<"key1">>=>true, <<"key2">>=>false},
  Actual = decode(<<"{ \"key1\" : true, \"key2\" : false  }">>),
  ?_assert(Expected =:= Actual).

test_empty_case_test_() ->
  ?_assert(#{} =:= decode(<<>>)).
