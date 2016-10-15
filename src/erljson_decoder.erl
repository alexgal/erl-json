-module(erljson_decoder).
-author("alexey").

%% API
-export([decode/1]).

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