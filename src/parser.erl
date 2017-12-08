-module(parser).

-export([parse/1]).

-include("types.hrl").

% @todo check if map() is a correct type for erlang maps
-spec(parse([json_token(), ...]) -> map() | list()).
% base case
parse(_Tokens = []) ->
  #{};
parse(Tokens = [begin_object| _T ]) ->
  parse(Tokens, #{});
parse(Tokens = [begin_array| _T ]) ->
  {Array, _} = parse_array(Tokens, []),
  Array.

% base case
parse([], Acc) ->
  Acc;
% begin_object
parse(Tokens = [begin_object| _T ], Acc) ->
  {Object, T} = parse_object(Tokens, #{}),
  Acc1 = maps:merge(Acc, Object),
  parse(T, Acc1).
% begin_array
% parse(Tokens = [begin_array| _T ], _Acc) ->
%   {Array, _} = parse_array(Tokens, []),
%   Array.

% empty object case
parse_object([begin_object, end_object | T ], #{}) ->
  {#{}, T};
% start of the object
parse_object([begin_object| T ], #{}) ->
  {Key, T1} = parse_key(T),
  {_Colon, T2}  = parse_colon(T1),
  {Value, T3} = parse_value(T2),
  Acc = #{Key => Value},
  parse_object(T3, Acc);
% end of the object
parse_object([end_object| T ], Acc) ->
  {Acc, T};
% @todo remove duplication in case of comma dn beginning of object
%next element in the object (delimited with comma)
parse_object([comma| T ], Acc) ->
  {Key, T1} = parse_key(T),
  {_Colon, T2}  = parse_colon(T1),
  {Value, T3} = parse_value(T2),
  Acc1 = maps:merge(Acc,#{Key => Value}),
  parse_object(T3, Acc1).

% start of the array
parse_array([begin_array | T], []) ->
  parse_array(T, []);
  % end of the array
parse_array([end_array | T], Acc) ->
  {Acc, T};
% if comma fou d skip to the next value
parse_array([comma | T], Acc) ->
  parse_array(T,Acc);
% parsing value in the array
parse_array(Tokens, Acc) ->
  {Value, T} = parse_value(Tokens),
  Acc1 = Acc ++ [Value],
  parse_array(T, Acc1).

% find colon
parse_colon([colon | T ]) ->
  {colon, T}.

%% find value (multiple cases) %%

% value is string
parse_value([{string, Value} | T ])  ->
  {Value, T};
% value is number
% @todo tranlsate number to appropriate type eg integer of float
parse_value([{number, Value} | T ])  ->
  {parse_number(Value), T};
% value is literal name
parse_value([Value | T ]) when (Value =:= false) orelse
                               (Value =:= true)  orelse
                               (Value =:= null) ->
  {Value, T};
% value is array
parse_value(Tokens = [begin_array | _T ])  ->
  {Array, T} = parse_array(Tokens, []),
  {Array, T};
% value is object
parse_value(Tokens = [begin_object | _T ])  ->
    {Value, T} = parse_object(Tokens, #{}),
    {Value, T}.

% parse key; keys can be binary strings only (?)
parse_key([{string, Key } | T ]) ->
    {Key, T}.

parse_number(Value) ->
  case length(binary:matches(Value, [<<".">>])) of
    0 -> binary_to_integer(Value); % integer
    1 -> binary_to_float(Value);   % float
    _ -> throw({"number is not invalid", Value})
  end.
