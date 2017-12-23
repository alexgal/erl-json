-module(lexer).

-export([generate_tokens/1]).

-include("types.hrl").

-spec(generate_tokens(binary()) -> [json_token(), ...]).
generate_tokens(Json) ->
  generate_tokens(Json, []).

% base case
generate_tokens(<<>>, Acc) ->
  Acc;
% begin_object
generate_tokens(<<"{", T/binary>>, Acc) ->
  Acc1 = Acc ++ [begin_object],
  generate_tokens(T, Acc1);
% ignore white space
generate_tokens(<<H, T/binary>>, Acc) when (H =:= $\n) orelse
                                           (H =:= $\r) orelse
                                           (H =:= $\t) orelse
                                           (H =:= $\s) ->
  generate_tokens(T, Acc);
% generate string token
generate_tokens(Json = <<$", _T/binary>>, Acc) ->
  {String, T} = fetch_string(Json),
  Acc1 = Acc ++ [{string, String}],
  generate_tokens(T, Acc1);
% colon
generate_tokens(<<$:, T/binary>>, Acc) ->
  Acc1 = Acc ++ [colon],
  generate_tokens(T, Acc1);
% end_object
generate_tokens(<<"}", T/binary>>, Acc) ->
  Acc1 = Acc ++ [end_object],
  generate_tokens(T, Acc1);
% comma
generate_tokens(<<",", T/binary>>, Acc) ->
  Acc1 = Acc ++ [comma],
  generate_tokens(T, Acc1);
% literal_names
generate_tokens(<<"false", T/binary>>, Acc) ->
  Acc1 = Acc ++ [false],
  generate_tokens(T, Acc1);
generate_tokens(<<"true", T/binary>>, Acc) ->
    Acc1 = Acc ++ [true],
    generate_tokens(T, Acc1);
generate_tokens(<<"null", T/binary>>, Acc) ->
    Acc1 = Acc ++ [null],
    generate_tokens(T, Acc1);
% begin_array
generate_tokens(<<"[", T/binary>>, Acc) ->
  Acc1 = Acc ++ [begin_array],
  generate_tokens(T, Acc1);
% end_array
generate_tokens(<<"]", T/binary>>, Acc) ->
    Acc1 = Acc ++ [end_array],
    generate_tokens(T, Acc1);

% @todo handle negative number cases
% @todo andle floating number cases(leaging dot?)
% numbers
generate_tokens(Json = <<H, _T/binary>>, Acc) when
                                      (H >= $0 andalso H =< $9) orelse
                                      (H =:= $-) ->
  {Number, T} = fetch_number(Json),
  Acc1 = Acc ++ [{number, Number}],
  generate_tokens(T, Acc1).

fetch_number(Json) ->
  fetch_number(Json, {out, <<>>}).

fetch_number(<<H, T/binary>>, {out, <<>>}) when
                                      (H >= $0 andalso H =< $9) orelse
                                      (H =:= $-) ->
  fetch_number(T, {in, <<H>>});
% handle leading minus @todo move minus to the case above
fetch_number(<<$-, T/binary>>, {out, <<>>})  ->
  Acc1 = <<"-">>,
  fetch_number(T, {in, Acc1});
% middle of the number(multiple cases). Numbers and single dot are allowed
fetch_number(<<H, T/binary>>, {State, Acc}) when (H >= $0 andalso H =< $9) andalso
                                                 (State =:= in orelse State =:= dot) ->
  Acc1 = <<Acc/binary, H>>,
  fetch_number(T, {in, Acc1});
% handle . for floating numbers
fetch_number(<<$., T/binary>>, {in, Acc})  ->
  Acc1 = <<Acc/binary, ".">>,
  fetch_number(T, {dot, Acc1});
% first and only allowed case with the dot
fetch_number(<<H, T/binary>>, {dot, Acc}) when (H >= $0 andalso H =< $9) ->
  Acc1 = <<Acc/binary, H>>,
  fetch_number(T, {dot, Acc1});
% end of the number. comma or end_object or end_array are expected
fetch_number(Json = <<H, _T/binary>>, {State, Acc}) when
                                             (H =:= $,)  orelse % comma
                                             (H =:= $])  orelse % end of list
                                             (H =:= $})  orelse % end of object
                                             (H =:= $\n) orelse % or whitespace
                                             (H =:= $\r) orelse
                                             (H =:= $\t) orelse
                                             (H =:= $\s)
                                                         andalso % should be in (integer) of dot(floating point)
                                             (State =:= in orelse State =:= dot) ->
   {Acc, Json};
% corner case when only number is provided which is considered valid json
fetch_number(<<>>, {in, Acc}) ->
  {Acc, <<>>}.
% ignore trailing spaces

% fetches string between first and next quote(") character
% return fetched string and remainder of input
-spec(fetch_string(binary()) -> {binary(), binary()}).
fetch_string(Json) ->
  fetch_string(Json, {out, <<>>}).
% base case
fetch_string(<<>>, {_,Acc}) ->
  {Acc, <<>>};
% start of the string
fetch_string(<<$", T/binary>>, {out, <<>>}) ->
  fetch_string(T, {in, <<>>});
% end of the string
fetch_string(<<$", T/binary>>, {in, Acc}) ->
  {Acc, T};
% fetch string contents
fetch_string(<<H, T/binary>>, {in, Acc}) ->
  Acc1 = <<Acc/binary, H>>,
  fetch_string(T, {in, Acc1}).