-module(erljson).
-author("alexey").

%% API
-export([decode/1]).
-export([encode/1]).

%% @doc 
%%  Decodes binary json into erlang map representation 
%% @end 
-spec(decode(binary()) -> map()). 
decode(Json) ->
  parser:parse(lexer:generate_tokens(Json)).

%% @doc 
%%  Encodes erlang map into binary json. 
%%  Erlang Types | Json types 
%%    binary     ->  String
%%    boolean    ->  boolean
%%    null       ->  null
%%    undefined  ->  null
%%    List       ->  array []
%%    Map        ->  object {}
%% @end  
-spec(encode(map()) -> binary()). 
encode(Map) ->
  encoder:encode(Map).
