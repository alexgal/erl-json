% -------------------------------------------------------
% Tests based on examples from json rfc https://tools.ietf.org/html/rfc7159#page-11
%
%
% -------------------------------------------------------
-module(decoder_rfc_tests).

-include_lib("eunit/include/eunit.hrl").

% test1_json_object() ->

% test2_list_containintg_json_objects() ->

test3_valid_json_string_test() ->
  "Hello world!".

test4_valid_json_number_test() ->
42.

test5_valid_json_boolean_test() ->
true.
