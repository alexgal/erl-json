-module(ut8_test).

-include_lib("eunit/include/eunit.hrl").


 reverse_nil_test() -> [] = lists:reverse([]).

% -1 lookup for test folder,
% 0 create ebin folder inside test folder(if needed)
% 1 move beam to new ebin folder
% 2 run given test
%
