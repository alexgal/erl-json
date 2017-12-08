% structural characters types

-type begin_array() :: begin_array. % '['.
-type end_array() :: ']'.

-type begin_object() :: begin_object. % '{'
-type end_object() :: end_object. %'}'

-type name_separator() :: colon. %, ':'.
-type value_separator() :: comma. % ','


-type structural_character() :: begin_array() | end_array()  |
                                begin_object() | end_object() |
                                name_separator() | value_separator() .

% json values types

-type json_string() :: binary().
-type object() :: term().
-type array()  :: term().
-type literal_names() :: true | false | null.

-type value() :: number() | json_string() | object() | array() | literal_names().

-type json_token() :: structural_character() | value().
