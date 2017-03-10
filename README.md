Erljson - Erlang json parser 
============================

A JSON parser from json (binary) to erlang maps.
This is a pretty simple implementation of [https://tools.ietf.org/html/rfc7159](https://tools.ietf.org/html/rfc7159)


Usage
-----

Erljson has a very simple api. For example `decode/1` accepts binary and returns map in case valid json binary provided. 
On other hand `encode/1` accepts map and returns json binary.

`erljson:decode`
------------------

* `Eshell V8.0.2  (abort with ^G)`
* `1> erljson:decode(<<"{\"key1\": \"value\", \"key2\": true, \"key3\": []}">>).`
* ` ``#{<<"key1">> => <<"value">>,<<"key2">> => true,<<"key3">> => []}`

note on decode
---
In case invalid json provided decode throws exception containing following tuple : `throw({error, invalid_json, Binary})`.
Where Binary is the one which was passed as an argument to decode. 


`erljson:encode`
------------------

* `Eshell V8.0.2  (abort with ^G)`
* `1> erljson:encode(#{<<"key1">> => <<"value">>,<<"key2">> => true,<<"key3">> => []}).`
* `<<"{\"key1\":\"value\",\"key2\":true,\"key3\":[]}">>`

Todo 
----

* At this moment an implementation does not set explicit limits on :
  * the size of texts that it accepts;
  * the maximum depth of nesting;  
  * the range and precision of numbers;
  * the length and character contents of strings.
* Performance wasn't checked and considered yet.
* Explicit support for UTF-8 characters wasn't tested.
