-module(hello).
-export([hello_name/1, recTest/1, recTest/0]).

-record(foo, {a,b,c=3}).

recTest(#foo{b = B, c = B}) ->
    same;
recTest(#foo{b = B, c = C}) ->
    {B, C}.
recTest() ->
    F = #foo{a = "a"},
    F1 = F#foo{b = "b"},
    recTest(F1).

hello_name(Name) ->
    io:format("hello, ~s\n", Name).
