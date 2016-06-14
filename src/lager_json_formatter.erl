-module(lager_json_formatter).

-include_lib("lager/include/lager.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([format/2]).

-spec format(lager_msg:lager_msg(),list()) -> any().
format(Message, _Config) ->
  {D, T} = lager_msg:datetime(Message),
  Metadata = lager_msg:metadata(Message),
  Map = #{
    date => iolist_to_binary([D, " ", T]),
    severity => lager_msg:severity(Message),
    message => iolist_to_binary(lager_msg:message(Message))
  },
  Map2 = maps:merge(Map, maps:from_list(lists:map(fun process_meta/1, Metadata))),
  << (jsx:encode(Map2))/binary, "\n" >>.

process_meta({Key, Value}) when is_binary(Value) ->
  {Key, Value};
process_meta({Key, Value}) when is_list(Value) ->
  {Key, iolist_to_binary(Value)};
process_meta({Key, Value}) ->
  {Key, iolist_to_binary(io_lib:format("~p", [Value]))}.

-ifdef(TEST).
format_test() ->
  Msg = lager_msg:new("hello", {1465, 900000, 0}, info, [{pid, self()}, {component, "C"}, {meta_bin, <<"meta">>}], []),
  ?assertEqual(<<"{"
                   "\"component\":\"C\","
                   "\"date\":\"2016-06-14 12:26:40.000\","
                   "\"message\":\"hello\","
                   "\"meta_bin\":\"meta\","
                   "\"pid\":\"",
                    (list_to_binary(io_lib:format("~p", [self()])))/binary,
                   "\",",
                   "\"severity\":\"info\""
                 "}\n">>, format(Msg, [])).
-endif.
