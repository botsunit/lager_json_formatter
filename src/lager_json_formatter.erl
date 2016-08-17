-module(lager_json_formatter).

-include_lib("lager/include/lager.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([format/2]).

-define(DEFAULT_CONFIG, [{date, [date, " ", time]},
                         message,
                         pid,
                         severity,
                         {component, [{module, "-"}, ":", {function, "-"}, "/", {line, "-"}]}]).

-spec format(lager_msg:lager_msg(), list()) -> any().
format(Message, []) ->
  format(Message, ?DEFAULT_CONFIG);
format(Message, Config) ->
  Map = lists:foldl(fun(Data, Map) ->
                        output(Data, Map, Message)
                    end, #{}, Config),
  << (jsone:encode(Map))/binary, "\n" >>.

output(Data, Map, Message) when is_atom(Data) ->
  maps:put(Data, get_data(Data, Message), Map);
output({Key, Value}, Map, Message) when is_list(Value) ->
  maps:put(Key, to_binary([get_data(V, Message) || V <- Value]), Map);
output({Key, Value}, Map, Message) ->
  maps:put(Key, get_data(Value, Message), Map).

get_data(Value, _) when is_list(Value); is_binary(Value) ->
  Value;
get_data(message, Message) ->
  to_binary(lager_msg:message(Message));
get_data(date, Message) ->
  {D, _} = lager_msg:datetime(Message),
  to_binary(D);
get_data(time, Message) ->
  {_, T} = lager_msg:datetime(Message),
  to_binary(T);
get_data(severity, Message) ->
  atom_to_binary(lager_msg:severity(Message), latin1);
get_data({Metadata, Absent}, Message) ->
  MD = lager_msg:metadata(Message),
  case lists:keyfind(Metadata, 1, MD) of
    false ->
      Absent;
    {Metadata, Value} ->
      to_binary(Value)
  end;
get_data(Metadata, Message) when is_atom(Metadata) ->
  get_data({Metadata, <<"Undefined">>}, Message).


to_binary(A) when is_atom(A) -> atom_to_binary(A, latin1);
to_binary(P) when is_pid(P) -> list_to_binary(pid_to_list(P));
to_binary(L) when is_list(L) -> iolist_to_binary(L);
to_binary(B) when is_binary(B) -> B;
to_binary(O) -> list_to_binary(io_lib:format("~p", [O])).

-ifdef(TEST).
format_test() ->
  Msg = lager_msg:new("hello", {1465, 900000, 0}, info,
                      [{pid, c:pid(0, 0, 0)},
                       {module, ?MODULE},
                       {function, test},
                       {line, 123}],
                      []),
  ?assertEqual(<<"{\"component\":\"lager_json_formatter:test\\/123\","
                 "\"date\":\"2016-06-14 12:26:40.000\","
                 "\"message\":\"hello\","
                 "\"pid\":\"<0.0.0>\","
                 "\"severity\":\"info\"}\n">>,
               format(Msg, [])),
  ?assertEqual(<<"{\"datetime\":\"2016-06-14-12:26:40.000\","
                 "\"msg\":\"hello\","
                 "\"sev\":\"info\"}\n">>,
               format(Msg, [{datetime, [date, "-", time]},
                            {msg, message},
                            {sev, severity}])).
special_chars_test() ->
  Msg = lager_msg:new("hello\r\n\t\"wor\\d!", {1465, 900000, 0}, info,
                      [],
                      []),
  ?assertEqual(<<"{\"component\":\"-:-\\/-\","
                 "\"date\":\"2016-06-14 12:26:40.000\","
                 "\"message\":\"hello\\r\\n\\t\\\"wor\\\\d!\","
                 "\"pid\":\"Undefined\","
                 "\"severity\":\"info\"}\n">>,
               format(Msg, [])).
-endif.
