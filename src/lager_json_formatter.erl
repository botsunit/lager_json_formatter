-module(lager_json_formatter).

-include_lib("lager/include/lager.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([format/2]).

-spec format(lager_msg:lager_msg(),list()) -> any().
format(Message, _Config) ->
  [output(V, Message) || V <- ["{\"date\": \"", date, " ", time ,"\",",
                               "\"severity\": \"", severity ,"\",",
                               "\"pid\": \"", {pid, "-"}, "\",",
                               "\"component\": \"", {module, "-"}, ":", {function, "-"}, "/", {line, "-"}, "\",",
                               "\"message\": \"", message, "\"}\n"]].

output(message, Msg) -> 
  lists:foldl(fun({Target, Replacement}, Acc) ->
                  re:replace(Acc, Target, Replacement, [global, {return,list}])
              end, lager_msg:message(Msg), [{"\\\\", "\\\\\\\\"},
                                            {"\"", "\\\\\""},
                                            {"\u", "\\\\\\\\u"},
                                            {"\b", "\\\\\\\\b"},
                                            {"\f", "\\\\\\\\f"},
                                            {"\n", "\\\\\\\\n"},
                                            {"\r", "\\\\\\\\r"},
                                            {"\t", "\\\\\\\\t"}
                                           ]);
output(date, Msg) ->
  {D, _T} = lager_msg:datetime(Msg),
  D;
output(time, Msg) ->
  {_D, T} = lager_msg:datetime(Msg),
  T;
output(severity, Msg) ->
  atom_to_list(lager_msg:severity(Msg));
output(blank, _Msg) ->
  output({blank," "}, _Msg);
output({blank, Fill}, _Msg) ->
  Fill;
output(sev, Msg) ->
  [lager_util:level_to_chr(lager_msg:severity(Msg))];
output(metadata, Msg) ->
  output({metadata, "=", " "}, Msg);
output({metadata, IntSep, FieldSep}, Msg) ->
  MD = lists:keysort(1, lager_msg:metadata(Msg)),
  string:join([io_lib:format("~s~s~p", [K, IntSep, V]) || {K, V} <- MD], FieldSep);
output(Prop,Msg) when is_atom(Prop) ->
  Metadata = lager_msg:metadata(Msg),
  make_printable(get_metadata(Prop,Metadata,<<"Undefined">>));
output({Prop,Default},Msg) when is_atom(Prop) ->
  Metadata = lager_msg:metadata(Msg),
  make_printable(get_metadata(Prop,Metadata,output(Default,Msg)));
output({Prop, Present, Absent}, Msg) when is_atom(Prop) ->
  %% sort of like a poor man's ternary operator
  Metadata = lager_msg:metadata(Msg),
  case get_metadata(Prop, Metadata) of
    undefined ->
      [ output(V, Msg) || V <- Absent];
    _ ->
      [ output(V, Msg) || V <- Present]
  end;
output({Prop, Present, Absent, Width}, Msg) when is_atom(Prop) ->
  %% sort of like a poor man's ternary operator
  Metadata = lager_msg:metadata(Msg),
  case get_metadata(Prop, Metadata) of
    undefined ->
      [ output(V, Msg, Width) || V <- Absent];
    _ ->
      [ output(V, Msg, Width) || V <- Present]
  end;
output(Other,_) -> make_printable(Other).

output(message, Msg, _Width) -> lager_msg:message(Msg);
output(date,Msg, _Width) ->
  {D, _T} = lager_msg:datetime(Msg),
  D;
output(time, Msg, _Width) ->
  {_D, T} = lager_msg:datetime(Msg),
  T;
output(severity, Msg, Width) ->
  make_printable(atom_to_list(lager_msg:severity(Msg)), Width);
output(sev,Msg, _Width) ->
  %% Write brief acronym for the severity level (e.g. debug -> $D)
  [lager_util:level_to_chr(lager_msg:severity(Msg))];
output(blank,_Msg, _Width) ->
  output({blank, " "},_Msg, _Width);
output({blank, Fill},_Msg, _Width) ->
  Fill;
output(metadata, Msg, _Width) ->
  output({metadata, "=", " "}, Msg, _Width);
output({metadata, IntSep, FieldSep}, Msg, _Width) ->
  MD = lists:keysort(1, lager_msg:metadata(Msg)),
  [string:join([io_lib:format("~s~s~p", [K, IntSep, V]) || {K, V} <- MD], FieldSep)];

output(Prop, Msg, Width) when is_atom(Prop) ->
  Metadata = lager_msg:metadata(Msg),
  make_printable(get_metadata(Prop,Metadata,<<"Undefined">>), Width);
output({Prop,Default},Msg, Width) when is_atom(Prop) ->
  Metadata = lager_msg:metadata(Msg),
  make_printable(get_metadata(Prop,Metadata,output(Default,Msg)), Width);
output(Other,_, Width) -> make_printable(Other, Width).

-spec make_printable(any()) -> iolist().
make_printable(A) when is_atom(A) -> atom_to_list(A);
make_printable(P) when is_pid(P) -> pid_to_list(P);
make_printable(L) when is_list(L) orelse is_binary(L) -> L; 
make_printable(Other) -> io_lib:format("~p",[Other]).

make_printable(A,W) when is_integer(W)-> string:left(make_printable(A),W);
make_printable(A,{Align,W}) when is_integer(W) ->
  case Align of
    left ->
      string:left(make_printable(A),W);
    centre ->
      string:centre(make_printable(A),W);
    right ->
      string:right(make_printable(A),W);
    _ ->
      string:left(make_printable(A),W)
  end;

make_printable(A,_W) -> make_printable(A).

get_metadata(Key, Metadata) ->
  get_metadata(Key, Metadata, undefined).

get_metadata(Key, Metadata, Default) ->
  case lists:keyfind(Key, 1, Metadata) of
    false ->
      Default;
    {Key, Value} ->
      Value
  end.

-ifdef(TEST).
output_test() ->
  meck:new(lager_msg, [passthrough]),
  meck:expect(lager_msg, message, fun(M) -> M end),
  ?assertEqual("\\\"hello\\\"", output(message, "\"hello\"")),
  ?assertEqual("\\\"hel\\\\lo\\\"", output(message, "\"hel\\lo\"")),
  ?assertEqual("\\\\\\\"hello\\\"", output(message, "\\\"hello\"")),
  ?assertEqual("xx \\\\u1234 xx", output(message, "xx \u1234 xx")),
  ?assertEqual("xx \\\\b xx", output(message, "xx \b xx")),
  ?assertEqual("xx \\\\f xx", output(message, "xx \f xx")),
  ?assertEqual("xx \\\\n xx", output(message, "xx \n xx")),
  ?assertEqual("xx \\\\t xx", output(message, "xx \t xx")),
  ?assertEqual("xx \\\\r xx", output(message, "xx \r xx")),
  meck:unload(lager_msg).
-endif.

