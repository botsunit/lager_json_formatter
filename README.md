## Lager JSON Formatter

[![Hex.pm](https://img.shields.io/hexpm/v/lager_json_formatter.svg)](https://hex.pm/packages/lager_json_formatter)

### Erlang 

sys.config :

```erlang
{lager, [
  {handlers, [
    {lager_console_backend, [info, {lager_default_formatter, [date, " ", time," [",severity,"] -- ", module, ":", function, " (", pid, "/", line, ") -- ", message, "\n"]}]},
    {lager_file_backend, [{file, "log/wok_error.log"}, {level, error}, {formatter, lager_json_formatter}, {formatter_config, [date, time, severity, module, function, line, pid]}]},
    {lager_file_backend, [{file, "log/wok_debug.log"}, {level, debug}, {formatter, lager_json_formatter}, {formatter_config, [date, time, severity, module, function, line, pid]}]},
    {lager_file_backend, [{file, "log/wok_console.log"}, {level, info}, {formatter, lager_json_formatter}, {formatter_config, [date, time, severity, module, function, line, pid]}]}
  ]}
]}
```

rebar.config :

```erlang
{lager_default_formatter, {git, "git@gitlab.botsunit.com:msaas/lager_json_formatter.git", {branch, "master"}}}
```

### Elixir

config.exs :

```elixir
config :lager,
  handlers: [
    lager_console_backend: [:info, {:lager_default_formatter, [:date, ' ', :time, ' [', :severity, '] -- ', :module, ':', :function, ' (', :pid, '/', :line, ') -- ', :message, '\n']}],
    lager_file_backend: [file: 'log/wok_error.log', level: :error, formatter: :lager_json_formatter, formatter_config: [date, time, severity, module, function, line, pid]],
    lager_file_backend: [file: 'log/wok_debug.log', level: :debug, formatter: :lager_json_formatter, formatter_config: [date, time, severity, module, function, line, pid]],
    lager_file_backend: [file: 'log/wok_console.log', level: :info, formatter: :lager_json_formatter, formatter_config: [date, time, severity, module, function, line, pid]]
  ]
```

mix.exs :

```elixir
{:lager_json_formatter, git: "git@gitlab.botsunit.com:msaas/lager_json_formatter.git", branch: "master"},
```

### Formatter

The output is formated as a JSON string. By default, this module use this format :

```erlang
[{date, [date, " ", time]}, 
 message, 
 pid, 
 severity, 
 {component, [{module, "-"}, ":", {function, "-"}, "/", {line, "-"}]}]).
```

Thus, the output will be something like :

```erlang
<<"{\"component\":\"module:function/10\","
  "\"date\":\"2016-06-14 12:26:40.000\","
  "\"message\":\"my message\","
  "\"pid\":\"<0.0.0>\","
  "\"severity\":\"info\"}\n">>
```

By default a field use it own name as key. So if in the format list you use `date` this will generate a key-value `"date": "2016-06-14"`. You can change the field key by using a tuple where the first element is the key and the second one is the value. So `{msg, message}` will generate `{"msg": "My message"}`. The value can be a list. In this case, the result will be a concatenation of this list ; for example `{datetime, [date, " ", time]}` will generate `{"datetime": "2016-06-14 12:26:40.000"}`.

### Licence 

lager_json_formatter is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2016 BotsUnit<br />

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
* The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.



THIS SOFTWARE IS PROVIDED BY THE AUTHOR `AS IS` AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

