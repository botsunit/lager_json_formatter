## Lager JSON Formatter

### Erlang 

sys.config :

```erlang
{lager, [
  {handlers, [
    {lager_console_backend, [info, {lager_default_formatter, [date, " ", time," [",severity,"] -- ", module, ":", function, " (", pid, "/", line, ") -- ", message, "\n"]}]},
    {lager_file_backend, [{file, "log/wok_error.log"}, {level, error}, {formatter, lager_json_formatter}]},
    {lager_file_backend, [{file, "log/wok_debug.log"}, {level, debug}, {formatter, lager_json_formatter}]},
    {lager_file_backend, [{file, "log/wok_console.log"}, {level, info}, {formatter, lager_json_formatter}]}
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
    lager_file_backend: [file: 'log/wok_error.log', level: :error, formatter: lager_json_formatter],
    lager_file_backend: [file: 'log/wok_debug.log', level: :debug, formatter: lager_json_formatter],
    lager_file_backend: [file: 'log/wok_console.log', level: :info, formatter: lager_json_formatter]
  ]
```

mix.exs :

```elixir
{:lager_json_formatter, git: "git@gitlab.botsunit.com:msaas/lager_json_formatter.git", branch: "master"},
```


