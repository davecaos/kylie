# Kylie (Elixir edition)

Pure-Elixir client for the [Cayley](https://cayley.io) graph database.
This subproject is a standalone Mix app — it shares no compilation with the
Erlang/Rebar3 library at the repo root. Use whichever edition fits your stack.

The library talks to Cayley over HTTP: `POST /api/v1/write` and
`POST /api/v1/delete` for quads, and `POST /api/v1/query/gizmo` for Gizmo
(the JS-flavoured successor to Gremlin) queries.

## Installation

Add `:kylie` to your mix.exs dependencies. While the Elixir edition is not
yet on Hex, pull it directly:

```elixir
defp deps do
  [
    {:kylie, github: "davecaos/kylie", sparse: "elixir"}
  ]
end
```

## Configuration

```elixir
config :kylie,
  base_url: "http://127.0.0.1:64210",
  query_path: "/api/v1/query/gizmo",
  recv_timeout: 5_000,
  connect_timeout: 5_000
```

`query_path` defaults to `/api/v1/query/gizmo`, matching current Cayley
releases. Older servers that still expose the pre-rename endpoint should
set it to `/api/v1/query/gremlin`.

## Quick start

```elixir
alias Kylie.Squad

Kylie.add(Squad.new("Kylie Minogue", "recorded", "Fever"))
#=> :ok

Kylie.get_result("Kylie Minogue", "recorded")
#=> {:ok, ["Fever"]}

# Raw Gizmo query
q = Kylie.Query.build([
  {:graph_vertex, "Kylie Minogue"},
  {:out, "recorded"},
  {:out, "includes"},
  :all
])

Kylie.query(q)
#=> {:ok, [%{"id" => "Can't Get You Out of My Head"}, ...]}
```

`Kylie.Query.build/1` takes a small keyword-DSL (`:graph_vertex`, `:out`,
`:in`, `:has`, `:save`, `:get_limit`, `:skip`, `:all`, …) and emits a Gizmo
string. Every string argument is escaped via `Kylie.Query.escape/1` so a
subject, predicate or object containing `'` or `\` cannot break out of the
single-quoted JS literal and inject a new traversal — see the
"query-injection defence" tests in
[test/kylie/query_test.exs](test/kylie/query_test.exs) for the guarantee.

## Testing

Three Mix aliases map to the three useful modes:

| Command                | What runs                                       | Needs Cayley? |
|------------------------|-------------------------------------------------|---------------|
| `mix test.unit`        | Pure unit tests (Squad, Query, escape, DSL)     | No            |
| `mix test.integration` | Integration suite against live Cayley           | Yes           |
| `mix test.all`         | Everything                                      | Yes           |

`test.integration` and `test.all` run a preflight Cayley probe; if the server
is unreachable they halt with a clear message and the Docker one-liner,
instead of letting every test's setup explode with `:econnrefused`.

### Running against a local Cayley (Docker)

```bash
docker run -d --name kylie-cayley -p 64210:64210 \
  cayleygraph/cayley:latest http --init --host=0.0.0.0:64210

mix test.integration

docker rm -f kylie-cayley
```

The integration suite at
[test/integration/kylie_minogue_test.exs](test/integration/kylie_minogue_test.exs)
populates the graph with curated Kylie Minogue facts — 9 studio albums with
release years, the songs on each of them (including the apostrophe
round-trip test case `Can't Get You Out of My Head`), and 6 of her films
(including `Moulin Rouge!` with its exclamation mark) — and exercises writes,
queries, chained traversals and delete-without-leakage.

## Layout

- [lib/kylie.ex](lib/kylie.ex) — public facade (`add/1`, `delete/1`,
  `query/1`, `get_result/2`).
- [lib/kylie/squad.ex](lib/kylie/squad.ex) — quad struct and JSON encoding.
- [lib/kylie/query.ex](lib/kylie/query.ex) — Gizmo DSL + escape.
- [lib/kylie/client.ex](lib/kylie/client.ex) — HTTP client (Req-based).
- [lib/kylie/application.ex](lib/kylie/application.ex) — OTP app entry.
- [test/kylie/](test/kylie/) — pure unit tests.
- [test/integration/](test/integration/) — Docker-Cayley integration tests.
- [test/support/cayley.ex](test/support/cayley.ex) — test teardown helper.

## License

MIT — see [../LICENSE](../LICENSE).
