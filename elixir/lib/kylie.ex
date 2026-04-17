defmodule Kylie do
  @moduledoc """
  Pure Elixir client for the [Cayley](https://cayley.io) graph database.

  ## Configuration

      config :kylie,
        base_url: "http://127.0.0.1:64210",
        query_path: "/api/v1/query/gizmo",
        recv_timeout: 5_000,
        connect_timeout: 5_000

  `query_path` defaults to `/api/v1/query/gizmo` for current Cayley. Older
  servers that still expose the pre-rename endpoint should set it to
  `/api/v1/query/gremlin`.

  ## Example

      iex> alias Kylie.Squad
      iex> Kylie.add(Squad.new("Kylie Minogue", "recorded", "Fever"))
      :ok
      iex> Kylie.get_result("Kylie Minogue", "recorded")
      {:ok, ["Fever"]}
  """

  alias Kylie.{Client, Query, Squad}

  @type error :: Client.error()

  @doc "Write one or more quads to Cayley."
  @spec add(Squad.t() | [Squad.t()]) :: :ok | error()
  defdelegate add(squad_or_squads), to: Client

  @doc "Delete one or more quads from Cayley."
  @spec delete(Squad.t() | [Squad.t()]) :: :ok | error()
  defdelegate delete(squad_or_squads), to: Client

  @doc """
  Run a raw Gizmo query string. Returns `{:ok, [map()]}` — each map is
  whatever Cayley returned (typically `%{"id" => binary()}`, possibly
  extended with `Save`/`Tag` keys).
  """
  @spec query(iodata()) :: {:ok, [map()]} | error()
  defdelegate query(query_string), to: Client

  @doc """
  Convenience wrapper around the common "what does `subject` `predicate`?"
  traversal. Equivalent to `g.V(subject).Out(predicate).All()` and returns
  just the list of object-ids.

      iex> Kylie.get_result("Kylie Minogue", "acted_in")
      {:ok, ["Moulin Rouge!"]}
  """
  @spec get_result(String.t(), String.t()) :: {:ok, [String.t()]} | error()
  def get_result(subject, predicate) when is_binary(subject) and is_binary(predicate) do
    case Client.query(Query.out_all(subject, predicate)) do
      {:ok, results} -> {:ok, Enum.map(results, & &1["id"])}
      {:error, _} = err -> err
    end
  end
end
