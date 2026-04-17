defmodule Kylie.Client do
  @moduledoc """
  HTTP client for Cayley's write/delete/query endpoints.

  Configuration is read from the `:kylie` application env — see
  `config/config.exs` and the `Kylie` module docs for the keys.
  """

  alias Kylie.Squad

  @type error ::
          {:error, {:http_status, pos_integer(), binary()}}
          | {:error, {:transport, term()}}
          | {:error, {:decode, term()}}

  @write_path "/api/v1/write"
  @delete_path "/api/v1/delete"

  @spec add(Squad.t() | [Squad.t()]) :: :ok | error()
  def add(squad_or_squads) do
    write(@write_path, Squad.to_json_payload(squad_or_squads))
  end

  @spec delete(Squad.t() | [Squad.t()]) :: :ok | error()
  def delete(squad_or_squads) do
    write(@delete_path, Squad.to_json_payload(squad_or_squads))
  end

  @doc """
  Run a Gizmo query string against Cayley. Returns the raw result list
  (each result is a map like `%{"id" => "Fever"}` — possibly with extra
  keys when the query used `Save`/`Tag`).
  """
  @spec query(iodata()) :: {:ok, [map()]} | error()
  def query(query_string) do
    url = base_url() <> query_path()

    case Req.post(url,
           body: IO.iodata_to_binary(query_string),
           receive_timeout: recv_timeout(),
           connect_options: [timeout: connect_timeout()]
         ) do
      {:ok, %Req.Response{status: 200, body: body}} ->
        {:ok, extract_results(body)}

      {:ok, %Req.Response{status: status, body: body}} ->
        {:error, {:http_status, status, to_string(body)}}

      {:error, reason} ->
        {:error, {:transport, reason}}
    end
  end

  # ---- helpers ----

  defp write(path, payload) do
    url = base_url() <> path

    case Req.post(url,
           json: payload,
           receive_timeout: recv_timeout(),
           connect_options: [timeout: connect_timeout()]
         ) do
      {:ok, %Req.Response{status: 200}} -> :ok
      {:ok, %Req.Response{status: status, body: body}} ->
        {:error, {:http_status, status, to_string(body)}}
      {:error, reason} ->
        {:error, {:transport, reason}}
    end
  end

  # Cayley's query endpoint returns {"result": [...]} on success, and may
  # return {"result": null} when there are no matches. Req auto-decodes
  # JSON when the Content-Type is application/json.
  defp extract_results(%{"result" => nil}), do: []
  defp extract_results(%{"result" => results}) when is_list(results), do: results
  defp extract_results(binary) when is_binary(binary) do
    case Jason.decode(binary) do
      {:ok, %{"result" => nil}} -> []
      {:ok, %{"result" => results}} when is_list(results) -> results
      _ -> []
    end
  end
  defp extract_results(_), do: []

  defp base_url,       do: Application.fetch_env!(:kylie, :base_url)
  defp query_path,     do: Application.fetch_env!(:kylie, :query_path)
  defp recv_timeout,   do: Application.fetch_env!(:kylie, :recv_timeout)
  defp connect_timeout, do: Application.fetch_env!(:kylie, :connect_timeout)
end
