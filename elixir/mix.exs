defmodule Kylie.MixProject do
  use Mix.Project

  @version "0.1.0"
  @source_url "https://github.com/davecaos/kylie"

  def project do
    [
      app: :kylie,
      version: @version,
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env()),
      description: "Pure Elixir client for the Cayley graph database.",
      package: package(),
      source_url: @source_url,
      aliases: aliases(),
      preferred_cli_env: [
        "test.unit": :test,
        "test.integration": :test,
        "test.all": :test
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Kylie.Application, []}
    ]
  end

  defp deps do
    [
      {:req, "~> 0.5"},
      {:jason, "~> 1.4"}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp package do
    [
      licenses: ["MIT"],
      links: %{"GitHub" => @source_url}
    ]
  end

  defp aliases do
    [
      "test.unit": ["test --exclude integration"],
      "test.integration": [&integration_preflight/1, "test --only integration"],
      "test.all": [&integration_preflight/1, "test --include integration"]
    ]
  end

  # Fail fast with a clear message when the user asks for integration tests
  # but Cayley is unreachable — better than letting every test's `setup`
  # blow up on :econnrefused.
  defp integration_preflight(_args) do
    Application.ensure_all_started(:inets)
    base_url = Application.get_env(:kylie, :base_url, "http://127.0.0.1:64210")
    url = String.to_charlist(base_url <> "/")

    case :httpc.request(:get, {url, []}, [{:timeout, 500}, {:connect_timeout, 500}], []) do
      {:ok, _} ->
        :ok

      _ ->
        Mix.raise(
          "Cayley is not reachable at #{base_url} — integration tests need " <>
            "a running Cayley. Start one with:\n\n" <>
            "    docker run -d --name kylie-cayley -p 64210:64210 \\\n" <>
            "      cayleygraph/cayley:latest http --init --host=0.0.0.0:64210\n"
        )
    end
  end
end
