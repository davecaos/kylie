defmodule Kylie.Test.Cayley do
  @moduledoc """
  Helpers for integration tests that need a reachable Cayley server.

  The "is Cayley up?" probe runs once in `test/test_helper.exs` and, when
  Cayley is unreachable, adds `:cayley_required` to the ExUnit exclude
  list so tagged tests are filtered out instead of failing noisily in CI.
  """

  @doc """
  Remove the given quads from Cayley. Used as a teardown step so one test
  cannot leak data into another.
  """
  def purge(squads) when is_list(squads) do
    _ = Kylie.delete(squads)
    :ok
  end
end
