defmodule Kylie.Integration.KylieMinogueTest do
  @moduledoc """
  End-to-end tests against a real Cayley server modelled around Kylie
  Minogue's music, discs and films. Requires a Cayley instance reachable
  at the configured `base_url` (default: `http://127.0.0.1:64210`).

  Run with:

      docker run -d --name kylie-cayley -p 64210:64210 \\
        cayleygraph/cayley:latest http --init --host=0.0.0.0:64210

      mix test.integration
  """

  use ExUnit.Case, async: false

  alias Kylie.Squad
  alias Kylie.Test.Cayley

  @moduletag :integration

  # ---- fixtures: curated, widely-known facts about Kylie Minogue ----

  defp kylie, do: "Kylie Minogue"

  defp discs do
    [
      {"Kylie",             "1988"},
      {"Enjoy Yourself",    "1989"},
      {"Rhythm of Love",    "1990"},
      {"Fever",             "2001"},
      {"Light Years",       "2000"},
      {"Aphrodite",         "2010"},
      {"Golden",            "2018"},
      {"DISCO",             "2020"},
      {"Tension",           "2023"}
    ]
  end

  # Songs grouped by their parent album. These are all real tracks.
  defp songs_by_album do
    %{
      "Kylie"          => ["The Loco-Motion", "I Should Be So Lucky"],
      "Enjoy Yourself" => ["Hand on Your Heart"],
      "Rhythm of Love" => ["Better the Devil You Know"],
      "Light Years"    => ["Spinning Around", "On a Night Like This"],
      # The "Can't ..." title is the real-world apostrophe test case.
      "Fever"          => ["Can't Get You Out of My Head", "In Your Eyes", "Love at First Sight"],
      "Aphrodite"      => ["All the Lovers"],
      "DISCO"          => ["Magic"],
      "Tension"        => ["Padam Padam"]
    }
  end

  defp films do
    [
      "The Delinquents",
      "Street Fighter",
      "Moulin Rouge!",
      "Jack & Diane",
      "Holy Motors",
      "San Andreas"
    ]
  end

  # Materialise every fixture fact into quads so the tests can share a
  # single graph without carefully orchestrating setup/teardown ordering.
  defp fixture_squads do
    disc_quads =
      Enum.flat_map(discs(), fn {album, year} ->
        [
          Squad.new(kylie(), "recorded", album),
          Squad.new(album, "released_in", year)
        ]
      end)

    song_quads =
      for {album, songs} <- songs_by_album(),
          song <- songs do
        Squad.new(album, "includes", song)
      end

    film_quads =
      Enum.map(films(), fn film -> Squad.new(kylie(), "acted_in", film) end)

    disc_quads ++ song_quads ++ film_quads
  end

  setup do
    squads = fixture_squads()
    :ok = Kylie.add(squads)
    on_exit(fn -> Cayley.purge(squads) end)
    {:ok, squads: squads}
  end

  # ---- discs ----

  describe "discs (albums Kylie recorded)" do
    test "the discography query returns every fixture album" do
      {:ok, recorded} = Kylie.get_result(kylie(), "recorded")

      expected = Enum.map(discs(), fn {album, _} -> album end)
      assert Enum.sort(recorded) == Enum.sort(expected)
    end

    test "Fever was released in 2001" do
      {:ok, years} = Kylie.get_result("Fever", "released_in")
      assert years == ["2001"]
    end

    test "the 1988 debut is the self-titled album 'Kylie'" do
      {:ok, year} = Kylie.get_result("Kylie", "released_in")
      assert year == ["1988"]
    end
  end

  # ---- music (songs on discs) ----

  describe "music (songs on discs)" do
    test "Fever includes Can't Get You Out of My Head — apostrophe round-trips unharmed" do
      {:ok, tracks} = Kylie.get_result("Fever", "includes")

      assert "Can't Get You Out of My Head" in tracks
      assert "In Your Eyes" in tracks
      assert "Love at First Sight" in tracks
    end

    test "chained traversal: all songs on all albums Kylie recorded" do
      q =
        Kylie.Query.build([
          {:graph_vertex, kylie()},
          {:out, "recorded"},
          {:out, "includes"},
          :all
        ])

      {:ok, results} = Kylie.query(q)
      tracks = results |> Enum.map(& &1["id"]) |> Enum.sort()

      expected = songs_by_album() |> Map.values() |> List.flatten() |> Enum.sort()
      assert tracks == expected
    end

    test "Padam Padam is a track on Tension (2023)" do
      {:ok, tracks} = Kylie.get_result("Tension", "includes")
      assert tracks == ["Padam Padam"]
    end
  end

  # ---- films ----

  describe "films (movies Kylie acted in)" do
    test "returns the full film fixture set" do
      {:ok, acted} = Kylie.get_result(kylie(), "acted_in")
      assert Enum.sort(acted) == Enum.sort(films())
    end

    test "Moulin Rouge! is in the fixture set" do
      {:ok, acted} = Kylie.get_result(kylie(), "acted_in")
      assert "Moulin Rouge!" in acted
    end
  end

  # ---- error path ----

  describe "error path" do
    test "a query pointing at a nonexistent subject returns an empty list, not an error" do
      {:ok, results} = Kylie.get_result("Nonexistent Artist", "recorded")
      assert results == []
    end

    test "deleting one album removes it from the discography without touching the others" do
      fever_disc = Squad.new(kylie(), "recorded", "Fever")
      :ok = Kylie.delete(fever_disc)

      {:ok, recorded} = Kylie.get_result(kylie(), "recorded")
      refute "Fever" in recorded
      assert "Tension" in recorded
      assert "Kylie" in recorded

      # Re-add so on_exit's purge sees a consistent state.
      :ok = Kylie.add(fever_disc)
    end
  end
end
