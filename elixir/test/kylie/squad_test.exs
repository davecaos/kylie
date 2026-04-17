defmodule Kylie.SquadTest do
  use ExUnit.Case, async: true

  doctest Kylie.Squad

  alias Kylie.Squad

  describe "new/3" do
    test "builds a three-part quad without a label" do
      s = Squad.new("Kylie Minogue", "recorded", "Fever")

      assert %Squad{
               subject: "Kylie Minogue",
               predicate: "recorded",
               object: "Fever",
               label: nil
             } = s
    end

    test "rejects non-binary fields" do
      assert_raise FunctionClauseError, fn ->
        Squad.new(:kylie, "recorded", "Fever")
      end
    end
  end

  describe "new/4" do
    test "accepts an optional label" do
      s = Squad.new("Kylie Minogue", "recorded", "Fever", "discography")
      assert s.label == "discography"
    end
  end

  describe "to_json_payload/1" do
    test "wraps a single squad in a list matching Cayley's write body shape" do
      s = Squad.new("Kylie Minogue", "recorded", "Fever")

      assert [%{"subject" => "Kylie Minogue", "predicate" => "recorded", "object" => "Fever"}] =
               Squad.to_json_payload(s)
    end

    test "preserves the label when present" do
      s = Squad.new("Kylie Minogue", "recorded", "Fever", "discography")

      assert [
               %{
                 "subject" => "Kylie Minogue",
                 "predicate" => "recorded",
                 "object" => "Fever",
                 "label" => "discography"
               }
             ] = Squad.to_json_payload(s)
    end

    test "accepts a list of squads and emits them in order" do
      squads = [
        Squad.new("Kylie Minogue", "recorded", "Kylie"),
        Squad.new("Kylie Minogue", "recorded", "Enjoy Yourself"),
        Squad.new("Kylie Minogue", "recorded", "Rhythm of Love")
      ]

      payload = Squad.to_json_payload(squads)

      assert length(payload) == 3
      assert Enum.at(payload, 0)["object"] == "Kylie"
      assert Enum.at(payload, 1)["object"] == "Enjoy Yourself"
      assert Enum.at(payload, 2)["object"] == "Rhythm of Love"
    end
  end
end
