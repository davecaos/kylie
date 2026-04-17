defmodule Kylie.QueryTest do
  use ExUnit.Case, async: true

  doctest Kylie.Query

  alias Kylie.Query

  describe "build/1 — single steps" do
    test "graph_vertex" do
      assert Query.build([{:graph_vertex, "Kylie Minogue"}]) == "g.V('Kylie Minogue')."
    end

    test "out" do
      assert Query.build([{:out, "recorded"}]) == "Out('recorded')."
    end

    test "in" do
      assert Query.build([{:in, "recorded"}]) == "In('recorded')."
    end

    test "has" do
      assert Query.build([{:has, ["released_in", "2001"]}]) ==
               "Has('released_in','2001')."
    end

    test "save" do
      assert Query.build([{:save, ["released_in", "year"]}]) ==
               "Save('released_in','year')."
    end

    test "get_limit emits an unquoted integer" do
      assert Query.build([{:get_limit, 5}]) == "GetLimit(5)."
    end

    test "skip emits an unquoted integer" do
      assert Query.build([{:skip, 3}]) == "Skip(3)."
    end

    test ":all is terminal and emits without a trailing dot" do
      assert Query.build([:all]) == "All()"
    end
  end

  describe "build/1 — composed chains" do
    test "Kylie Minogue's discography" do
      q =
        Query.build([
          {:graph_vertex, "Kylie Minogue"},
          {:out, "recorded"},
          :all
        ])

      assert q == "g.V('Kylie Minogue').Out('recorded').All()"
    end

    test "songs on albums that Kylie Minogue recorded" do
      q =
        Query.build([
          {:graph_vertex, "Kylie Minogue"},
          {:out, "recorded"},
          {:out, "includes"},
          :all
        ])

      assert q == "g.V('Kylie Minogue').Out('recorded').Out('includes').All()"
    end

    test "films with limit and skip (pagination shape)" do
      q =
        Query.build([
          {:graph_vertex, "Kylie Minogue"},
          {:out, "acted_in"},
          {:skip, 2},
          {:get_limit, 3},
          :all
        ])

      assert q == "g.V('Kylie Minogue').Out('acted_in').Skip(2).GetLimit(3).All()"
    end
  end

  describe "build/1 — argument validation" do
    test "get_limit with a non-integer is rejected (no silent quoting)" do
      assert_raise ArgumentError, ~r/invalid query step/, fn ->
        Query.build([{:get_limit, "5"}])
      end
    end

    test "skip with a negative integer is rejected" do
      assert_raise ArgumentError, ~r/invalid query step/, fn ->
        Query.build([{:skip, -1}])
      end
    end

    test "unknown step raises a clear ArgumentError" do
      assert_raise ArgumentError, ~r/invalid query step/, fn ->
        Query.build([{:teleport, "Kylie Minogue"}])
      end
    end
  end

  describe "escape/1 — Kylie Minogue's apostrophe-bearing titles" do
    test "Can't Get You Out of My Head has its apostrophe escaped" do
      assert Query.escape("Can't Get You Out of My Head") ==
               ~S(Can\'t Get You Out of My Head)
    end

    test "backslash is doubled" do
      assert Query.escape("a\\b") == "a\\\\b"
    end

    test "backslash is escaped before single-quote so \\' is literal" do
      assert Query.escape("a\\'b") == "a\\\\\\'b"
    end
  end

  describe "build/1 — query-injection defence" do
    test "a hostile subject containing ');All();g.V(' cannot add a new traversal" do
      hostile = "x');All();g.V('y"

      q = Query.build([{:graph_vertex, hostile}, :all])

      # Structural invariant: exactly two unescaped single-quotes survive —
      # the opening and closing of the single g.V('…') call the builder
      # emits. Every apostrophe from the hostile input must be escaped.
      unescaped_quotes =
        q
        |> String.replace(~S(\'), "")
        |> String.graphemes()
        |> Enum.count(&(&1 == "'"))

      assert unescaped_quotes == 2
      assert q =~ "g.V('"
      assert q =~ "').All()"
    end

    test "an object title with a real apostrophe round-trips through the builder" do
      # A genuine Kylie track: "Can't Get You Out of My Head". The builder must
      # not break on it, and the apostrophe must be escaped at the byte level.
      q = Query.build([{:graph_vertex, "Can't Get You Out of My Head"}, :all])

      assert q == ~S|g.V('Can\'t Get You Out of My Head').All()|
    end

    test "a predicate trying to break out and inject code is neutralised" do
      hostile = "is');delete;//"
      q = Query.build([{:out, hostile}])

      # The leading ' from the hostile payload must carry a \ before it.
      assert q =~ ~S|\');delete|
    end
  end
end
