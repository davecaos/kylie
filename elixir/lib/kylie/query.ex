defmodule Kylie.Query do
  @moduledoc """
  Builds Cayley Gizmo queries from a small keyword-list DSL.

  The DSL is a list of "steps" — each step is either a tagged tuple
  `{step_name, arg}` or the terminal atom `:all`. Steps are composed
  into a single chained traversal:

      iex> Kylie.Query.build([
      ...>   {:graph_vertex, "Kylie Minogue"},
      ...>   {:out, "recorded"},
      ...>   :all
      ...> ])
      "g.V('Kylie Minogue').Out('recorded').All()"

  String arguments are escaped so a user-supplied subject, predicate or
  object cannot break out of the surrounding `'...'` and inject a new
  traversal. Integer arguments to `:get_limit` and `:skip` are enforced
  at build time and emitted unquoted (Gizmo's actual signature).
  """

  @type step ::
          {:graph_vertex, String.t()}
          | {:graph_morphism, String.t()}
          | {:graph_emit, String.t()}
          | {:out, String.t()}
          | {:in, String.t()}
          | {:has, [String.t()]}
          | {:save, [String.t()]}
          | {:follow, String.t()}
          | {:followr, String.t()}
          | {:except, String.t()}
          | {:union, String.t()}
          | {:intersect, String.t()}
          | {:get_limit, non_neg_integer()}
          | {:skip, non_neg_integer()}
          | :all

  @type t :: [step()]

  @doc """
  Render a DSL list to a Gizmo query string.
  """
  @spec build(t()) :: String.t()
  def build(steps) when is_list(steps) do
    steps |> Enum.map(&step/1) |> IO.iodata_to_binary()
  end

  @doc """
  Convenience: the default "what is the value of <predicate> for <subject>?"
  traversal, equivalent to `g.V(subject).Out(predicate).All()`.
  """
  @spec out_all(String.t(), String.t()) :: String.t()
  def out_all(subject, predicate) when is_binary(subject) and is_binary(predicate) do
    build([{:graph_vertex, subject}, {:out, predicate}, :all])
  end

  # ---- steps ----

  defp step({:graph_vertex, v}),     do: ["g.V('", escape(v), "')."]
  defp step({:graph_morphism, v}),   do: ["g.M('", escape(v), "')."]
  defp step({:graph_emit, v}),       do: ["g.Emit('", escape(v), "')."]
  defp step({:out, v}),              do: ["Out('", escape(v), "')."]
  defp step({:in, v}),               do: ["In('", escape(v), "')."]
  defp step({:has, [p, o]}),         do: ["Has('", escape(p), "','", escape(o), "')."]
  defp step({:save, [p, tag]}),      do: ["Save('", escape(p), "','", escape(tag), "')."]
  defp step({:follow, v}),           do: ["Follow('", escape(v), "')."]
  defp step({:followr, v}),          do: ["FollowR('", escape(v), "')."]
  defp step({:except, v}),           do: ["Except('", escape(v), "')."]
  defp step({:union, v}),            do: ["Union('", escape(v), "')."]
  defp step({:intersect, v}),        do: ["Intersect('", escape(v), "')."]

  defp step({:get_limit, n}) when is_integer(n) and n >= 0,
    do: ["GetLimit(", Integer.to_string(n), ")."]

  defp step({:skip, n}) when is_integer(n) and n >= 0,
    do: ["Skip(", Integer.to_string(n), ")."]

  defp step(:all), do: "All()"

  defp step(other) do
    raise ArgumentError,
          "invalid query step: #{inspect(other)} — see Kylie.Query docs for supported steps"
  end

  # ---- escape ----

  @doc """
  Escape a string so it can be safely embedded inside a single-quoted Gizmo
  argument. Backslash and single-quote are the only characters that can
  break out of the JS string literal; both are escaped with a leading `\\`.

      iex> Kylie.Query.escape("Can't Get You Out of My Head")
      ~S(Can\\'t Get You Out of My Head)
  """
  @spec escape(String.t()) :: String.t()
  def escape(s) when is_binary(s) do
    s
    |> String.replace("\\", "\\\\")
    |> String.replace("'", "\\'")
  end
end
