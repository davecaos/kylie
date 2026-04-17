defmodule Kylie.Squad do
  @moduledoc """
  A Cayley quad — the atomic unit of the graph.

  Every fact in Cayley is a quad: `subject -> predicate -> object` with an
  optional `label`. For example, `Kylie Minogue -> recorded -> Fever`.

      iex> Kylie.Squad.new("Kylie Minogue", "recorded", "Fever")
      %Kylie.Squad{subject: "Kylie Minogue", predicate: "recorded", object: "Fever", label: nil}
  """

  @enforce_keys [:subject, :predicate, :object]
  defstruct [:subject, :predicate, :object, label: nil]

  @type t :: %__MODULE__{
          subject: String.t(),
          predicate: String.t(),
          object: String.t(),
          label: String.t() | nil
        }

  @spec new(String.t(), String.t(), String.t(), String.t() | nil) :: t()
  def new(subject, predicate, object, label \\ nil)
      when is_binary(subject) and is_binary(predicate) and is_binary(object) and
             (is_nil(label) or is_binary(label)) do
    %__MODULE__{subject: subject, predicate: predicate, object: object, label: label}
  end

  @doc """
  Render a squad (or list of squads) into the JSON shape Cayley expects on
  `POST /api/v1/write`:

      [{"subject": "...", "predicate": "...", "object": "...", "label": "..."}]
  """
  @spec to_json_payload(t() | [t()]) :: [map()]
  def to_json_payload(%__MODULE__{} = s), do: [to_map(s)]
  def to_json_payload(squads) when is_list(squads), do: Enum.map(squads, &to_map/1)

  defp to_map(%__MODULE__{label: nil} = s) do
    %{"subject" => s.subject, "predicate" => s.predicate, "object" => s.object}
  end

  defp to_map(%__MODULE__{} = s) do
    %{
      "subject" => s.subject,
      "predicate" => s.predicate,
      "object" => s.object,
      "label" => s.label
    }
  end
end
