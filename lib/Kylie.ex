# File: Kylie.ex
# This file was generated from kylie.beam
# Using rebar3_elixir (https://github.com/botsunit/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Kylie do
  def unquote(:"start")() do
    :erlang.apply(:"kylie", :"start", [])
  end
  def unquote(:"stop")() do
    :erlang.apply(:"kylie", :"stop", [])
  end
  def unquote(:"add")(arg1) do
    :erlang.apply(:"kylie", :"add", [arg1])
  end
  def unquote(:"delete")(arg1) do
    :erlang.apply(:"kylie", :"delete", [arg1])
  end
  def unquote(:"get_result")(arg1, arg2) do
    :erlang.apply(:"kylie", :"get_result", [arg1, arg2])
  end
  def unquote(:"build_gremblin_human_readable")(arg1) do
    :erlang.apply(:"kylie", :"build_gremblin_human_readable", [arg1])
  end
  def unquote(:"build_gremblin")(arg1) do
    :erlang.apply(:"kylie", :"build_gremblin", [arg1])
  end
  def unquote(:"query")(arg1) do
    :erlang.apply(:"kylie", :"query", [arg1])
  end
end
