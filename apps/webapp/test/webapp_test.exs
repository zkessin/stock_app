defmodule WebappTest do
  use ExUnit.Case
  doctest Webapp

  test "greets the world" do
    assert Webapp.hello() == :world
  end
end
