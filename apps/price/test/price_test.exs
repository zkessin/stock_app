defmodule PriceTest do
  use ExUnit.Case
  doctest Price

  property "Code Change Round Trip Property" do
    forall initial_state <- integer(100, 1000) do
      {:ok, new_state} = Price.Server.code_change("0.1.0", initial_state, [])
      assert {:ok, initial_state} = Price.Server.code_change({:down, "0.1.0"}, new_state, [])
    end
  end

  test "greets the world" do
    assert Price.hello() == :world
  end
end
