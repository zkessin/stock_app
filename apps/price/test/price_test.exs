defmodule PriceTest do
  use ExUnit.Case
  doctest Price

  test "greets the world" do
    assert Price.hello() == :world
  end
end
