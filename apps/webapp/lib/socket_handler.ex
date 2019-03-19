defmodule WebApp.SocketHandler do
  @behaviour :cowboy_websocket

  def init(request, _state) do
    state = %{registry_key: request.path, count: 0}
    {:cowboy_websocket, request, state, %{idle_timeout: :infinity}}
  end

  def websocket_init(state) do
    Price.Server.register(:enron)
    Price.Server.register(:lex_corp)
    Price.Server.register(:acme)
    Price.Server.register(:barings_bank)
    Registry.register(Registry.WebApp, state.registry_key, {})
    {:ok, state}
  end

  def read_file(file) do
    {:ok, Data} = File.read(file)
    Data
  end

  def websocket_info({:update, stock, company, price, vsn}, state = %{count: count}) do
    {:ok, info} =
      Jason.encode(%{
        "stock" => stock,
        "company" => company,
        "price" => :erlang.float_to_binary(price, decimals: 2),
        "version" => vsn,
        "count" => count
      })

    {:reply, {:text, [info, "\n"]}, %{state | count: count + 1}}
  end

  def websocket_handle({:text, _json}, state) do
    {:reply, {:text, "ok"}, state}
  end
end
