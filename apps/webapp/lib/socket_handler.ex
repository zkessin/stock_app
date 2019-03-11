defmodule WebApp.SocketHandler do
  @behaviour :cowboy_websocket

  def init(request, _state) do
    state = %{registry_key: request.path}
    {:cowboy_websocket, request, state}
  end

  def websocket_init(state) do
    IO.inspect({state, self()}, libel: "State")
    Price.Server.register(:enron)
    Price.Server.register(:lex_corp)
    Price.Server.register(:acme)
    Price.Server.register(:barings_bank)

    Registry.WebApp
    |> Registry.register(state.registry_key, {})

    {:ok, state}
  end

  def websocket_handle({:text, _json}, state) do
    {:reply, {:text, "ok"}, state}
  end

  def websocket_info({:update, stock, company, price, vsn}, state) do
    info =
      Jason.encode!(%{
        "stock" => stock,
        "company" => company,
        "price" => :erlang.float_to_binary(price, decimals: 2),
        "version" => vsn
      })

    {:reply, {:text, info}, state}
  end

  def websocket_info(info, state) do
    {:reply, {:text, info}, state}
  end
end
