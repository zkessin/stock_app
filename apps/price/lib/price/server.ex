defmodule Price.Server do
  use TypedStruct
  use GenServer

  @typedoc "Internal Structure of the application"
  typedstruct do
    field(:stock, atom(), enforce: true)
    field(:listeners, [pid()], default: [], enforce: true)
    field(:price, integer(), enforce: true)
  end

  def start_link(stock) when is_atom(stock) do
    GenServer.start_link(__MODULE__, stock)
  end

  @impl true
  def init(stock) do
    {:ok, %__MODULE__{stock: stock, listeners: [], price: 100}}
  end

  @impl true
  def handle_call(
        {:register, pid},
        _from,
        state = %__MODULE__{listeners: listeners, price: price}
      ) do
    Proceess.monitor(pid)
    {:reply, {:listen, price}, %__MODULE__{state | listeners: [pid | listeners]}}
  end

  @impl true
  def handle_cast(
        {:delta, diff},
        state = %__MODULE__{listeners: listeners, price: price, stock: stock}
      ) do
    newprice = price + diff
    for p <- listeners, do: send(p, {:update, stock, newprice})
    {:noreply, %__MODULE__{state | price: newprice}}
  end

  @impl true
  def handle_info(
        {:DOWN, _montior_ref, _type, pid, _exit_reason},
        state = %__MODULE__{listeners: listeners}
      ) do
    {:noreply, %__MODULE__{state | listeners: List.delete(listeners, pid)}}
  end
end
