defmodule Price.Server do
  use TypedStruct
  use GenServer

  @typedoc "Internal Structure of the application"
  typedstruct do
    field(:stock, atom(), enforce: true)
    field(:name, String.t(), enforce: true)
    field(:listeners, [pid()], default: [], enforce: true)
    field(:price, integer(), enforce: true)
    field(:delta, pid(), enforce: true)
  end

  def register(stock) do
    GenServer.call(stock, {:register, self()})
  end

  def start_link([stock, name]) when is_atom(stock) do
    GenServer.start_link(__MODULE__, [stock, name], name: stock)
  end

  @impl true
  def init([stock, name]) do
    Process.flag(:trap_exit, true)
    delta = spawn_link(__MODULE__, :price_loop, [self()])
    {:ok, %__MODULE__{stock: stock, name: name, listeners: [], price: 100, delta: delta}}
  end

  @impl true
  def handle_call(
        {:register, pid},
        _from,
        state = %__MODULE__{listeners: listeners, price: price}
      ) do
    Process.monitor(pid)
    {:reply, {:listen, price}, %__MODULE__{state | listeners: [pid | listeners]}}
  end

  @impl true
  def handle_cast(
        {:delta, diff},
        state = %__MODULE__{listeners: listeners, name: name, price: price, stock: stock}
      ) do
    newprice = price + diff
    for p <- listeners, do: send(p, {:update, stock, name, newprice})
    {:noreply, %__MODULE__{state | price: newprice}}
  end

  @impl true
  def handle_info({:EXIT, from, _reason}, state = %__MODULE__{delta: from}) do
    delta = spawn_link(__MODULE__, :price_loop, [self()])
    {:noreply, %__MODULE__{state | delta: delta}}
  end

  def handle_info(
        {:DOWN, _montior_ref, _type, pid, _exit_reason},
        state = %__MODULE__{listeners: listeners}
      ) do
    {:noreply, %__MODULE__{state | listeners: List.delete(listeners, pid)}}
  end

  def price_loop(pid) do
    Process.sleep(100)
    delta = Enum.random(-5..5)
    GenServer.cast(pid, {:delta, delta})
    __MODULE__.price_loop(pid)
  end
end
