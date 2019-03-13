defmodule Price.Server do
  use TypedStruct
  use GenServer
  alias :gb_sets, as: Set

  @typedoc "Internal Structure of the application"
  typedstruct do
    field(:stock, atom(), enforce: true)
    field(:name, String.t(), enforce: true)
    field(:listeners, Sets.set(pid()), enforce: true)
    field(:price, integer(), enforce: true)
    field(:delta, pid(), enforce: true)
  end

  def version do
    2
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
    {:ok, %__MODULE__{stock: stock, name: name, listeners: Set.empty(), price: 100, delta: delta}}
  end

  @impl true
  def handle_call(
        {:register, pid},
        _from,
        state = %__MODULE__{listeners: listeners, price: price}
      ) do
    Process.monitor(pid)
    {:reply, {:listen, price}, %__MODULE__{state | listeners: Set.add(pid, listeners)}}
  end

  @impl true
  def handle_cast(
        {:delta, diff},
        state = %__MODULE__{listeners: listeners, name: name, price: price, stock: stock}
      ) do
    newprice = price + diff

    Set.fold(
      fn p, _ ->
        send(p, {:update, stock, name, newprice, version()})
        :ok
      end,
      :ok,
      listeners
    )

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
    {:noreply, %__MODULE__{state | listeners: Set.delete(pid, listeners)}}
  end

  @impl true
  def code_change(old_version, state = %__MODULE__{listeners: listeners}, _)
      when is_list(listeners) do
    :io.format(
      "////////////////////////////////////////////////////////////////////////////////~n"
    )

    :io.format("Upgrade from old_version ~p state ~p ", [old_version, state])
    {:ok, %__MODULE__{state | listeners: Set.from_list(listeners)}}
  end

  def code_change(old_version, state, _) do
    :io.format(
      "********************************************************************************~n"
    )

    :io.format("Upgrade Miss from old_version ~p state ~p ~n", [old_version, state])
    {:ok, state}
  end

  def price_loop(pid) do
    Process.sleep(500)
    delta = :rand.normal(0, 0.04)

    GenServer.cast(pid, {:delta, delta})
    __MODULE__.price_loop(pid)
  end
end
