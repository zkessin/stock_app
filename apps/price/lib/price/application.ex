defmodule Price.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    # List all child processes to be supervised
    children = [
      # Starts a worker by calling: Price.Worker.start_link(arg)
      # {Price.Worker, arg}
      Supervisor.child_spec({Price.Server, [:enron, "Enron"]}, id: :enron),
      Supervisor.child_spec({Price.Server, [:barings_bank, "Barings Bank"]}, id: :barings_bank),
      Supervisor.child_spec({Price.Server, [:lex_corp, "Lex Corp"]}, id: :lex_corp),
      Supervisor.child_spec({Price.Server, [:acme, "Acme"]}, id: :acme)
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Price.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
