defmodule WebApp do
  use Application

  def start(_type, _args) do
    children = [
      Plug.Cowboy.child_spec(
        scheme: :http,
        plug: WebApp.Router,
        options: [
          dispatch: dispatch(),
          port: 80
        ]
      ),
      Registry.child_spec(
        keys: :duplicate,
        name: Registry.WebApp
      )
    ]

    opts = [strategy: :one_for_one, name: WebApp.Application]
    Supervisor.start_link(children, opts)
  end

  defp dispatch do
    [
      {:_,
       [
         {"/assets/[...]", :cowboy_static, {:priv_dir, :webapp, "static/assets"}},
         {"/js/[...]", :cowboy_static, {:priv_dir, :webapp, "static/js"}},
         {"/ws/[...]", WebApp.SocketHandler, []},
         {:_, Plug.Cowboy.Handler, {WebApp.Router, []}}
       ]}
    ]
  end
end
