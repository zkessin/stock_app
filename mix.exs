defmodule StockApp.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Dependencies listed here are available only for this
  # project and cannot be accessed from applications inside
  # the apps folder.
  #
  # Run "mix help deps" for examples and options.
  defp deps do
    [
      {:distillery, "~> 2.0"},
      {:typed_struct, "~> 0.1.4"},
      {:dialyxir, "~> 0.5.1"},
      {:propcheck, "~> 1.1"},
      {:cowboy, "~> 2.6"},
      {:recon_ex, "~> 0.9.1"}
    ]
  end
end
