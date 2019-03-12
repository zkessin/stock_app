release:
	MIX_ENV=prod mix release
	cp ./_build/prod/rel/stock_app/releases/0.1.0/stock_app.tar.gz release/
	cp ./_build/prod/rel/stock_app/releases/0.1.0/stock_app.tar.gz $(HOME)
