release:
	MIX_ENV=prod mix release
	cp -r ./_build/prod/rel/stock_app/releases/* release/
