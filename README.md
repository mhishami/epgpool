Introduction
============

This is a simple PostgreSQL library for reading and writing to PostgreSQL.
It has poolboy configured for various configuration imaginable.

For example, in the configuration below:

``` erlang
{env, [
  {pools, [
    {write, [
      {size, 5},
      {max_overflow, 20}
    ], [
      {hostname, "localhost"},
      {database, "test"},
      {username, "hisham"},
      {password, "sa"}
    ]},
    {read, [
      {size, 10},
      {max_overflow, 20}
    ], [
      {hostname, "localhost"},
      {database, "test"},
      {username, "hisham"},
      {password, "sa"}
    ]}
  ]}
]}
```

There are 2 pools configured. One for read, named `read`, and one for write, named `write`.
You can use various other combinations when you have multi-master and slave setup.

Use In App
----------

When you use it in your app, ensure that all the deps are loaded in your application.

``` erlang
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start() ->
	ok = ensure_started(crypto),
    ok = ensure_started(poolboy),
    ok = ensure_started(epgpool).
    
stop() ->
    application:stop(epgpool),
    ok.
```

Example
-------

A simple example would be like this, but do browse through the samples in epgsql deps directory.

``` erlang
epgpool:squery(read, <<"SELECT * FROM users">>).

epgpool:equery(write, 
  <<"INSERT INTO users (username, password) VALUES ($1, $2)">>,
  [ <<"foo">>, <<"bar">>]).

```


Author
======

- Hisham Ismail <mhishami@gmail.com>

License
=======
`epgpool` is available in the public domain (see `UNLICENSE`).
`epgpool` is also optionally available under the Apache License (see `LICENSE`),
meant especially for jurisdictions that do not recognize public domain works.