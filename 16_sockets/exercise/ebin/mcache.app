{
    application,
    mcache,
    [
        {description, "mcache"},
        {vsn, "0.0.1"},
        {modules, [mcache_app, mcache_sup, mcache_srv, mcache]},
        {registered, []},
        {applications, [kernel, stdlib]},
        {mod, {mcache_app, []}},
        {env,
            [
                {port, 1234},
                {accept_pool_size, 5}
            ]
        }
    ]
}.