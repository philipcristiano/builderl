-module(builderl_app).
-behaviour(application).
-compile({parse_transform, lager_transform}).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    case application:get_env(builderl, ssl, false) of
        letsencrypt -> setup_cowboy_with_letsencrypt();
        _ -> setup_cowboy_http()
    end,

	  builderl_sup:start_link().

stop(_State) ->
	  ok.

setup_cowboy_with_letsencrypt() ->
    Dispatch = cowboy_router:compile([{'_', acme_routes() ++ https_redirect_routes()}]),

    Port = 80,
    {ok, _} = cowboy:start_clear(http, 10,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    lager:info("HTTP listening on port ~p", [Port]),

    Certs = start_letsencrypt(),
    start_tls(builderl_routes(), Certs).

setup_cowboy_http() ->
    Dispatch = cowboy_router:compile([{'_', builderl_routes()}]),

    Port = application:get_env(builderl, http_port, 8080),
    {ok, _} = cowboy:start_clear(http, 20,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    lager:info("HTTP listening on port ~p", [Port]).

start_tls(Routes, Certs) ->
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    Options = [{port, 443}] ++ Certs,
    lager:info("Starting TLS with options ~p", [Options]),
    {ok, _} = cowboy:start_tls(https, 10, Options,
                               #{env => #{dispatch => Dispatch}}).

builderl_routes() ->
    [{"/webhooks/github", builderl_github_webhook, builderl_github_webhook:state_init()},
     {"/projects", builderl_projects_handler, builderl_projects_handler:state_init()},
     {"/builds/:org/:repo", builderl_builds_handler, []},
     {"/builds/:org/:repo/:build", builderl_build_handler, []}].

acme_routes() ->
    [{<<"/.well-known/acme-challenge/:token">>, letsencrypt_cowboy_handler, []}].

https_redirect_routes() ->
    {ok, Domain} = application:get_env(builderl, domain),
    [{'_', builderl_https_redirect_handler, [{domain, Domain}]}].

start_letsencrypt() ->
    {ok, Domain} = application:get_env(builderl, domain),
    BDomain = binary:list_to_bin(Domain),
    {ok, _Pid} = letsencrypt:start([{mode,slave}, {cert_path,"/var/lib/builderl/certs"}]),
    MC = letsencrypt:make_cert(BDomain, #{async => false}),
    {ok, CertMap} = MC,
    lager:info("Lets Encrypt ~p", [MC]),
    CertProps = [{certfile, binary:bin_to_list(maps:get(cert, CertMap))},
                 {keyfile, binary:bin_to_list(maps:get(key, CertMap))}],

    CertProps.
