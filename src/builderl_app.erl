-module(builderl_app).
-behaviour(application).
-compile({parse_transform, lager_transform}).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/webhooks/github", builderl_github_webhook, builderl_github_webhook:state_init()},
               {"/projects", builderl_projects_handler, builderl_projects_handler:state_init()},
               {"/builds/:org/:repo", builderl_builds_handler, []},
               {"/builds/:org/:repo/:build", builderl_build_handler, []}]
    }]),
    Port = application:get_env(builderl, http_port, 8080),
    {ok, _} = cowboy:start_clear(my_http_listener, 10,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    lager:info("HTTP listening on port ~p", [Port]),

    case application:get_env(builderl, ssl, false) of
        letsencrypt -> Certs = start_letsencrypt(),
                       start_tls(builderl_routes(), Certs);
        _ -> ok
    end,

	  builderl_sup:start_link().

stop(_State) ->
	  ok.

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

start_letsencrypt() ->
    {ok, _Pid} = letsencrypt:start([{mode,slave}, staging, {cert_path,"/var/lib/builderl/certs"}]),
    MC = letsencrypt:make_cert(<<"builderl.stratobuilder.com">>, #{async => false}),
    {ok, CertMap} = MC,
    lager:info("Lets Encrypt ~p", [MC]),
    CertProps = [{certfile, binary:bin_to_list(maps:get(cert, CertMap))},
                 {key, binary:bin_to_list(maps:get(key, CertMap))}],

    CertProps.
