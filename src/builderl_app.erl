-module(builderl_app).
-behaviour(application).
-compile({parse_transform, lager_transform}).

-include_lib("public_key/include/public_key.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    SU = should_update_cert("crt.crt"),
    ok = lager:info("Key info ~p", [SU]),
    SU2 = should_update_cert("crt2.crt"),
    ok = lager:info("Key info ~p", [SU2]),
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
    ok = lager:info("HTTP listening on port ~p", [Port]),

    Certs = start_letsencrypt(),
    ok = start_tls(builderl_routes(), Certs).

setup_cowboy_http() ->
    Dispatch = cowboy_router:compile([{'_', builderl_routes()}]),

    Port = application:get_env(builderl, http_port, 8080),
    {ok, _} = cowboy:start_clear(http, 20,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    ok = lager:info("HTTP listening on port ~p", [Port]).

start_tls(Routes, Certs) ->
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    Options = [{port, 443}] ++ Certs,
    ok = lager:info("Starting TLS with options ~p", [Options]),
    {ok, _} = cowboy:start_tls(https, 10, Options,
                               #{env => #{dispatch => Dispatch}}),
    ok.

builderl_routes() ->
    [{"/static/[...]", cowboy_static, {priv_dir, builderl, "static"}},
     {"/webhooks/github", builderl_github_webhook, builderl_github_webhook:state_init()},
     {"/github_login", builderl_github_login_handler, []},
     {"/gh-auth", builderl_github_auth_handler, []},
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
    {ok, CertDir} = application:get_env(builderl, certificates_directory),
    CertFile = CertDir ++ "/" ++ Domain ++ ".crt",

    CertProps = case should_update_cert(CertFile) of
        true -> BDomain = binary:list_to_bin(Domain),
                {ok, _Pid} = letsencrypt:start([{mode, slave}, {cert_path, CertDir}]),
                MC = letsencrypt:make_cert(BDomain, #{async => false}),
                {ok, CertMap} = MC,
                ok = lager:info("Lets Encrypt ~p", [MC]),
                [{certfile, binary:bin_to_list(maps:get(cert, CertMap))},
                 {cacertfile, binary:bin_to_list(maps:get(cacert, CertMap))},
                 {keyfile, binary:bin_to_list(maps:get(key, CertMap))}];
        false -> [{certfile, CertDir ++ "/" ++ Domain ++ ".crt"},
                  {cacertfile, CertDir ++ "/" ++ Domain ++ ".ca.crt"},
                  {keyfile, CertDir ++ "/" ++ Domain ++ ".key"}]
    end,

    CertProps.

should_update_cert(Path) ->
    File = file:read_file(Path),
    should_update_cert_from_file(File).

should_update_cert_from_file({error, enoent}) ->
    true;
should_update_cert_from_file({ok, File}) ->
    [Cert1| _] = public_key:pem_decode(File),
    CertData = public_key:pem_entry_decode(Cert1),
    TBSC = CertData#'Certificate'.tbsCertificate,
    Validity = TBSC#'TBSCertificate'.validity,
    {utcTime, ValidUntil} = Validity#'Validity'.notAfter,

    SY = string:sub_string(ValidUntil, 1, 2),
    SM = string:sub_string(ValidUntil, 3, 4),
    SD = string:sub_string(ValidUntil, 5, 6),

    {IY, _} = string:to_integer(SY),
    {IM, _} = string:to_integer(SM),
    {ID, _} = string:to_integer(SD),

    ValidUntilDate = {IY + 2000, IM, ID},
    ValidUntilDay = calendar:date_to_gregorian_days(ValidUntilDate),
    {Today, _} = calendar:universal_time(),
    DaysUntilToday = calendar:date_to_gregorian_days(Today),
    DaysRemaining =  ValidUntilDay - DaysUntilToday,

    DaysRemaining < 31.
