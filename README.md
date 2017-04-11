# Builderl

A build server to take Github webhooks and kick off builds from repo-embedded YAML files.

## Local Development

Builderl relies on a `local.config` file. Run `cp local.config.example local.config` to get
started.

`make deps app shell` will start a server.

### Building on OmniOS

```
pkg install pkg:/omniti/runtime/erlang@19.0.0
pkg install pkg:/developer/versioning/git
pkg install pkg:/developer/build/gnu-make
pkg install pkg:/developer/gcc51
pkg install pkg:/system/header@0.5.11

CC=gcc CXX="/usr/bin/g++ -m64" PATH=/usr/gnu/bin:/opt/omni/bin/:/opt/gcc-5.1.0/bin:$PATH make deps app


```

### Bootstrapping

```
PKGSRVR='$SERVER' CC=gcc CXX="/usr/bin/g++ -m64" PATH=/usr/gnu/bin:/opt/omni/bin/:/opt/gcc-5.1.0/bin:$PATH make shell
```

Then post in a build file

```
curl -X POST -d '@example_github_push.json' $HOST:8080/webhooks/github -v
```

## Configuration

Configuration goes into the `local.config` file or (eventually) when deployed an `/etc/builderl.config` file.

`global_env` - List of tuples specifying global environment settings. If `keep` is used as the value then the current environment variable will be carried over to the build.

Example:

```
{global_env, [{"PATH", keep},
              {"FOO", "BAR"}]}
```
