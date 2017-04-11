# Builderl

A build server to take Github webhooks and kick off builds from repo-embedded YAML files.

## Local Development

Builderl relies on a `local.config` file. Run `cp local.config.example local.config` to get
started.

`make deps app shell` will start a server.

## Configuration

Configuration goes into the `local.config` file or (eventually) when deployed an `/etc/builderl.config` file.

`global_env` - List of tuples specifying global environment settings. If `keep` is used as the value then the current environment variable will be carried over to the build.

Example:

```
{global_env, [{"PATH", keep},
              {"FOO", "BAR"}]}
```
