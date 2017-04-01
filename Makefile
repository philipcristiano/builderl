PROJECT = builderl
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy erlexec erlgit sh yamler
SHELL_DEPS = sync
dep_cowboy_commit = 2.0.0-pre.7
dep_erlexec = git https://github.com/saleyn/erlexec.git 1.6.4
dep_erlgit = git https://github.com/gleber/erlgit v0.7.5
dep_sh = git https://github.com/gleber/sh.git master
dep_yamler = git https://github.com/goertzenator/yamler.git master
dep_sync = git https://github.com/rustyio/sync.git master

DEP_PLUGINS = cowboy

include erlang.mk
