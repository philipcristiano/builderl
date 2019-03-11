PROJECT = builderl
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy erlgit sh jsx yamerl lager uuid erlydtl letsencrypt jiffy egithub urilib hackney
BUILD_DEPS = elvis_mk
LOCAL_DEPS = sasl
SHELL_DEPS = sync
TEST_DEPS = meck
CT_OPTS ?= -create_priv_dir auto_per_tc

IPS_DEPS = pkg:/developer/versioning/git \
					 pkg:/developer/build/gnu-make

dep_cowboy_commit = 2.0.0-pre.9
dep_erlgit = git https://github.com/gleber/erlgit v0.7.5
dep_hackney = git https://github.com/benoitc/hackney.git 1.11.0
dep_sh = git https://github.com/gleber/sh.git master
dep_sync = git https://github.com/rustyio/sync.git master
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.8.2
dep_oauth2_client = git https://github.com/kivra/oauth2_client.git 1.0.1
dep_yamerl = git https://github.com/yakaz/yamerl.git v0.4.0
dep_lager = git https://github.com/erlang-lager/lager.git 3.6.8
dep_uuid = git https://github.com/avtobiff/erlang-uuid.git v0.5.1
dep_erlydtl = git https://github.com/philipcristiano/erlydtl.git master
dep_letsencrypt = git https://github.com/philipcristiano/letsencrypt-erlang.git cacert-file
# Get around ELFCLASS errors with older specified version
dep_jiffy = git https://github.com/davisp/jiffy.git 0.14.11
dep_egithub = git https://github.com/inaka/erlang-github.git 0.5.2
dep_urilib = git https://github.com/gmr/urilib.git 0.3.0

dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0
dep_meck = git https://github.com/eproxus/meck.git 0.8.9

DEP_PLUGINS = cowboy elvis_mk

SHELL_OPTS = -eval 'application:ensure_all_started(builderl), sync:go().' -config sys
DTL_OPTS = force_recompile, {libraries, [{builderl_filters, builderl_dtl_filters}]}, {default_libraries, [builderl_filters]}

script_modules = static/scripts/jquery-3.3.1.js \
							   static/scripts/bootstrap.bundle.js \
								 static/scripts/builderl.js
style_modules = static/styles/bootstrap-4.0.0.css

priv/static:
	mkdir -p priv/static

priv/static/script.js: ${script_modules}
	cat $^ > $@

priv/static/style.css: ${style_modules}
	cat $^ > $@

.PHONY: static
static: priv/static priv/static/script.js priv/static/style.css
	@echo 'DONE'

package: ips-prototype
	# Builderl runtime directories
	mkdir -p ${IPS_BUILD_DIR}/var/lib/builderl/builds
	mkdir -p ${IPS_BUILD_DIR}/var/lib/builderl/build_logs
	mkdir -p ${IPS_BUILD_DIR}/var/lib/builderl/certs

	# SMF
	mkdir -p ${IPS_BUILD_DIR}/lib/svc/manifest/application/
	cp smf.xml ${IPS_BUILD_DIR}/lib/svc/manifest/application/${PROJECT}.xml
	cp epmd.xml ${IPS_BUILD_DIR}/lib/svc/manifest/application/epmd.xml

	# Config
	cp omnios.config "${IPS_BUILD_DIR}/etc/${PROJECT}.config"
	$(call add-ips-transform, "<transform file path=etc/builderl.config -> add preserve true>")

publish: ips-package
ifndef PKGSRVR
	echo "Need to define PKGSRVR, something like http://localhost:10000"
	exit 1
endif
	pkgsend publish -s ${PKGSRVR} -d ${IPS_BUILD_DIR} ${IPS_TMP_DIR}/pkg.pm5.final
	pkgrepo refresh -s ${PKGSRVR}

.PHONY:test
test: tests

include erlang.mk
include erlang-ips.mk
