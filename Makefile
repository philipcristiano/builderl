PROJECT = builderl
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy erlgit sh jsx yamerl lager uuid erlydtl letsencrypt jiffy
LOCAL_DEPS = sasl
SHELL_DEPS = sync
TEST_DEPS = meck

dep_cowboy_commit = 2.0.0-pre.9
dep_erlgit = git https://github.com/gleber/erlgit v0.7.5
dep_sh = git https://github.com/gleber/sh.git master
dep_sync = git https://github.com/rustyio/sync.git master
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.8.2
dep_yamerl = git https://github.com/yakaz/yamerl.git v0.4.0
dep_lager = git https://github.com/erlang-lager/lager.git 3.4.1
dep_uuid = git https://github.com/avtobiff/erlang-uuid.git v0.5.0
dep_erlydtl = git https://github.com/erlydtl/erlydtl.git 0.12.1
dep_letsencrypt = git https://github.com/philipcristiano/letsencrypt-erlang.git cacert-file
# Get around ELFCLASS errors with older specified version
dep_jiffy = git https://github.com/davisp/jiffy.git 0.14.11

dep_meck = git https://github.com/eproxus/meck.git 0.8.4

DEP_PLUGINS = cowboy

SHELL_OPTS = -eval 'application:ensure_all_started(builderl), sync:go().' -config sys
DTL_OPTS = force_recompile, {libraries, [{builderl_filters, builderl_dtl_filters}]}, {default_libraries, [builderl_filters]}

# OmniOS build/packaging
BUILDDIR = build
BUILDTMP = tmp

BUILD_TIME=$(shell TZ=UTC date +"%Y%m%dT%H%M%SZ")
export IPS_FMRI=server/${PROJECT}@${PROJECT_VERSION}:${BUILD_TIME}
export IPS_DESCRIPTION=${PROJECT_DESCRIPTION}
export IPS_SUMMARAY=${IPS_DESCRIPTION}
#PKG_VERSION	?= $(shell git describe --tags | tr - .)
ARCH=$(shell uname -p)

define IPS_METADATA
set name=pkg.fmri value=${IPS_FMRI}
set name=pkg.description value="${IPS_DESCRIPTION}"
set name=pkg.summary value="${IPS_SUMMARAY}"
set name=variant.arch value=${ARCH}
depend type=require fmri=pkg:/developer/versioning/git
depend type=require fmri=pkg:/developer/build/gnu-make
group groupname=builderl
user username=builderl group=builderl home-dir=/opt/builderl
endef
export IPS_METADATA

package: rel
	rm -rf ${BUILDDIR} ${BUILDTMP}
	mkdir -p ${BUILDDIR}/opt/ ${BUILDTMP} "${BUILDDIR}/etc"

	cp -R _rel/builderl_release ${BUILDDIR}/opt/builderl
	rm ${BUILDDIR}/opt/builderl/builderl_release-*.tar.gz

	# Builderl runtime directories
	mkdir -p ${BUILDDIR}/var/lib/builderl/builds
	mkdir -p ${BUILDDIR}/var/lib/builderl/build_logs
	mkdir -p ${BUILDDIR}/var/lib/builderl/certs

	# Config
	cp omnios.config "${BUILDDIR}/etc/builderl.config"

	# SMF
	mkdir -p ${BUILDDIR}/lib/svc/manifest/application/
	cp smf.xml ${BUILDDIR}/lib/svc/manifest/application/builderl.xml
	cp epmd.xml ${BUILDDIR}/lib/svc/manifest/application/epmd.xml

	pkgsend generate build | pkgfmt > ${BUILDTMP}/pkg.pm5.1
	cp LICENSE ${BUILDDIR}/

	# Store metadata into a file
	echo "$$IPS_METADATA" > ${BUILDTMP}/pkg.mog

	pkgmogrify ${BUILDTMP}/pkg.pm5.1 ${BUILDTMP}/pkg.mog transform.mog | pkgfmt > ${BUILDTMP}/pkg.pm5.final


	pkglint ${BUILDTMP}/pkg.pm5.final

publish:
ifndef PKGSRVR
	echo "Need to define PKGSRVR, something like http://localhost:10000"
	exit 1
endif
	pkgsend publish -s ${PKGSRVR} -d ${BUILDDIR} ${BUILDTMP}/pkg.pm5.final
	pkgrepo refresh -s ${PKGSRVR}

include erlang.mk
