PROJECT = builderl
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy erlgit sh jsx yamerl
LOCAL_DEPS = sasl
SHELL_DEPS = sync
dep_cowboy_commit = 2.0.0-pre.7
dep_erlgit = git https://github.com/gleber/erlgit v0.7.5
dep_sh = git https://github.com/gleber/sh.git master
dep_sync = git https://github.com/rustyio/sync.git master
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.8.2
dep_yamerl = git https://github.com/yakaz/yamerl.git v0.4.0

DEP_PLUGINS = cowboy

SHELL_OPTS = -eval 'application:ensure_all_started(builderl), sync:go().' -config sys

# Building on OmniOS
# CC=gcc CXX="/usr/bin/g++ -m64" PATH=/usr/gnu/bin:/opt/omni/bin/:/opt/gcc-5.1.0/bin:$PATH make app
# with fast_yaml (not working yet CPATH=/opt/omni/lib CPPFLAGS="-I/opt/omni/include/amd64" CFLAGS="-I/opt/omni/include/amd64" LDFLAGS="-L/opt/omni/lib/amd64" CC=gcc CXX="/usr/bin/g++ -m64" PATH=/usr/gnu/bin:/opt/omni/bin/:/opt/gcc-5.1.0/bin:$PATH make deps app shell
#
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
endef
export IPS_METADATA

package: rel
	rm -rf ${BUILDDIR} ${BUILDTMP}
	mkdir -p ${BUILDDIR}/opt/ ${BUILDTMP}
	cp -R _rel/builderl_release ${BUILDDIR}/opt/builderl

	# SMF
	mkdir -p ${BUILDDIR}/lib/svc/manifest/application/
	cp smf.xml ${BUILDDIR}/lib/svc/manifest/application/builderl.xml

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
