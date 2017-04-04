PROJECT = builderl
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy erlexec erlgit sh yamler
SHELL_DEPS = sync
dep_cowboy_commit = 2.0.0-pre.7
dep_erlexec = git https://github.com/saleyn/erlexec.git 1.2.2
dep_erlgit = git https://github.com/gleber/erlgit v0.7.5
dep_sh = git https://github.com/gleber/sh.git master
dep_yamler = git https://github.com/goertzenator/yamler.git master
dep_sync = git https://github.com/rustyio/sync.git master

DEP_PLUGINS = cowboy

SHELL_OPTS = -eval 'application:ensure_all_started(builderl), sync:go().'

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
