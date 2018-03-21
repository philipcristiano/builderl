IPS_BUILD_DIR ?= erlang-ips
IPS_TMP_DIR ?= tmp

IPS_BUILD_TIME=$(shell TZ=UTC date +"%Y%m%dT%H%M%SZ")
export IPS_FMRI=server/${PROJECT}@${PROJECT_VERSION}:${IPS_BUILD_TIME}
export IPS_DESCRIPTION=${PROJECT_DESCRIPTION}
export IPS_SUMMARAY=${IPS_DESCRIPTION}
#PKG_VERSION	?= $(shell git describe --tags | tr - .)

ARCH=$(shell uname -p)

define IPS_METADATA
set name=pkg.fmri value=${IPS_FMRI}
set name=pkg.description value="${IPS_DESCRIPTION}"
set name=pkg.summary value="${IPS_SUMMARAY}"
set name=variant.arch value=${ARCH}
group groupname=${PROJECT}
user username=${PROJECT} group=${PROJECT} home-dir=/opt/${PROJECT}
endef
export IPS_METADATA

ips-clean:
	rm -rf ${IPS_BUILD_DIR} ${IPS_TMP_DIR}

ips-prototype: rel
	mkdir -p ${IPS_BUILD_DIR}/opt/ ${IPS_TMP_DIR} "${IPS_BUILD_DIR}/etc"
	cp -R _rel/${PROJECT}_release ${IPS_BUILD_DIR}/opt/${PROJECT}
	rm -rf ${IPS_BUILD_DIR}/opt/${PROJECT}/${PROJECT}_release-*.tar.gz

	cp LICENSE ${IPS_BUILD_DIR}/

	# Store initial transform
	echo "$$IPS_TRANSFORM" > ${IPS_TMP_DIR}/transform.mog

ips-package: ips-prototype
	pkgsend generate ${IPS_BUILD_DIR} | pkgfmt > ${IPS_TMP_DIR}/pkg.pm5.1
	# Store metadata into a file
	echo "$$IPS_METADATA" > ${IPS_TMP_DIR}/pkg.mog
	for dep in ${IPS_DEPS}; do \
		echo "depend type=require fmri=$$dep" >> tmp/pkg.mog ; \
	done

	pkgmogrify ${IPS_TMP_DIR}/pkg.pm5.1 ${IPS_TMP_DIR}/pkg.mog ${IPS_TMP_DIR}/transform.mog | pkgfmt > ${IPS_TMP_DIR}/pkg.pm5.final

	pkglint ${IPS_TMP_DIR}/pkg.pm5.final

define IPS_TRANSFORM
<transform dir path=usr$$ -> drop>
<transform dir path=lib$$ -> drop>
<transform dir path=lib/svc$$ -> drop>
<transform dir path=lib/svc/manifest$$ -> drop>
<transform dir path=lib/svc/manifest/application$$ -> drop>
<transform dir path=opt$$ -> drop>
<transform dir path=etc$$ -> drop>
<transform dir path=var$$ -> drop>
<transform dir path=var/lib$$ -> drop>
<transform file path=(var|lib)/svc/manifest/.*\.xml$ -> default restart_fmri svc:/system/manifest-import:default>
<transform file dir -> set owner builderl>
<transform file dir -> set group builderl>
endef
export IPS_TRANSFORM

define add-ips-transform
echo $(1) >> ${IPS_TMP_DIR}/transform.mog
endef
