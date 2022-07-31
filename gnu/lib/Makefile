# $FreeBSD$

.include <src.opts.mk>

.if "${MK_OBJC}" != "no"
SUBDIR=	libobjc2
.endif

SUBDIR.${MK_DIALOG}+=	libdialog
SUBDIR.${MK_TESTS}+=	tests

SUBDIR_PARALLEL=

.include <bsd.subdir.mk>
