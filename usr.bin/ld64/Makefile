# $FreeBSD$

.include <src.opts.mk>

PROG_CXX=	ld64
MAN=
# Man page directory
.PATH:		${.CURDIR}/docs/man
.PATH:          ${.CURDIR}/src

CFLAGS+=	-Isrc/abstract -Isrc/ld -Isrc/other
CFLAGS+=        -I/usr/include/apple

SRCS+=		ld/OutputFile.cpp
SRCS+=		ld/ld.cpp
SRCS+=		ld/PlatformSupport.cpp
SRCS+=		ld/parsers/macho_relocatable_file.cpp
SRCS+=		ld/parsers/archive_file.cpp
SRCS+=		ld/parsers/opaque_section_file.cpp
SRCS+=		ld/parsers/textstub_dylib_file.cpp
SRCS+=		ld/parsers/lto_file.cpp
SRCS+=		ld/parsers/macho_dylib_file.cpp
SRCS+=		ld/parsers/generic_dylib_file.cpp
SRCS+=		ld/InputFiles.cpp
SRCS+=		ld/debugline.c
SRCS+=		ld/code-sign-blobs/blob.cpp
SRCS+=		ld/SymbolTable.cpp
SRCS+=		ld/ResponseFiles.cpp
SRCS+=		ld/passes/bitcode_bundle.cpp
SRCS+=		ld/passes/got.cpp
SRCS+=		ld/passes/huge.cpp
SRCS+=		ld/passes/thread_starts.cpp
SRCS+=		ld/passes/branch_shim.cpp
SRCS+=		ld/passes/tlvp.cpp
SRCS+=		ld/passes/stubs/stubs.cpp
SRCS+=		ld/passes/order.cpp
SRCS+=		ld/passes/dtrace_dof.cpp
SRCS+=		ld/passes/inits.cpp
SRCS+=		ld/passes/dylibs.cpp
SRCS+=		ld/passes/compact_unwind.cpp
SRCS+=		ld/passes/code_dedup.cpp
SRCS+=		ld/passes/branch_island.cpp
SRCS+=		ld/passes/objc.cpp
SRCS+=		ld/passes/objc_constants.cpp
SRCS+=		ld/Snapshot.cpp
SRCS+=		ld/Resolver.cpp
SRCS+=		ld/libcodedirectory.c
SRCS+=		ld/Options.cpp
SRCS+=		other/dyldinfo.cpp
SRCS+=		other/machochecker.cpp
SRCS+=		other/ObjectDump.cpp
SRCS+=		other/unwinddump.cpp
SRCS+=		other/objcimageinfo.cpp
SRCS+=		other/PruneTrie.cpp

LIBDEPS+=	llvm

.for lib in ${LIBDEPS}
DPADD+=		${OBJTOP}/lib/clang/lib${lib}/lib${lib}.a
LDADD+=		${OBJTOP}/lib/clang/lib${lib}/lib${lib}.a
.endfor

TGHDRS+=	${INCFILE}

DEPENDFILES+=	${TGHDRS:C/$/.d/}
DPSRCS+=	${TGHDRS}
CLEANFILES+=	${TGHDRS} ${TGHDRS:C/$/.d/}

.if ${.MAKE.OS} == "FreeBSD" || !defined(BOOTSTRAPPING)
LIBADD+=	execinfo
LIBADD+=	tinfow
.endif
LIBADD+=	pthread
LIBADD+=	z

.include <bsd.prog.mk>

