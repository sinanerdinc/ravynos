# $FreeBSD$

.include <src.opts.mk>

LLVM_BASE=	${SRCTOP}/contrib/llvm-project
LLVM_SRCS=	${LLVM_BASE}/llvm
LLD_SRCS=	${LLVM_BASE}/lld

PACKAGE=	lld
PROG_CXX=	ld64
MAN=
MLINKS=		ld.lld.1 ld64.1
# Man page directory
.PATH:		${LLD_SRCS}/docs
.if (!defined(TOOLS_PREFIX) && ${MK_LLD_IS_LD} != "no") || \
    (defined(TOOLS_PREFIX) && ${MK_LLD_BOOTSTRAP} != "no")
#SYMLINKS=	${PROG_CXX} ${BINDIR}/ld
.endif

.if ${MK_SHARED_TOOLCHAIN} == "no"
NO_SHARED?= yes
.endif

.include "${SRCTOP}/lib/clang/llvm.pre.mk"

CFLAGS+=	-I${LLD_SRCS}/MachO
CFLAGS+=	-I${LLD_SRCS}/include
CFLAGS+=	-I${.OBJDIR}
CFLAGS+=	-I${OBJTOP}/lib/clang/libllvm
CFLAGS+=        -I${LLD_SRCS}/../libunwind/include/
CFLAGS+=        -D__ld64__

SRCDIR=		lld
SRCS+=		Common/Args.cpp
SRCS+=		Common/CommonLinkerContext.cpp
SRCS+=		Common/DWARF.cpp
SRCS+=		Common/ErrorHandler.cpp
SRCS+=		Common/Filesystem.cpp
SRCS+=		Common/Memory.cpp
SRCS+=		Common/Reproduce.cpp
SRCS+=		Common/Strings.cpp
SRCS+=		Common/TargetOptionsCommandFlags.cpp
SRCS+=		Common/Version.cpp

SRCS+=		MachO/Arch/ARM.cpp
SRCS+=		MachO/Arch/ARM64_32.cpp
SRCS+=		MachO/Arch/ARM64.cpp
SRCS+=		MachO/Arch/ARM64Common.cpp
SRCS+=		MachO/Arch/X86_64.cpp
SRCS+=		MachO/OutputSegment.cpp
SRCS+=		MachO/SyntheticSections.cpp
SRCS+=		MachO/ConcatOutputSection.cpp
SRCS+=		MachO/ICF.cpp
SRCS+=		MachO/MapFile.cpp
SRCS+=		MachO/Relocations.cpp
SRCS+=		MachO/Target.cpp
SRCS+=		MachO/InputFiles.cpp
SRCS+=		MachO/MarkLive.cpp
SRCS+=		MachO/Driver.cpp
SRCS+=		MachO/SectionPriorities.cpp
SRCS+=		MachO/UnwindInfoSection.cpp
SRCS+=		MachO/InputSection.cpp
SRCS+=		MachO/ObjC.cpp
SRCS+=		MachO/DriverUtils.cpp
SRCS+=		MachO/Symbols.cpp
SRCS+=		MachO/Writer.cpp
SRCS+=		MachO/Dwarf.cpp
SRCS+=		MachO/LTO.cpp
SRCS+=		MachO/OutputSection.cpp
SRCS+=		MachO/SymbolTable.cpp
SRCS+=		MachO/ExportTrie.cpp

SRCS+=		tools/lld/lld.cpp

.include "${SRCTOP}/lib/clang/llvm.build.mk"

LIBDEPS+=	llvm

.for lib in ${LIBDEPS}
DPADD+=		${OBJTOP}/lib/clang/lib${lib}/lib${lib}.a
LDADD+=		${OBJTOP}/lib/clang/lib${lib}/lib${lib}.a
.endfor

INCFILE=	Options.inc
TDFILE=		${LLD_SRCS}/MachO/Options.td
GENOPT=		-gen-opt-parser-defs
${INCFILE}: ${TDFILE}
	${LLVM_TBLGEN} ${GENOPT} -I ${LLVM_SRCS}/include -d ${.TARGET:C/$/.d/} \
	    -o ${.TARGET} ${TDFILE}
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
