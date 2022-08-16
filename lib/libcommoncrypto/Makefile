.include <bsd.init.mk>

LIB=            commoncrypto
MK_WERROR=      no

.PATH: ${.CURDIR}

CFLAGS+=	-I${.CURDIR}/../../include/apple -I${.CURDIR}/include
CFLAGS+=	-I${.CURDIR}/../../include -I${.CURDIR}/lib -I${.CURDIR}/libcn
CFLAGS+=        -I${.CURDIR}/include/Private -fPIC -fblocks
CFLAGS+=	-D__APPLE__ -DUSE_OBJC=0 -D__BLOCKS__=1

SRCS=	./libcn/crc16-xmodem.c \
		./libcn/crc16-ccitt-false.c \
		./libcn/crc16.c \
		./libcn/crc32-castagnoli.c \
		./libcn/crc16-verifone.c \
		./libcn/crc16-dect-x.c \
		./libcn/CommonBuffering.c \
		./libcn/crc16-dect-r.c \
		./libcn/crc32-xfer.c \
		./libcn/reverse_crc.c \
		./libcn/crc8-rohc.c \
		./libcn/crc32-mpeg-2.c \
		./libcn/crc16-ccitt-true.c \
		./libcn/CommonBaseXX.c \
		./libcn/CommonCRC.c \
		./libcn/normal_crc.c \
		./libcn/crc32-posix.c \
		./libcn/crc8-icode.c \
		./libcn/crc8.c \
		./libcn/crc16-a.c \
		./libcn/crc16-usb.c \
		./libcn/adler32.c \
		./libcn/gen_std_crc_table.c \
		./libcn/reverse_poly.c \
		./libcn/crc16-icode.c \
		./libcn/crc32.c \
		./libcn/crc64-ecma-182.c \
		./libcn/crc32-bzip2.c \
		./libcn/crc8-itu.c \
		./libcn/crc16-b.c \
		./libcn/reflect.c \
		./libcn/crc8-wcdma.c \
		./lib/CommonCMAC.c \
		./lib/CommonCryptorGCM.c \
		./lib/CommonRandom.c \
		./lib/CommonDigest.c \
		./lib/CommonKeyDerivation.c \
		./lib/CommonECCryptor.c \
		./lib/ccGlobals.c \
		./lib/CommonCryptorChaCha20.c \
		./lib/CommonSymmetricKeywrap.c \
		./lib/CommonDH.c \
		./lib/timingsafe_bcmp.c \
		./lib/CommonCryptorChaCha20Poly1305.c \
		./lib/CommonCryptor.c \
		./lib/CommonRSACryptor.c \
		./lib/CommonHMAC.c \
		./lib/CommonCryptorDES.c \
		./lib/CommonCollabKeyGen.c \
		./lib/CommonKeyDerivationSPI.c \
		./lib/ccDispatch.c \
		./lib/CommonBigNum.c \
		./lib/dummy.c 

INCS=           ./include/CommonCrypto/CommonKeyDerivation.h \
                ./include/CommonCrypto/CommonCryptor.h \
                ./include/CommonCrypto/CommonDigest.h \
                ./include/CommonCrypto/CommonRandom.h \
                ./include/CommonCrypto/CommonCryptoError.h \
                ./include/CommonCrypto/CommonCrypto.h \
                ./include/CommonCrypto/CommonSymmetricKeywrap.h \
                ./include/CommonCrypto/CommonHMAC.h 

INCSDIR= ${INCLUDEDIR}/CommonCrypto
#CLEANFILES+=

includes: mkincdir installincludes
beforeinstall: mkincdir
mkincdir: .PHONY
	${INSTALL} -d -o ${BINOWN} -g ${BINGRP} -m 0755 ${DESTDIR}${INCSDIR}

.include <rvn.common.mk>
.include <bsd.lib.mk>
