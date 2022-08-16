/* 
 * Copyright (c) 2011 Apple Computer, Inc. All Rights Reserved.
 * 
 * @APPLE_LICENSE_HEADER_START@
 * 
 * This file contains Original Code and/or Modifications of Original Code
 * as defined in and that are subject to the Apple Public Source License
 * Version 2.0 (the 'License'). You may not use this file except in
 * compliance with the License. Please obtain a copy of the License at
 * http://www.opensource.apple.com/apsl/ and read it before using this
 * file.
 * 
 * The Original Code and all software distributed under the License are
 * distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR NON-INFRINGEMENT.
 * Please see the License for the specific language governing rights and
 * limitations under the License.
 * 
 * @APPLE_LICENSE_HEADER_END@
 */

/*
 * Adapted to support ravynOS without corecrypto. Modifications
 * Copyright (C) 2022 Zoe Knox <zoe@pixin.net>
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */


// #define COMMON_CMAC_FUNCTIONS

#define CC_CHANGEFUNCTION_28544056_cccmac_init 1

#include  <CommonCrypto/CommonCMACSPI.h>
#include "CommonCryptorPriv.h"
#include "ccdebug.h"

#ifdef __RAVYNOS__
#define CCAES_KEY_SIZE_128 16
#define CCAES_BLOCK_SIZE 16
#include <openssl/evp.h>
#include <openssl/cmac.h>
#else
#include <corecrypto/cccmac.h>
#include <corecrypto/ccaes.h>
#endif

void CCAESCmac(const void *key,
               const uint8_t *data,
               size_t dataLength,			/* length of data in bytes */
               void *macOut)				/* MAC written here */
{
#ifdef __RAVYNOS__
    uint64_t maclen = 0;
    CMAC_CTX *ctx = CMAC_CTX_new();
    CMAC_Init(ctx, key, CCAES_KEY_SIZE_128, EVP_aes_128_cbc(), NULL);
    CMAC_Update(ctx, data, dataLength);
    CMAC_Final(ctx, macOut, &maclen);
    CMAC_CTX_free(ctx);
#else
    cccmac_one_shot_generate(ccaes_cbc_encrypt_mode(),
                              CCAES_KEY_SIZE_128, key,
                              dataLength, data,
                              CCAES_BLOCK_SIZE, macOut);
#endif
}

struct CCCmacContext {
#ifdef __RAVYNOS__
    CMAC_CTX *ctxptr;
#else
    cccmac_ctx_t ctxptr;
#endif
};

CCCmacContextPtr
CCAESCmacCreate(const void *key, size_t keyLength)
{
    // Allocations
    CCCmacContextPtr retval = (CCCmacContextPtr) malloc(sizeof(struct CCCmacContext));
    if(!retval) return NULL;

#ifdef __RAVYNOS__
    retval->ctxptr = CMAC_CTX_new();
#else
    const struct ccmode_cbc *cbc = ccaes_cbc_encrypt_mode();
    retval->ctxptr = malloc(cccmac_ctx_size(cbc));
#endif
    if(retval->ctxptr == NULL) {
        free(retval);
        return NULL;
    }

    // Initialization (key length check)
    if (key==NULL
#ifdef __RAVYNOS__
        || CMAC_Init(retval->ctxptr, key, keyLength, EVP_aes_128_cbc(), NULL) != 0) {
        CMAC_CTX_free(retval->ctxptr);
#else
        || cccmac_init(cbc, retval->ctxptr,
                    keyLength, key)!=0) {
        free(retval->ctxptr);
#endif
        free(retval);
        return NULL;
    }
    
    return retval;
}

void CCAESCmacUpdate(CCCmacContextPtr ctx, const void *data, size_t dataLength) {
#ifdef __RAVYNOS__
    CMAC_Update(ctx->ctxptr, data, dataLength);
#else
    cccmac_update(ctx->ctxptr,dataLength,data);
#endif
}

void CCAESCmacFinal(CCCmacContextPtr ctx, void *macOut) {
#ifdef __RAVYNOS__
    uint64_t maclen = 0;
    CMAC_Final(ctx->ctxptr, macOut, &maclen);
#else
    cccmac_final_generate(ctx->ctxptr, 16, macOut);
#endif
}

void CCAESCmacDestroy(CCCmacContextPtr ctx) {
    if(ctx) {
#ifdef __RAVYNOS__
        CMAC_CTX_free(ctx->ctxptr);
#else
        free(ctx->ctxptr);
#endif
        free(ctx);
    }
}

size_t
CCAESCmacOutputSizeFromContext(CCCmacContextPtr ctx) {
#ifdef __RAVYNOS__
    return CCAES_BLOCK_SIZE;
#else
    return cccmac_cbc(ctx->ctxptr)->block_size;
#endif
}

