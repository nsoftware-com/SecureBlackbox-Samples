/*
 * SecureBlackbox 2022 C++ Edition - Sample Project
 *
 * This sample project demonstrates the usage of SecureBlackbox in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/secureblackbox
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../include/secureblackbox.h"
#define LINE_LEN 100

namespace ArgParser {
    static char* optval(int argc, char** argv, const char* option) {
        for (int x = 0; x < argc - 1; x++) {
            if (!strcmp(argv[x], option)) {
                return argv[x + 1];
            }
        }
        return (char*)"";
    }

    static bool optext(int argc, char** argv, const char* option) {
        for (int x = 0; x < argc; x++) {
            if (!strcmp(argv[x], option)) {
                return true;
            }
        }
        return false;
    }
};

using namespace ArgParser;

char* keyFile;
CryptoKeyManager keymanager;
PublicKeyCrypto crypto;

unsigned char* hexstr_to_char(const char* hexstr) {
    const size_t len = strlen(hexstr);
    const size_t final_len = len / 2;
    unsigned char* chrs = (unsigned char*)malloc((final_len + 1) * sizeof(*chrs));
    for (size_t i = 0, j = 0; j < final_len; i += 2, j++) {
        chrs[j] = (hexstr[i] % 32 + 9) % 25 * 16 + (hexstr[i + 1] % 32 +
            9) % 25;
    }
    chrs[final_len] = '\0';
    return chrs;
}

char* barray2hexstr(const unsigned char* data, size_t datalen) {
    const size_t final_len = datalen * 2;
    char* chrs = (char*)malloc((final_len + 1) * sizeof(*chrs));
    unsigned int j = 0;
    for (j = 0; j < datalen; j++) {
        chrs[2 * j] = (data[j] >> 4) + 48;
        chrs[2 * j + 1] = (data[j] & 15) + 48;
        if (chrs[2 * j] > 57) {
            chrs[2 * j] += 7;
        }
        if (chrs[2 * j + 1] > 57) {
            chrs[2 * j + 1] += 7;
        }
    }
    chrs[2 * j] = '\0';
    return chrs;
}

class MyPDFSigner : public PDFSigner {
public:

    int FireExternalSign(PDFSignerExternalSignEventParams* e) override {
        keymanager.ImportFromFile(keyFile, 3, "", "", e->Pars, 0);

        if (keymanager.GetLastErrorCode() == 0) {
            crypto.SetKeyHandle(keymanager.GetKeyHandle());
        } else {
            printf("Error [%d]: %s", keymanager.GetLastErrorCode(), keymanager.GetLastError());
            return 0;
        }

        crypto.SetHashAlgorithm(e->HashAlgorithm);
        crypto.SetInputIsHash(true);
        crypto.SetSchemeParams(e->Pars);

        char* inBuf = (char*)hexstr_to_char(e->Data);

        int outSize;
        char* outBuf = crypto.Sign(inBuf, strlen(inBuf), true, &outSize);

        e->SignedData = barray2hexstr((unsigned char*)outBuf, outSize);

        return 0;
    }
};

MyPDFSigner pdfsigner;
CertificateManager certificatemanager;

int showHelp() {
    printf("Usage:\n"
        "\t./pdfsignerexternal\t-input <inputFile> -output <outputFile>\n"
        "\t\t\t-cert <certFile> -certpass <password> -key <keyFile> [-option ]?\n"
    );
    printf("Options: \n");
    printf("\t-input        (string) The PDF file to be signed.\n");
    printf("\t-output       (string) Where to save the signed document.\n");
    printf("\t-cert         (string) The certificate file to be imported.\n");
    printf("\t-certpass     (string) The key password.\n");
    printf("\t-key          (string) The key file to be imported.\n");
    exit(1);
}

int main(int argc, char** argv) {
    if (argc < 9) {
        fprintf(stderr, "Error: Missing arguments.\n");
        showHelp();
    }

    optext(argc, argv, "-input") ? pdfsigner.SetInputFile(optval(argc, argv, "-input")) : showHelp();
    optext(argc, argv, "-output") ? pdfsigner.SetOutputFile(optval(argc, argv, "-output")) : showHelp();

    if (optext(argc, argv, "-cert")) {
        char* certFile = optval(argc, argv, "-cert");
        char* certPass = optval(argc, argv, "-certpass");
        certificatemanager.ImportFromFile(certFile, certPass);
        pdfsigner.SetSigningCertHandle(certificatemanager.GetCertHandle());
        if (certificatemanager.GetCertKeyAlgorithm() == "id-dsa") {
            pdfsigner.SetNewSigHashAlgorithm("SHA1");
        }
    } else {
        showHelp();
    }

    keyFile = optval(argc, argv, "-key");

    pdfsigner.SetNewSigAuthorName("test demo author");
    pdfsigner.SetNewSigReason("test demo reason");

    pdfsigner.SetIgnoreChainValidationErrors(TRUE);
    pdfsigner.SetExternalCryptoMode(ECM_GENERIC);

    const int retcode = pdfsigner.SignExternal();

    if (retcode) {
        printf("Error [%d]: %s", pdfsigner.GetLastErrorCode(), pdfsigner.GetLastError());
        return 0;
    } else {
        printf("The document was signed successfully.\n");
    };

    fprintf(stderr, "\npress <return> to continue...\n");
    getchar();
    return 0;
}


