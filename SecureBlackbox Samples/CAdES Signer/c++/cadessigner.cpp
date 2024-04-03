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

namespace ArgParser {
    static char* optval(int argc, char** argv, const char* option) {
        for (int x = 0; x < argc - 1; x++) {
            if (!strcmp(argv[x], option)) {
                return argv[x + 1];
            }
        }
        return (char*)"";
    }
}

static bool optext(int argc, char** argv, const char* option) {
    for (int x = 0; x < argc; x++) {
        if (!strcmp(argv[x], option)) {
            return true;
        }
    }
    return false;
}

using namespace ArgParser;

CAdESSigner signer;
CertificateManager certificatemanager;

int showHelp() {
    printf(
        "Usage:\n"
        "\t./cadessigner\t-inputFile <inputFile> -outputFile <outputFile>\n"
        "\t\t\t-signingKey <keyfilePath> -keyPassword <password> [-option ]?\n"
    );
    printf("Options: \n");
    printf("\t-inputFile        (string)  The message to be signed.\n");
    printf("\t-outputFile       (string)  Where to save the signed message.\n");
    printf("\t-signingKey       (string)  The key file to be imported.\n");
    printf("\t-keyPassword      (string)  The key password.\n");
    printf("\t-hashAlgorithm    (string)  Hash algorithm used to calculate the message digest.\n");
    printf("\t-detached         (switch)  Whether to store the generated signature in a separate message.\n");
    printf("\t-sigLevel         (int)     CMS Advanced Electronic Signature level\n");
    exit(1);
}

int main(int argc, char** argv) {
    if (argc < 9) {
        fprintf(stderr, "Error: Missing arguments.\n");
        showHelp();
    }
    int sigLevel;
    bool detached;

    optext(argc, argv, "-inputFile") ? signer.SetInputFile(optval(argc, argv, "-inputFile")) : showHelp();
    optext(argc, argv, "-outputFile") ? signer.SetOutputFile(optval(argc, argv, "-outputFile")) : showHelp();
    optext(argc, argv, "-hashAlgorithm") ? signer.SetHashAlgorithm(optval(argc, argv, "-hashAlgorithm")) : 0;
    optext(argc, argv, "-sigLevel") ? sigLevel = atoi(optval(argc, argv, "-sigLevel")) : sigLevel = 1;
    optext(argc, argv, "-detached") ? detached = true : detached = false;

    if (optext(argc, argv, "-signingKey") && optext(argc, argv, "-keyPassword")) {
        char* signingKeyPath = optval(argc, argv, "-signingKey");
        char* signingKeyPassword = optval(argc, argv, "-keyPassword");
        certificatemanager.ImportFromFile(signingKeyPath, signingKeyPassword);
        signer.SetSigningChainCount(1);
        signer.SetSigningCertHandle(certificatemanager.GetCertHandle());
    } else {
        showHelp();
    }

    int retcode = signer.Sign(sigLevel, detached);

    if (retcode) {
        printf("Error [%d]: %s", signer.GetLastErrorCode(), signer.GetLastError());
        return 0;
    } else {
        printf("The message was signed successfully.\n");
    };

    fprintf(stderr, "\npress <return> to continue...\n");
    getchar();
}


