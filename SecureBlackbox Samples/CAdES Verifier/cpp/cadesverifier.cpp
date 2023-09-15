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

CAdESVerifier verifier;
CertificateManager certificatemanager;

void printSignerCertificate(int idx, CAdESVerifier& verifier) {
    printf("Certificate '%s'\n", verifier.GetCertSubject(idx));
    printf("  Valid:            from %s to %s\n", verifier.GetCertValidFrom(idx), verifier.GetCertValidTo(idx));
    printf("  Self-signed:            %d\n", verifier.GetCertSelfSigned(idx));
    printf("  Subject RDN:                %s\n", verifier.GetCertSubjectRDN(idx));
}

int showHelp() {
    printf("Usage: \n\t./cadesverifier -inputFile <inputFile> -outputFile <outputFile> [-option ]?\n");
    printf("Options: \n");
    printf("\t-inputFile                      (string)  The message to be verified.\n");
    printf("\t-outputFile                     (string)  Where to save the verified, unpacked message.\n");
    printf("\t-performRevocationCheck         (switch)  Whether certificate revocation information should be checked.\n");
    printf("\t-ignoreChainValidationErrors    (switch)  Whether to ignore chain validation errors.\n");
    printf(
        "\t-forceCompleteChainValidation   (switch)  Whether to check issuer (CA) certificates when the signing certificate is invalid.\n");
    exit(1);
}

int main(int argc, char** argv) {
    if (argc < 5) {
        fprintf(stderr, "Error: Missing arguments.\n");
        showHelp();
    }

    optext(argc, argv, "-inputFile") ? verifier.SetInputFile(optval(argc, argv, "-inputFile")) : showHelp();
    optext(argc, argv, "-outputFile") ? verifier.SetOutputFile(optval(argc, argv, "-outputFile")) : 0;
    optext(argc, argv, "-performRevocationCheck")
        ? verifier.SetRevocationCheck(CRC_AUTO)
        : verifier.SetRevocationCheck(CRC_NONE);
    optext(argc, argv, "-ignoreChainValidationErrors")
        ? verifier.SetIgnoreChainValidationErrors(true)
        : verifier.SetIgnoreChainValidationErrors(false);
    optext(argc, argv, "-forceCompleteChainValidation")
        ? verifier.Config("ForceCompleteChainValidation=true")
        : verifier.Config("ForceCompleteChainValidation=false");

    const int retcode = verifier.Verify();

    if (retcode) {
        printf("Error [%d]: %s", verifier.GetLastErrorCode(), verifier.GetLastError());
        return 0;
    } else {
        switch (verifier.GetSignatureValidationResult()) {
        case 0:
            printf("The signature is valid.\n");
            break;
        case 1:
            printf("Signature verification failed: Unknown signature.\n");
            break;
        case 2:
            printf("Signature verification failed: The signature is corrupt or invalid.\n");
            break;
        case 3:
            printf("Signature verification failed: The signature does not contain a signer.\n");
            break;
        case 4:
            printf("Signature verification failed.\n");
            break;
        }
    }
    for (int i = 0; i < verifier.GetCertCount(); i++) {
        printSignerCertificate(i, verifier);
    }

    fprintf(stderr, "\npress <return> to continue...\n");
    getchar();
}


