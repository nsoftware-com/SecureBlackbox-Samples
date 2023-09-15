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

PDFVerifier pdfverifier;
CertificateManager certificatemanager;

void printPDFSignature(int idx, PDFVerifier& verifier) {
    printf("Signature %d\n", idx);
    printf("  Timestamp:         %s\n", verifier.GetSignatureValidatedSigningTime(idx));

    printf("  Validation Result: %d, ", verifier.GetSignatureSignatureValidationResult(idx));
    switch (verifier.GetSignatureSignatureValidationResult(idx)) {
    case SVT_VALID:
      printf("The signature is valid.\n");
      break;
    case SVT_UNKNOWN:
      printf("Signature validity is unknown.\n");
      break;
    case SVT_CORRUPTED:
      printf("The signature is corrupted.\n");
      break;
    case SVT_SIGNER_NOT_FOUND:
      printf("Failed to acquire the signing certificate. The signature cannot be validated.\n");
      break;
    case SVT_FAILURE:
      printf("General failure.\n");
      break;
    default:
      printf("Signature validity is unknown.\n");
      break;
    }

    printf("  Chain Result:      %d\n", verifier.GetSignatureChainValidationResult(idx));
}

int showHelp() {
    printf("Usage: \n\t./pdfverifier -inputFile <inputFile> [-option ]?\n");
    printf("Options: \n");
    printf("\t-performRevocationCheck       (switch)  Whether certificate revocation information should be checked.\n");
    printf("\t-ignoreChainValidationErrors  (switch)  Whether to ignore chain validation errors.\n");
    printf(
        "\t-forceCompleteChainValidation (switch)  Whether to check issuer (CA) certificates when the signing certificate is invalid.\n");
    exit(1);
}

int main(int argc, char** argv) {
    if (argc < 3) {
        fprintf(stderr, "Error: Missing arguments.\n");
        showHelp();
    }

    optext(argc, argv, "-inputFile") ? pdfverifier.SetInputFile(optval(argc, argv, "-inputFile")) : showHelp();
    optext(argc, argv, "-performRevocationCheck")
        ? pdfverifier.SetRevocationCheck(CRC_AUTO)
        : pdfverifier.SetRevocationCheck(CRC_NONE);
    optext(argc, argv, "-ignoreChainValidationErrors")
        ? pdfverifier.SetIgnoreChainValidationErrors(true)
        : pdfverifier.SetIgnoreChainValidationErrors(false);
    optext(argc, argv, "-forceCompleteChainValidation")
        ? pdfverifier.Config("ForceCompleteChainValidation=true")
        : pdfverifier.Config("ForceCompleteChainValidation=false");

    const int retcode = pdfverifier.Verify();

    if (retcode) {
        printf("Error [%d]: %s", pdfverifier.GetLastErrorCode(), pdfverifier.GetLastError());
        return 0;
    } 

    for (int i = 0; i < pdfverifier.GetSignatureCount(); i++) {
        printPDFSignature(i, pdfverifier);
    }

    fprintf(stderr, "\npress <return> to continue...\n");
    getchar();
}


