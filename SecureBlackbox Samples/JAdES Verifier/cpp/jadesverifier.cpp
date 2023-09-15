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

JAdESVerifier jadesverifier;
CertificateManager certificatemanager;

void printJAdESSignature(int idx, JAdESVerifier& verifier) {
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
    printf("Usage: \n\t./jadesverifier -inputFile <inputFile> [-option ]?\n");
    printf("Options: \n");
    printf("\t-input                        (string) An input file to verify (Required).\n");
    printf("\t-dataFile                     (string) The payload to be validated (for detached signatures).\n");
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

    optext(argc, argv, "-inputFile") ? jadesverifier.SetInputFile(optval(argc, argv, "-inputFile")) : showHelp();
    if (optext(argc, argv, "-dataFile"))
      jadesverifier.SetDataFile(optval(argc, argv, "-dataFile"));
    optext(argc, argv, "-performRevocationCheck")
        ? jadesverifier.SetRevocationCheck(CRC_AUTO)
        : jadesverifier.SetRevocationCheck(CRC_NONE);
    optext(argc, argv, "-ignoreChainValidationErrors")
        ? jadesverifier.SetIgnoreChainValidationErrors(true)
        : jadesverifier.SetIgnoreChainValidationErrors(false);
    optext(argc, argv, "-forceCompleteChainValidation")
        ? jadesverifier.Config("ForceCompleteChainValidation=true")
        : jadesverifier.Config("ForceCompleteChainValidation=false");

    const int retcode = jadesverifier.Verify();

    if (retcode) {
        printf("Error [%d]: %s", jadesverifier.GetLastErrorCode(), jadesverifier.GetLastError());
        return 0;
    } 

    for (int i = 0; i < jadesverifier.GetSignatureCount(); i++) {
        printJAdESSignature(i, jadesverifier);
    }

    fprintf(stderr, "\npress <return> to continue...\n");
    getchar();
}


