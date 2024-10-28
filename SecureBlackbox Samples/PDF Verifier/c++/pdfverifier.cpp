/*
 * SecureBlackbox 2024 C++ Edition - Sample Project
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

    printf("  Chain Result:      %d\n\n", verifier.GetSignatureChainValidationResult(idx));
}

void displayHelp(const char* errMes) {
	printf(
		"NAME\n"
		"  pdfverifier -- SecureBlackbox PDFVerifier Demo Application\n\n"
		"SYNOPSIS\n"
		"  pdfverifier <-input input_file> [-checkrev] [-ignoreerrors] [-offline]\n\n"
		"DESCRIPTION\n"
		"  This sample illustrates the use of PDFVerifier component for validating PDF signatures.\n\n"
		"  The options are as follows:\n\n"
		"  -input         An input file to verify (Required).\n\n"
		"  -checkrev      Whether certificate revocation information should be checked.\n\n"
		"  -ignoreerrors  Whether to ignore chain validation errors.\n\n"
		"  -offline       Whether offline mode be used.\n\n"
		"EXAMPLES\n"
		"  pdfverifier -input C:\\myfile.pdf -offline\n\n"
		"  pdfverifier -input C:\\myfile.pdf -checkrev -ignoreerrors\n\n"
	);

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
	PDFVerifier verifier;

	// Validate input
	if (argc < 2) {
		displayHelp("");
		goto done;
	}

	char* input = optval(argc, argv, "-input");
	if (!strcmp(input, "")) {
		displayHelp("-input is required.");
		goto done;
	}
	verifier.SetInputFile(input);

	optext(argc, argv, "-checkrev")
		? verifier.SetRevocationCheck(CRC_AUTO)
		: verifier.SetRevocationCheck(CRC_NONE);
	optext(argc, argv, "-ignoreerrors")
		? verifier.SetIgnoreChainValidationErrors(true)
		: verifier.SetIgnoreChainValidationErrors(false);
	optext(argc, argv, "-offline")
		? verifier.SetOfflineMode(true)
		: verifier.SetOfflineMode(false);

    if (verifier.Verify()) {
        goto err;
    } 

    for (int i = 0; i < verifier.GetSignatureCount(); i++) {
        printPDFSignature(i, verifier);
    }

err:
	if (verifier.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", verifier.GetLastErrorCode(), verifier.GetLastError());
	}
done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


