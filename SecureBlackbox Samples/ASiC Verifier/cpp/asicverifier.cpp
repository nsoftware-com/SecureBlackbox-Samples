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
#include <string.h>
#include "../../include/secureblackbox.h"
using namespace std;

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

const char* translateSigType(int type) {
    switch (type) {
    case CAST_CAD_ES: return "CAdES";
        break;
    case CAST_XAD_ES: return "XAdES";
        break;
    case CAST_TIMESTAMP: return "Timestamp";
        break;
    default: return "Unknown";
        break;
    }
}

const char* translateValidationResult(int res) {
  switch (res) {
  case SVT_VALID:
    return "The signature is valid.";
    break;
  case SVT_UNKNOWN:
    return "Signature validity is unknown.";
    break;
  case SVT_CORRUPTED:
    return "The signature is corrupted.";
    break;
  case SVT_SIGNER_NOT_FOUND:
    return "Failed to acquire the signing certificate. The signature cannot be validated.";
    break;
  case SVT_FAILURE:
    return "General failure.";
    break;
  default:
    return "Signature validity is unknown.";
    break;
  }
}

int main(int argc, char** argv) {

    ASiCVerifier verifier;

    if (!optext(argc, argv, "-input")) {
        printf(
            "NAME\n"
            "  asicverifier -- SecureBlackbox ASiCVerifier Demo Application\n\n"
            "SYNOPSIS\n"
            "  asicverifier [-input input_file] [-extractpath extract_path]\n\n"
            "DESCRIPTION\n"
            "  ASiCVerifier demonstrates the usage of ASiCVerifier from SecureBlackbox.\n"
            "  Used to verify the signature of and optionally extract any files in an Associated Signature Container (ASic).\n\n"
            "  The options are as follows:\n\n"
            "  -extractpath  The path to extract files to. If unspecified, files will not be extracted.\n\n"
            "  -input        The ASiC to verify.\n\n"
        );
        goto done;
    }

    verifier.SetInputFile(optval(argc, argv, "-input"));
    if (optext(argc, argv, "-extractpath")) {
      verifier.SetExtractionMode(AEM_ALL);
      verifier.SetOutputPath(optval(argc, argv, "-extractpath"));
    }

    if (verifier.Verify()) {
        goto done;
    }

    printf("There are %i signatures in this file.\n", verifier.GetSignatureCount());
    for (int i = 0; i < verifier.GetSignatureCount(); i++) {
        printf(
            "Signature #%i\n"
            "  SignatureType: %s\n"
            "  File(s): %s\n",
            i, translateSigType(verifier.GetSignatureSignatureType(i)), verifier.GetSignatureSignedFiles(i)
        );

        printf("  Validation Result: %d, %s\n", verifier.GetSignatureSignatureValidationResult(i), translateValidationResult(verifier.GetSignatureSignatureValidationResult(i)));
        printf("  Chain Result: %d\n\n", verifier.GetSignatureChainValidationResult(i));
    }

done:
    if (verifier.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", verifier.GetLastErrorCode(), verifier.GetLastError());
    }
    printf("Goodbye.\n");
    getchar();
}


