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

class MyVerify : public SOAPVerifier {
public:

    bool showrefs = false;

    int FireReferenceValidated(SOAPVerifierReferenceValidatedEventParams* e) override {
        if (showrefs) {
            printf(
                "%.10s %.22s %.22s %s\n",
                e->ID, e->URI, e->RefType, e->DigestValid ? "true" : "false"
            );
        }
        return 0;
    }
};

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

void displayHelp(const char* errMes) {
    printf(
        "NAME\n"
        "  soapverifier -- SecureBlackbox SOAPVerifier Demo Application\n\n"
        "SYNOPSIS\n"
		"  soapverifier <-input input_file> [-cert certificate_file] [-certpass certificate_password] [-showrefs]\n"
        "DESCRIPTION\n"
        "  SOAPVerifier demonstrates the usage of SOAPVerifier from SecureBlackbox.\n"
        "  Used to verify the signature.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to verify (Required).\n\n"
        "  -cert         The certificate used to sign files.\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
        "  -showrefs     Whether to display detailed results of reference verification.\n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
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
  case SVT_REFERENCE_CORRUPTED:
    return "The signature has invalid reference(s).";
    break;
  default:
    return "Signature validity is unknown.";
    break;
  }
}

int main(int argc, char** argv) {
    MyVerify verifier;
    CertificateManager cm;

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

    // Additional options
    char* cert = optval(argc, argv, "-cert");
    char* certpass = optval(argc, argv, "-certpass");

    // Required options
    verifier.SetInputFile(input);

    if (strcmp(cert, "")) {
        cm.ImportFromFile(cert, certpass);
        verifier.SetKnownCertCount(1);
        verifier.SetKnownCertHandle(0, cm.GetCertHandle());
    }

    if (optext(argc, argv, "-showrefs")) {
        verifier.showrefs = true;
        printf(
            "ID URI RefType DigestValid?\n"
            "---------------------\n"
        );
    }

    // Verify
    if (verifier.Verify()) {
        goto err;
    }

    for (int i = 0; i < verifier.GetSignatureCount(); i++)
    {
      printf("Signature #%i\n", i);

      printf("  Validation Result: %d, %s\n", verifier.GetSignatureSignatureValidationResult(i), translateValidationResult(verifier.GetSignatureSignatureValidationResult(i)));
      printf("  Chain Result: %d\n\n", verifier.GetSignatureChainValidationResult(i));
    }

err:
    if (verifier.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", verifier.GetLastErrorCode(), verifier.GetLastError());
    }
done:
	printf("Press Enter to exit the demo.\n");
    getchar();
}


