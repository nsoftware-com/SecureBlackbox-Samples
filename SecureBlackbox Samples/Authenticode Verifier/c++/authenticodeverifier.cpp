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

using namespace ArgParser;

void displayHelp(const char* errMes) {
    printf(
        "NAME\n"
        "  authenticodeverifier -- SecureBlackbox AuthenticodeVerifier Demo Application\n\n"
        "SYNOPSIS\n"
		"  authenticodeverifier <-input input_file> [-cert certificate_file] [-certpass certificate_password]\n\n"
        "DESCRIPTION\n"
        "  AuthenticodeVerifier demonstrates the usage of AuthenticodeVerifier from SecureBlackbox.\n"
        "  Used to verify the signature.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to verify (Required).\n\n"
        "  -cert         The certificate used to sign files.\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
        "EXAMPLES\n"
        "  authenticodeverifier -input C:\\myexe.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

const char* translateSigValidity(int type) {
    switch (type) {
    case SVT_VALID: return "Valid";
        break;
    case SVT_CORRUPTED: return "Corrupted";
        break;
    case SVT_SIGNER_NOT_FOUND: return "Signer not found";
        break;
    case SVT_FAILURE: return "Failure";
        break;
    default: return "Unknown";
        break;
    }
}

int main(int argc, char** argv) {
    AuthenticodeVerifier verifier;
    CertificateManager cm;

    // Validate input
    if (argc <= 1) {
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

    // Verify
    if (verifier.Verify()) {
        goto err;
    }

    if (!verifier.GetSigned()) {
        printf("The file is not singed.\n");
        goto done;
    }

    printf("There are %i signatures in this file.\n", verifier.GetSignatureCount());
    for (int x = 0; x < verifier.GetSignatureCount(); x++) {
        printf(
            "Signature #%i\n"
            "  Hash algorithm: %s\n"
            "  Description: %s\n"
            "  URL: %s\n"
            "  Validity: %s\n\n",
            x, verifier.GetSignatureHashAlgorithm(x), verifier.GetSignatureDescription(x), verifier.GetSignatureURL(x),
            translateSigValidity(verifier.GetSignatureSignatureValidationResult(x))
        );
    }

err:
    if (verifier.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", verifier.GetLastErrorCode(), verifier.GetLastError());
    }
done:
	printf("Press Enter to exit the demo.\n");
    getchar();
}


