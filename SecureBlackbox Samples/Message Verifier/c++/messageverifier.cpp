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
        "  messageverifier -- SecureBlackbox MessageVerifier Demo Application\n\n"
        "SYNOPSIS\n"
		"  messageverifier <-input input_file> [-output output_file] [-data data_file]\n"
        "            [-cert certificate_file] [-certpass certificate_password]\n\n"
        "DESCRIPTION\n"
        "  MessageVerifier demonstrates the usage of MessageVerifier from SecureBlackbox.\n"
        "  Used to validating PKCS#7-compliant signed files.\n\n"
        "  The options are as follows:\n\n"
        "  -input        A signature to verify (Required). If the signature is detached, this will take\n"
        "                the signature file and -data will take the original data.\n\n"
        "  -data         The original data (Required for detached signature).\n\n"
        "  -output       Where to save the verified, unpacked message (Required for non detached signature).\n\n"
        "  -cert         The certificate used to sign files.\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
        "EXAMPLES\n"
        "  messageverifier -input C:\\mes\\mymes.scs -detached -data C:\\mes\\helloworld.txt -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
        "  messageverifier -input C:\\mes\\mymes.scs -output C:\\mes\\helloworld.txt\n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
    MessageVerifier verifier;
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
    verifier.SetInputFile(input);

    const bool detached = false;
	char* output = optval(argc, argv, "-output");
	char* data = optval(argc, argv, "-data");
	if (strcmp(output, "")) {
		verifier.SetOutputFile(output);
	}
	else if (strcmp(data, "")) {
		verifier.SetDataFile(output);
	}
	else {
		printf("-output or -data is required.");
		goto done;
	}

    // Additional options
    char* cert = optval(argc, argv, "-cert");
    char* certpass = optval(argc, argv, "-certpass");

    if (strcmp(cert, "")) {
        cm.ImportFromFile(cert, certpass);
        verifier.SetKnownCertCount(1);
        verifier.SetKnownCertHandle(0, cm.GetCertHandle());
    }

    // Verify
    if (detached ? verifier.VerifyDetached() : verifier.Verify()) {
        goto err;
    }

    switch (verifier.GetSignatureValidationResult()) {
    case SVT_VALID:
        printf(
            "The signature is valid.\n"
            "Hash algorithm: %s\n\n",
            verifier.GetHashAlgorithm());
        break;
    case SVT_UNKNOWN:
        printf("Signature verification failed: Unknown signature.\n\n");
        break;
    case SVT_CORRUPTED:
        printf("Signature verification failed: The signature is corrupt or invalid.\n\n");
        break;
    case SVT_SIGNER_NOT_FOUND:
        printf("Signature verification failed: The signature does not contain a signer.\n\n");
        break;
    case SVT_FAILURE:
        printf("Signature verification failed.\n\n");
        break;
    }

err:
    if (verifier.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", verifier.GetLastErrorCode(), verifier.GetLastError());
    }
done:
	printf("Press Enter to exit the demo.\n");
    getchar();
}


