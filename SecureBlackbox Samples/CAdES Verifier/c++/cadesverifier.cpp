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

static bool optext(int argc, char** argv, const char* option) {
    for (int x = 0; x < argc; x++) {
        if (!strcmp(argv[x], option)) {
            return true;
        }
    }
    return false;
}

using namespace ArgParser;

void printSignerCertificate(int idx, CAdESVerifier& verifier) {
    printf("Certificate '%s'\n", verifier.GetCertSubject(idx));
    printf("  Valid:            from %s to %s\n", verifier.GetCertValidFrom(idx), verifier.GetCertValidTo(idx));
    printf("  Self-signed:            %d\n", verifier.GetCertSelfSigned(idx));
    printf("  Subject RDN:                %s\n\n", verifier.GetCertSubjectRDN(idx));
}

void displayHelp(const char* errMes) {
	printf(
		"NAME\n"
		"  cadesverifier -- SecureBlackbox CAdESVerifier Demo Application\n\n"
		"SYNOPSIS\n"
		"  cadesverifier <-input input_file> [-output output_file] [-data data_file] [-checkrev] [-ignoreerrors] [-offline]\n\n"
		"DESCRIPTION\n"
		"  This sample shows processing of CAdES signatures. \n\n"
		"  The options are as follows:\n\n"
		"  -input         An input file to verify (Required).\n\n"
		"  -output        Where to save the verified, unpacked message.\n\n"
		"  -data          The payload to be validated (for detached signatures).\n\n"
		"  -checkrev      Whether certificate revocation information should be checked.\n\n"
		"  -ignoreerrors  Whether to ignore chain validation errors.\n\n"
		"  -offline       Whether offline mode be used.\n\n"
		"EXAMPLES\n"
		"  cadesverifier -input C:\\myexe.scs -output C:\\helloworld.exe -offline\n\n"
		"  cadesverifier -input C:\\mydll.scs -data C:\\helloworld.dll -checkrev -ignoreerrors\n\n"
	);

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
	CAdESVerifier verifier;

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
	verifier.SetInputFile(input);

	if (optext(argc, argv, "-output"))
	{
		verifier.SetDetached(false);
		verifier.SetOutputFile(optval(argc, argv, "-output"));
	}
	else if(optext(argc, argv, "-data"))
	{
		verifier.SetDetached(true);
		verifier.SetDataFile(optval(argc, argv, "-data"));
	}
	else {
		displayHelp("-output or -data is required.");
		goto done;
	}

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
    
	switch (verifier.GetSignatureSignatureValidationResult(0)) {
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

    for (int i = 0; i < verifier.GetCertCount(); i++) {
        printSignerCertificate(i, verifier);
    }

err:
	if (verifier.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", verifier.GetLastErrorCode(), verifier.GetLastError());
	}
done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


