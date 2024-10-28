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
		"  jadessigner -- SecureBlackbox JAdESSigner Demo Application\n\n"
		"SYNOPSIS\n"
		"  jadessigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password]\n"
		"             [-level sig_level] [-detached] [-compact] [-flattened]\n\n"
		"DESCRIPTION\n"
		"  This sample illustrates the use of JAdESSigner component for creating JWS/JAdES signature.\n\n"
		"  The options are as follows:\n\n"
		"  -input        An input file to sign (Required).\n\n"
		"  -output       Where the signed file will be saved (Required).\n\n"
		"  -cert         The certificate used to sign files (Required).\n\n"
		"  -certpass     The password for the signing certificate.\n\n"
		"  -level        The level for JAdES signatures. Enter the corresponding number. Valid values:\n\n"
		"                  1  - JASL_GENERIC\n"
		"                  2  - JASL_BASELINE_B\n"
		"                  3  - JASL_BASELINE_T\n"
		"                  4  - JASL_BASELINE_LT\n"
		"                  5  - JASL_BASELINE_LTA\n\n"
		"  -detached     Whether a detached signature should be produced.\n\n"
		"  -compact      Whether the JWS compact serialization should be used.\n\n"
		"  -flattened    Whether the flattened signature should be created.\n\n"
		"EXAMPLES\n"
		"  jadessigner -input C:\\helloworld.exe -output C:\\myexe.scs -cert C:\\certs\\mycert.pfx -certpass mypassword -flattened\n\n"
		"  jadessigner -input C:\\helloworld.dll -output C:\\mydll.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
		"             -level 1 -detached -compact\n\n"
	);

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
	JAdESSigner signer;
	CertificateManager cm;

	if (argc < 2) {
		displayHelp("");
		goto done;
	}

	char* input = optval(argc, argv, "-input");
	if (!strcmp(input, "")) {
		displayHelp("-input is required.");
		goto done;
	}

	char* output = optval(argc, argv, "-output");
	if (!strcmp(output, "")) {
		displayHelp("-output is required.");
		goto done;
	}

	char* cert = optval(argc, argv, "-cert");
	if (!strcmp(cert, "")) {
		displayHelp("-cert is required.");
		goto done;
	}

	char* certpass = optval(argc, argv, "-certpass");

	// Required options
	signer.SetDataFile(input);
	signer.SetOutputFile(output);
	cm.ImportFromFile(cert, certpass);
	signer.SetSigningCertHandle(cm.GetCertHandle());

	// Additional options
	optext(argc, argv, "-level")
		? signer.SetNewSigLevel(atoi(optval(argc, argv, "-level")))
		: signer.SetNewSigLevel(JASL_BASELINE_B);
	optext(argc, argv, "-detached")
		? signer.SetDetached(true)
		: signer.SetDetached(false);
	optext(argc, argv, "-compact")
		? signer.SetCompactForm(true)
		: signer.SetCompactForm(false);
	optext(argc, argv, "-flattened")
		? signer.SetFlattenedSignature(true)
		: signer.SetFlattenedSignature(false);

	if (signer.Sign()) {
		goto err;
	}

	printf("JWS/JAdES signature successfully created.\n\n");

err:
	if (signer.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", signer.GetLastErrorCode(), signer.GetLastError());
	}
done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}




