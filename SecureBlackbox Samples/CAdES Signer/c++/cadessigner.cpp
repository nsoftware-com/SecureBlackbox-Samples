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

void displayHelp(const char* errMes) {
	printf(
		"NAME\n"
		"  cadessigner -- SecureBlackbox CAdESSigner Demo Application\n\n"
		"SYNOPSIS\n"
		"  cadessigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password]\n"
		"             [-level sig_level] [-hashalg hashalg] [-detached]\n\n"
		"DESCRIPTION\n"
		"  This sample shows how to create CAdES signatures.\n\n"
		"  The options are as follows:\n\n"
		"  -input        An input file to sign (Required).\n\n"
		"  -output       Where the signed file will be saved (Required).\n\n"
		"  -cert         The certificate used to sign files (Required).\n\n"
		"  -certpass     The password for the signing certificate.\n\n"
		"  -level        The level for CAdES signatures. Enter the corresponding number. Valid values:\n\n"
	        "                  0  - ASL_UNKNOWN\n"
        	"                  1  - ASL_GENERIC\n"
	        "                  2  - ASL_BASELINE_B\n"
	        "                  3  - ASL_BASELINE_T\n"
        	"                  4  - ASL_BASELINE_LT\n"
        	"                  5  - ASL_BASELINE_LTA\n"
	        "                  6  - ASL_BES\n"
        	"                  7  - ASL_EPES\n"
	        "                  8  - ASL_T\n"
        	"                  9  - ASL_C\n"
	        "                  10 - ASL_X\n"
        	"                  11 - ASL_XTYPE_1\n"
	        "                  12 - ASL_XTYPE_2\n"
        	"                  13 - ASL_XL\n"
	        "                  14 - ASL_XLTYPE_1\n"
        	"                  15 - ASL_XLTYPE_2\n"
	        "                  16 - ASL_A\n"
        	"                  17 - ASL_EXTENDED_BES\n"
	        "                  18 - ASL_EXTENDED_EPES\n"
        	"                  19 - ASL_EXTENDED_T\n"
	        "                  20 - ASL_EXTENDED_C\n"
        	"                  21 - ASL_EXTENDED_X\n"
	        "                  22 - ASL_EXTENDED_XTYPE_1\n"
        	"                  23 - ASL_EXTENDED_XTYPE_2\n"
	        "                  24 - ASL_EXTENDED_XLONG\n"
        	"                  25 - ASL_EXTENDED_XL\n"
	        "                  26 - ASL_EXTENDED_XLTYPE_1\n"
        	"                  27 - ASL_EXTENDED_XLTYPE_2\n"
	        "                  28 - ASL_EXTENDED_A\n\n"
		"  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224, MD5\n\n"
		"  -detached     Whether to store the generated signature in a separate message.\n\n"
		"EXAMPLES\n"
		"  cadessigner -input C:\\helloworld.exe -output C:\\myexe.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
		"  cadessigner -input C:\\helloworld.dll -output C:\\mydll.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
		"             -hashalg SHA256 -level 1 -detached\n\n"
	);

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
	CAdESSigner signer;
	CertificateManager cm;

    if (argc <= 1) {
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
	signer.SetInputFile(input);
	signer.SetOutputFile(output);
	cm.ImportFromFile(cert, certpass);
	signer.SetSigningCertHandle(cm.GetCertHandle());

	// Additional options
	if (optext(argc, argv, "-hashalg")) {
		signer.SetNewSigHashAlgorithm(optval(argc, argv, "-hashalg"));
	}
	if (optext(argc, argv, "-level")) {
		signer.SetNewSigLevel(atoi(optval(argc, argv, "-level")));
	}
    optext(argc, argv, "-detached") ? signer.SetDetached(true) : signer.SetDetached(false);

    if (signer.Sign()) {
		goto err;
    }

    printf("The message was signed successfully.\n\n");

err:
	if (signer.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", signer.GetLastErrorCode(), signer.GetLastError());
	}
done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


