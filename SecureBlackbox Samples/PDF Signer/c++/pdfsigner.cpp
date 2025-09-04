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

void displayHelp(const char* errMes) {
	printf(
		"NAME\n"
		"  pdfsigner -- SecureBlackbox PDFSigner Demo Application\n\n"
		"SYNOPSIS\n"
		"  pdfsigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password]\n"
		"             [-sigtype sig_type] [-level sig_level] [-hashalg hashalg] [-author author] [-reason reason] [-signame signame]\n\n"
		"DESCRIPTION\n"
		"  This sample illustrates the use of PDFSigner component for signing PDF documents.\n\n"
		"  The options are as follows:\n\n"
		"  -input        An input file to sign (Required).\n\n"
		"  -output       Where the signed file will be saved (Required).\n\n"
		"  -cert         The certificate used to sign files (Required).\n\n"
		"  -certpass     The password for the signing certificate.\n\n"
		"  -sigtype      The signature type. Enter the corresponding number. Valid values:\n\n"
		"                  0  - PST_UNKNOWN\n"
		"                  1  - PST_LEGACY\n"
		"                  2  - PST_PADES\n"
		"                  3  - PST_DOCUMENT_TIMESTAMP\n\n"
		"  -level        The level for PAdES signatures. Enter the corresponding number. Valid values:\n\n"
		"                  0  - PASL_UNKNOWN\n"
		"                  1  - PASL_GENERIC\n"
		"                  2  - PASL_BASELINE_B\n"
		"                  3  - PASL_BASELINE_T\n"
		"                  4  - PASL_BASELINE_LT\n"
		"                  5  - PASL_BASELINE_LTA\n"
		"                  6  - PASL_BES\n"
		"                  7  - PASL_EPES\n"
		"                  8  - PASL_LTV\n\n"
		"  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224, MD5\n\n"
		"  -author       The name of the signer who produced this signature.\n\n"
		"  -reason       Specifies the reason of the signing, for example to confirm the document correctness.\n\n"
		"  -signame      Specifies the signature identifier in the PDF-file.\n\n"
		"EXAMPLES\n"
		"  pdfsigner -input C:\\helloworld.pdf -output C:\\sign.pdf -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
		"  pdfsigner -input C:\\helloworld.pdf -output C:\\sign.pdf -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
		"           -hashalg SHA256 -level 1 -author \"Test author\"\n\n"
	);

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
	PDFSigner signer;
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
	signer.SetInputFile(input);
	signer.SetOutputFile(output);
	cm.ImportFromFile(cert, certpass);
	signer.SetSigningCertHandle(cm.GetCertHandle());
	if (cm.GetCertKeyAlgorithm() == "id-dsa") {
		signer.SetNewSigHashAlgorithm("SHA1");
	}

	// Additional options
	if (optext(argc, argv, "-hashalg")) {
		signer.SetNewSigHashAlgorithm(optval(argc, argv, "-hashalg"));
	}
	if (optext(argc, argv, "-sigtype")) {
		signer.SetNewSigSignatureType(atoi(optval(argc, argv, "-sigtype")));
	}
	if (optext(argc, argv, "-level")) {
		signer.SetNewSigLevel(atoi(optval(argc, argv, "-level")));
	}
	if (optext(argc, argv, "-author")) {
		signer.SetNewSigAuthorName(optval(argc, argv, "-author"));
	}
	if (optext(argc, argv, "-reason")) {
		signer.SetNewSigReason(optval(argc, argv, "-reason"));
	}
	if (optext(argc, argv, "-signame")) {
		signer.SetNewSigSignatureName(optval(argc, argv, "-signame"));
	}

    if (signer.Sign()) {
		goto err;
    } else {
        printf("The document was signed successfully.\n\n");
    };

err:
	if (signer.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", signer.GetLastErrorCode(), signer.GetLastError());
	}
done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


