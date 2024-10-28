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
        "  xmlsigner -- SecureBlackbox XMLSigner Demo Application\n\n"
        "SYNOPSIS\n"
		"  xmlsigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password] \n"
        "          [-hashalg hash_algorithm] [-canonmethod canon_method] [-includekey] [-detached] \n\n"
        "DESCRIPTION\n"
        "  XMLSigner demonstrates the usage of XMLSigner from SecureBlackbox.\n"
        "  Used to create an XML Signature from an XML file.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the XML signature will be saved (Required).\n\n"
        "  -cert         The certificate used to sign files (Required).\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
        "  -detached     Whether the signature is detached.\n\n"
        "  -canonmethod  The canonicalization method to use. Enter the corresponding number. Valid values:\n\n"
        "                  0 - CXCM_NONE\n"
        "                  1 - CXCM_CANON\n"
        "                  2 - CXCM_CANON_COMMENT\n"
        "                  3 - CXCM_EXCL_CANON\n"
        "                  4 - CXCM_EXCL_CANON_COMMENT\n"
        "                  5 - CXCM_MIN_CANON\n"
        "                  6 - CXCM_CANON_V_1_1\n"
        "                  7 - CXCM_CANON_COMMENT_V_1_1\n\n"
		"  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224\n\n"
        "  -includekey   Whether to include the public key in the signature.\n\n"
        "EXAMPLES\n"
        "  xmlsigner -input C:\\xml\\myfile.xml -output C:\\xml\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
        "  xmlsigner -input C:\\xml\\myfile.xml -output C:\\xml\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
        "           -canonmethod 3 -hashalg SHA1 -detached\n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
    XMLSigner signer;
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

    // Additional options
    if (optext(argc, argv, "-detached")) {
      signer.SetDataFile(input);
      signer.SetDataType(CXDT_BINARY);
      signer.SetDataURI("filename.txt"); // use real name of the input
      signer.SetSignatureType(CXST_DETACHED);
    } else {
      signer.SetInputFile(input);
      signer.SetSignatureType(CXST_ENVELOPED);
    }

    signer.SetCanonicalizationMethod(atoi(optval(argc, argv, "-canonmethod")));
    if (optext(argc, argv, "-hashalg")) {
        signer.SetHashAlgorithm(optval(argc, argv, "-hashalg"));
    } else {
        signer.SetHashAlgorithm("SHA256");
    }
    if (optext(argc, argv, "-includekey")) {
        signer.Config("IncludeKey=true");
    }

    // Required options
    signer.SetOutputFile(output);
    cm.ImportFromFile(cert, certpass);
    signer.SetSigningCertHandle(cm.GetCertHandle());

    // Sign & Create
    if (signer.Sign()) {
        goto err;
    }
    printf("XML file successfully signed.\n\n");

err:
    if (signer.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", signer.GetLastErrorCode(), signer.GetLastError());
    }
done:
	printf("Press Enter to exit the demo.\n");
    getchar();
}


