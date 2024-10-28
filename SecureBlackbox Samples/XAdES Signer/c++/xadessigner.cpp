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
        "  xadessigner -- SecureBlackbox XAdESSigner Demo Application\n\n"
        "SYNOPSIS\n"
		"  xadessigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password] \n"
        "              [-version version] [-hashalg hash_algorithm] [-canonmethod canon_method] [-tsserver timestamp_server] \n"
        "              [-level level] [-includekey] [-detached] \n\n"
        "DESCRIPTION\n"
        "  XAdESSigner demonstrates the usage of XAdESSigner from SecureBlackbox.\n"
        "  Used to create an XML Extended Signature (XAdES) from an XML file.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the XAdES will be saved (Required).\n\n"
        "  -cert         The certificate used to sign files (Required).\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
        "  -version      The XAdES version to use. Enter the corresponding number. Valid values:\n"
        "                  1 - XAV_111\n"
        "                  2 - XAV_122\n"
        "                  3 - XAV_132\n"
        "                  4 - XAV_141\n\n"
        "  -level         The XAdES level/form to use. Enter the corresponding number. Valid values:\n"
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
        "  -detached     Whether the signature is detached.\n\n"
        "  -canonmethod  The canonicalization method to use. Enter the corresponding number. Valid values:\n"
        "                  0 - CXCM_NONE\n"
        "                  1 - CXCM_CANON\n"
        "                  2 - CXCM_CANON_COMMENT\n"
        "                  3 - CXCM_EXCL_CANON\n"
        "                  4 - CXCM_EXCL_CANON_COMMENT\n"
        "                  5 - CXCM_MIN_CANON\n"
        "                  6 - CXCM_CANON_V_1_1\n"
        "                  7 - CXCM_CANON_COMMENT_V_1_1\n\n"
		"  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224\n\n"
        "  -tsserver     A timestamp server to use during signing.\n\n"
        "  -includekey   Whether to include the public key in the signature.\n\n"
        "EXAMPLES\n"
        "  xadessigner -input C:\\xades\\myfile.xml -output C:\\xades\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
        "  xadessigner -input C:\\xades\\myfile.xml -output C:\\xades\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
        "              -version 3 -level 3 -canonmethod 1 -hashalg SHA256 -detached -tsserver http://timestamp.wosign.com\n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
    XAdESSigner signer;
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
    signer.SetTimestampServer(optval(argc, argv, "-tsserver"));
    if (optext(argc, argv, "-detached")) {
      signer.SetDataFile(input);
      signer.SetDataType(CXDT_BINARY);
      signer.SetDataURI("filename.txt"); // use real name of the input
      signer.SetNewSigSignatureType(CXST_DETACHED); // cxstDetached
    } else {
      signer.SetInputFile(input);
      signer.SetNewSigSignatureType(CXST_ENVELOPED); // cxstEnveloped
    }
    signer.SetNewSigXAdESVersion(atoi(optval(argc, argv, "-version")));
    signer.SetNewSigLevel(atoi(optval(argc, argv, "-level")));
    signer.SetNewSigCanonicalizationMethod(atoi(optval(argc, argv, "-canonmethod")));
    signer.SetNewSigHashAlgorithm(optext(argc, argv, "-hashalg") ? optval(argc, argv, "-hashalg") : "SHA256");
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
    printf("XAdES created.\n\n");

err:
    if (signer.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", signer.GetLastErrorCode(), signer.GetLastError());
    }
done:
	printf("Press Enter to exit the demo.\n");
    getchar();
}


