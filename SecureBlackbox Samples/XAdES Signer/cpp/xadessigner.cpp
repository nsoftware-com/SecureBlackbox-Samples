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

void displayHelp() {
    printf(
        "NAME\n"
        "  xadessigner -- SecureBlackbox XAdESSigner Demo Application\n\n"
        "SYNOPSIS\n"
        "  xadessigner [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password] \\\n"
        "              [-version version] [-hashalgo hash_algorithm] [-canonmethod canon_method] [-tsserver timestamp_server] \\\n"
        "              [-form form] [-includekey] [-detached] \n\n"
        "DESCRIPTION\n"
        "  XAdESSigner demonstrates the usage of XAdESSigner from SecureBlackbox.\n"
        "  Used to create an XML Extended Signature (XAdES) from an XML file.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the XAdES will be saved (Required).\n\n"
        "  -cert         The certificate used to sign files (Required).\n\n"
        "  -certpass     The password for the signing certificate (Required).\n\n"
        "  -version      The XAdES version to use. Enter the corresponding number. Valid values:\n\n"
        "                  1 - XAV_111\n"
        "                  2 - XAV_122\n"
        "                  3 - XAV_132\n"
        "                  4 - XAV_141\n\n"
        "  -form         The XAdES form to use. Enter the corresponding number. Valid values:\n\n"
        "                  1  - XAF_BASIC \n"
        "                  2  - XAF_BES \n"
        "                  3  - XAF_EPES \n"
        "                  4  - XAF_T \n"
        "                  5  - XAF_C \n"
        "                  6  - XAF_X \n"
        "                  7  - XAF_XL \n"
        "                  8  - XAF_A \n"
        "                  9  - XAF_EXTENDED_BES \n"
        "                  10 - XAF_EXTENDED_EPES \n"
        "                  11 - XAF_EXTENDED_T \n"
        "                  12 - XAF_EXTENDED_C \n"
        "                  13 - XAF_EXTENDED_X \n"
        "                  14 - XAF_EXTENDED_XLONG \n"
        "                  15 - XAF_EXTENDED_XL \n"
        "                  16 - XAF_EXTENDED_A\n\n"
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
        "  -hashalgo     The hashing algorithm to use. Valid values:\n\n"
        "                  SHA1\n"
        "                  MD5\n"
        "                  SHA256\n"
        "                  SHA384\n"
        "                  SHA512\n"
        "                  RIPEMD160\n\n"
        "  -tsserver     A timestamp server to use during signing.\n\n"
        "  -includekey   Whether to include the public key in the signature.\n\n"
        "EXAMPLES\n"
        "  xadessigner -input C:\\xades\\myfile.xml -output C:\\xades\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
        "  xadessigner -input C:\\xades\\myfile.xml -output C:\\xades\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword \\\n"
        "              -version 2 -form 12 -canonmethod 3 -hashalgo SHA1 -detached -tsserver http://timestamp.wosign.com\n\n"
    );
}

int main(int argc, char** argv) {
    XAdESSigner signer;
    CertificateManager cm;

    // Validate input
    if (argc < 8) {
        displayHelp();
        goto done;
    }

    char* input = optval(argc, argv, "-input");
    if (!strcmp(input, "")) {
        printf("-input is required.");
        displayHelp();
        goto done;
    }

    char* output = optval(argc, argv, "-output");
    if (!strcmp(output, "")) {
        printf("-output is required.");
        displayHelp();
        goto done;
    }

    char* cert = optval(argc, argv, "-cert");
    if (!strcmp(cert, "")) {
        printf("-cert is required.");
        displayHelp();
        goto done;
    }

    char* certpass = optval(argc, argv, "-certpass");
    if (!strcmp(certpass, "")) {
        printf("-certpass is required.");
        displayHelp();
        goto done;
    }

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
    signer.SetNewSigXAdESForm(atoi(optval(argc, argv, "-form")));
    signer.SetNewSigCanonicalizationMethod(atoi(optval(argc, argv, "-canonmethod")));
    signer.SetNewSigHashAlgorithm(optext(argc, argv, "-hashalgo") ? optval(argc, argv, "-hashalgo") : "SHA256");
    if (optext(argc, argv, "-includekey")) {
        signer.Config("IncludeKey=true");
    }

    // Required options
    signer.SetOutputFile(output);
    cm.ImportFromFile(cert, certpass);
    signer.SetSigningCertHandle(cm.GetCertHandle());
    // Sign & Create
    if (signer.Sign()) {
        goto done;
    }
    printf("XAdES created.\n");

done:
    if (signer.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", signer.GetLastErrorCode(), signer.GetLastError());
    }
    getchar();
}


