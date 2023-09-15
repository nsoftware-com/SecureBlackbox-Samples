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
        "  messagesigner -- SecureBlackbox MessageSigner Demo Application\n\n"
        "SYNOPSIS\n"
        "  messagesigner [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password]\n"
        "           [-sigtype signature_type] [-hashalg hash_algorithm]\n\n"
        "DESCRIPTION\n"
        "  MessageSigner demonstrates the usage of MessageSigner from SecureBlackbox.\n"
        "  Used to create PKCS#7-compliant signed files.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the signed file will be saved (Required).\n\n"
        "  -cert         The certificate used to sign files (Required).\n\n"
        "  -certpass     The password for the signing certificate (Required).\n\n"
        "  -sigtype      The type of signature to use. Enter the corresponding number. Valid values:\n\n"
        "                  1 - ST_PKCS1DETACHED\n"
        "                  2 - ST_PKCS7DETACHED (Default)\n"
        "                  3 - ST_PKCS7ENVELOPING\n\n"
        "  -hashalg      The Hash algorithm.\n\n"
        "EXAMPLES\n"
        "  messagesigner -input C:\\mes\\helloworld.txt -output C:\\mes\\mymes.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
        "  messagesigner -input C:\\mes\\helloworld.txt -output C:\\mes\\mymes.scs -cert C:\\certs\\mycert.pfx -certpass mypassword \\\n"
        "             -sigtype 2 -hashalg SHA256 \n\n"
    );
}

int main(int argc, char** argv) {
    MessageSigner signer;
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
    if (optext(argc, argv, "-sigtype")) {
        signer.SetSignatureType(atoi(optval(argc, argv, "-sigtype")));
    }

    if (optext(argc, argv, "-hashalg")) {
        signer.SetHashAlgorithm(optval(argc, argv, "-hashalg"));
    }

    // Required options
    signer.SetInputFile(input);
    signer.SetOutputFile(output);
    cm.ImportFromFile(cert, certpass);
    signer.SetSigningCertHandle(cm.GetCertHandle());

    // Sign & Create
    if (signer.Sign()) {
        goto done;
    }
    printf("The file successfully signed.\n");

done:
    if (signer.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", signer.GetLastErrorCode(), signer.GetLastError());
    }
    getchar();
}


