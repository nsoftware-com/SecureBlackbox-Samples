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
};

using namespace ArgParser;

void displayHelp() {
    printf(
        "NAME\n"
        "  simplepdfsigner -- SecureBlackbox PDFSigner Demo Application\n\n"
        "SYNOPSIS\n"
        "  simplepdfsigner [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password]\n"
        "             [-signbody] [-hashalg hash_algorithm] [-sigtype signature_type]\n\n"
        "DESCRIPTION\n"
        "  PDFSigner demonstrates the usage of PDFSigner from SecureBlackbox.\n"
        "  This sample illustrates the use of PDFSigner component for signing PDF documents. \n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the signed file will be saved (Required).\n\n"
        "  -cert         The certificate used to sign files.\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
        "  -pkcs11       The pkcs11 storage used to sign file.\n\n"
        "  -pin          The PIN for pkcs11 storage\n\n"
        "  -win32        The win32 store name\n\n"
        "EXAMPLES\n"
        "  simplepdfsigner -input C:\\pdf\\myfile.pdf -output C:\\pdf\\mysignedfile.pdf -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n"
        "  simplepdfsigner -input C:\\pdf\\myfile.pdf -output C:\\pdf\\mysignedfile.pdf -pkcs11 C:\\pkcs11\\pkcs11.dll -pin mypassword \n\n"
        "  simplepdfsigner -input C:\\pdf\\myfile.pdf -output C:\\pdf\\mysignedfile.pdf -win32 My \n\n"
    );
}

int main(int argc, char** argv) {
    PDFSigner signer;
    CertificateManager cm;
    CertificateStorage cs;

    // Validate input
    if (argc < 6) {
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
    char* certpass = optval(argc, argv, "-certpass");
    char* pkcs11File = optval(argc, argv, "-pkcs11");
    char* pin = optval(argc, argv, "-pin");
    char* win32Store = optval(argc, argv, "-win32");

    if (!strcmp(cert, "") && !strcmp(pkcs11File, "") && !strcmp(win32Store, "")) {
        printf("-cert or -pkcs11 or -win32 is required.");
        displayHelp();
        goto done;
    }

    if (strcmp(cert, "") && strcmp(pkcs11File, "") || strcmp(cert, "") && strcmp(win32Store, "") || strcmp(pkcs11File, "")
        && strcmp(win32Store, "")) {
        printf("Use only one -cert or -pkcs11 or -win32 parameter.");
        displayHelp();
        goto done;
    }

    if (strcmp(cert, "")) {
        cm.ImportFromFile(cert, certpass);
        signer.SetSigningCertHandle(cm.GetCertHandle());
    } else {
        char request[100];

        if (strcmp(pkcs11File, "")) {
            strcpy(request, "pkcs11://user:");
            strcat(request, pin);
            strcat(request, "@/");
            strcat(request, pkcs11File);
        } else {
            strcpy(request, "system://?store=");
            strcat(request, win32Store);
        }

        if (cs.Open(request)) {
            goto done;
        }

        signer.SetSigningCertHandle(cs.GetCertHandle(0));
    }

    signer.SetSigLevel(1); // pslBES
    signer.SetSigInvisible(false);
    signer.SetIgnoreChainValidationErrors(true);

    // Required options
    signer.SetInputFile(input);
    signer.SetOutputFile(output);

    // Sign
    if (signer.Sign()) {
        goto done;
    }
    printf("PDF file successfully signed.\n");

done:
    if (signer.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", signer.GetLastErrorCode(), signer.GetLastError());
    }
    getchar();
}


