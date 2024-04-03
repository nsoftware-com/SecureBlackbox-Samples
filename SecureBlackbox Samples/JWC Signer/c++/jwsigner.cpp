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
        "  jwsigner -- SecureBlackbox SymmetricCrypto Demo Application\n\n"
        "SYNOPSIS\n"
        "  jwsigner -s/-v [-input input_data] [-sig signature_data] [-cert certificate_file] [-certpass certificate_password] [-compact]\n\n"
        "DESCRIPTION\n"
        "  This sample illustrates how to create a detached signature over a text string.\n"
        "  Used to sign and verify data.\n\n"
        "  The options are as follows:\n\n"
        "  -s            Whether to sign input data. \n\n"
        "  -v            Whether to verify signature data. \n\n"
        "  -input        An input data to sign/verify (Required). \n\n"
        "  -sig          An signature data to verify (Required to verify). \n\n"
        "  -cert         The certificate used to encrypt file (Required). \n\n"
        "  -certpass     The password for the certificate. \n\n"
        "  -compact      Whether to use compact format \n\n"
        "EXAMPLES\n"
        "	jwsigner -s -input \"And now that you don’t have to be perfect, you can be good.\" -cert C:\\\\certs\\\\mycert.pfx -certpass mypassword \n\n"
        "	jwsigner -v -input \"And now that you don’t have to be perfect, you can be good.\" \n"
        "		-sig eyJhbGciOiJkaXIiLCJlbmMiOiJBMjU2Q0JDLUhTNTEyIn0..kuN2U -cert C:\\\\certs\\\\mycert.pfx -certpass mypassword -compact \n\n"
    );
}

int main(int argc, char** argv) {
    PublicKeyCrypto crypto;
    CertificateManager cm;
    CryptoKeyManager ckm;
    bool sign = false;
    bool verify = false;
    bool compact = false;

    // Validate input
    if (argc < 6) {
        displayHelp();
        goto done;
    }

    if (optext(argc, argv, "-s")) {
        sign = true;
    }

    if (optext(argc, argv, "-v")) {
        verify = true;
    }

    if (!(sign || verify)) {
        printf("-s or -v is required.\n");
        displayHelp();
        goto done;
    }

    if (sign && verify) {
        printf("Use only one -s or -v parameter.\n");
        displayHelp();
        goto done;
    }

    char* input = optval(argc, argv, "-input");
    if (!strcmp(input, "")) {
        printf("-input is required.\n");
        displayHelp();
        goto done;
    }

    char* sig = optval(argc, argv, "-sig");
    if (verify && !strcmp(sig, "")) {
        printf("-sig is required.\n");
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

    if (optext(argc, argv, "-compact")) compact = true;

    // Key from Cert file
    if (cm.ImportFromFile(cert, certpass)) {
        goto done;
    }
    ckm.SetCertHandle(cm.GetCertHandle());
    if (ckm.ImportFromCert() || crypto.SetKeyHandle(ckm.GetKeyHandle())) {
        goto done;
    }

    // Sign 
    if (sign) {
        if (compact) {
            crypto.SetOutputEncoding(3); //cetCompact
        } else {
            crypto.SetOutputEncoding(4); //cetJSON
        }

        sig = crypto.Sign(input, strlen(input), true);
        if (!sig) {
            goto done;
        }
        char* res = new char[1000];
        strcpy(res, "Signature: ");
        strcat(res, sig);
        printf(res);
        delete[] res;
    } else {
        // Verify 
        if (compact) {
            crypto.SetInputEncoding(3); //cetCompact
        } else {
            crypto.SetInputEncoding(4); //cetJSON
        }

        if (crypto.VerifyDetached(input, strlen(input), sig, strlen(sig))) goto done;

        switch (crypto.GetSignatureValidationResult()) {
        case SVT_VALID:
            printf("Verification succeeded.\n");
            break;
        case SVT_CORRUPTED:
            printf("Verification corrupted.\n");
            break;
        case SVT_SIGNER_NOT_FOUND:
            printf("Signer not found.\n");
            break;
        case SVT_FAILURE:
            printf("Verification failed.\n");
            break;
        default:
            printf("Verification unknown.\n");
            break;
        }
    }

done:
    if (crypto.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", crypto.GetLastErrorCode(), crypto.GetLastError());
    }
    getchar();
}


