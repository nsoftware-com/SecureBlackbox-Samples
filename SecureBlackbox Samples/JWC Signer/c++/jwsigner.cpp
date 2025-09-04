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
        "  jwsigner -- SecureBlackbox SymmetricCrypto Demo Application\n\n"
        "SYNOPSIS\n"
		"  jwsigner <-s/-v> <-input input_data> <-cert certificate_file> [-certpass certificate_password [-sig signature_data]] [-compact]\n\n"
        "DESCRIPTION\n"
        "  This sample illustrates how to create a detached signature over a text string.\n"
        "  Used to sign and verify data.\n\n"
        "  The options are as follows:\n\n"
        "  -s            Whether to sign input data. \n\n"
        "  -v            Whether to verify signature data. \n\n"
        "  -input        An input data to sign/verify (Required). \n\n"
        "  -cert         The certificate used to encrypt file (Required). \n\n"
        "  -certpass     The password for the certificate. \n\n"
        "  -sig          An signature data to verify (Required to verify). \n\n"
        "  -compact      Whether to use compact format \n\n"
        "EXAMPLES\n"
        "	jwsigner -s -input \"And now that you don’t have to be perfect, you can be good.\" -cert C:\\\\certs\\\\mycert.pfx -certpass mypassword \n\n"
        "	jwsigner -v -input \"And now that you don’t have to be perfect, you can be good.\" \n"
        "		-sig eyJhbGciOiJkaXIiLCJlbmMiOiJBMjU2Q0JDLUhTNTEyIn0..kuN2U -cert C:\\\\certs\\\\mycert.pfx -certpass mypassword -compact \n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
    PublicKeyCrypto crypto;
    CertificateManager cm;
    CryptoKeyManager ckm;
    bool sign = false;
    bool verify = false;
    bool compact = false;

    // Validate input
    if (argc < 2) {
        displayHelp("");
        goto done;
    }

    if (optext(argc, argv, "-s")) {
        sign = true;
    }

    if (optext(argc, argv, "-v")) {
        verify = true;
    }

    if (!(sign || verify)) {
        displayHelp("-s or -v is required.");
        goto done;
    }

    if (sign && verify) {
        displayHelp("Use only one -s or -v parameter.");
        goto done;
    }

    char* input = optval(argc, argv, "-input");
    if (!strcmp(input, "")) {
        displayHelp("-input is required.");
        goto done;
    }

    char* sig = optval(argc, argv, "-sig");
    if (verify && !strcmp(sig, "")) {
        displayHelp("-sig is required.");
        goto done;
    }

    char* cert = optval(argc, argv, "-cert");
    if (!strcmp(cert, "")) {
        displayHelp("-cert is required.");
        goto done;
    }

    char* certpass = optval(argc, argv, "-certpass");

    if (optext(argc, argv, "-compact")) compact = true;

    // Key from Cert file
    if (cm.ImportFromFile(cert, certpass)) {
        goto err;
    }
    ckm.SetCertHandle(cm.GetCertHandle());
    if (ckm.ImportFromCert() || crypto.SetKeyHandle(ckm.GetKeyHandle())) {
        goto err;
    }

    // Sign 
    if (sign) {
        if (compact) {
            crypto.SetOutputEncoding(CET_COMPACT);
        } else {
            crypto.SetOutputEncoding(CET_JSON);
        }

        sig = crypto.Sign(input, strlen(input), true);
        if (!sig) {
            goto err;
        }
        char* res = new char[1000];
        strcpy(res, "Signature: ");
        strcat(res, sig);
        printf(res);
        delete[] res;
    } else {
        // Verify 
        if (compact) {
            crypto.SetInputEncoding(CET_COMPACT);
        } else {
            crypto.SetInputEncoding(CET_JSON);
        }

        if (crypto.VerifyDetached(input, strlen(input), sig, strlen(sig))) goto err;

        switch (crypto.GetSignatureValidationResult()) {
        case SVT_VALID:
            printf("Verification succeeded.\n\n");
            break;
        case SVT_CORRUPTED:
            printf("Verification corrupted.\n\n");
            break;
        case SVT_SIGNER_NOT_FOUND:
            printf("Signer not found.\n\n");
            break;
        case SVT_FAILURE:
            printf("Verification failed.\n\n");
            break;
        default:
            printf("Verification unknown.\n\n");
            break;
        }
    }

err:
    if (crypto.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", crypto.GetLastErrorCode(), crypto.GetLastError());
    }
done:
	printf("Press Enter to exit the demo.\n");
    getchar();
}


