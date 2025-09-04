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
        "  publickeycrypto -- SecureBlackbox PublicKeyCrypto Demo Application\n\n"
        "SYNOPSIS\n"
		"  publickeycrypto <-s/-v> <-input input_file> <-cert certificate_file> [-certpass certificate_password] \n"
        "             [-output output_file] [-sig signature_file] [-encoding encoding]\n\n"
        "DESCRIPTION\n"
        "  PublicKeyCrypto demonstrates the usage of PublicKeyCrypto from SecureBlackbox.\n"
        "  Used to sign and verify files.\n\n"
        "  The options are as follows:\n\n"
        "  -s            Sign input file and save to output \n\n"
        "  -v            Verify signature using original file (input) \n\n"
        "  -input        An input file to sign or verify (Required). \n\n"
        "  -cert         The certificate used to sign files (Required).\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
        "  -output       Where the signed file will be saved (Required for signing). \n\n"
        "  -sig          An signature file to verify (Required on verifing). \n\n"
        "  -encoding     The encoding of hash. Valid values:\n\n"
        "                  0 - CET_DEFAULT\n"
        "                  1 - CET_BINARY\n"
        "                  2 - CET_BASE_64\n"
        "                  3 - CET_COMPACT\n"
        "                  4 - CET_JSON\n\n"
        "EXAMPLES\n"
        "  publickeycrypto -s -input C:\\cypto\\helloworld.txt -output C:\\cypto\\signature.dat -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n"
        "  publickeycrypto -v -input C:\\cypto\\helloworld.txt -sig C:\\cypto\\signature.dat -cert C:\\certs\\mycert.pfx -certpass mypassword -encoding 2 \n\n"
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

    char* output = optval(argc, argv, "-output");
    if (sign && !strcmp(output, "")) {
        displayHelp("-output is required.");
        goto done;
    }

    char* signature = optval(argc, argv, "-sig");
    if (verify && !strcmp(signature, "")) {
        displayHelp("-sig is required.");
        goto done;
    }

    char* cert = optval(argc, argv, "-cert");
    if (!strcmp(cert, "")) {
        displayHelp("-cert is required.");
        goto done;
    }

    char* certpass = optval(argc, argv, "-certpass");

    if (cm.ImportFromFile(cert, certpass)) {
		printf("Error: [%i] %s\n", cm.GetLastErrorCode(), cm.GetLastError());
        goto done;
    }

    ckm.SetCertHandle(cm.GetCertHandle());
    ckm.ImportFromCert();

    if (crypto.SetKeyHandle(ckm.GetKeyHandle())) {
        goto err;
    }

    // Sign 
    if (sign) {
        if (optext(argc, argv, "-encoding")) {
            crypto.SetOutputEncoding(atoi(optval(argc, argv, "-encoding")));
        }
		else {
			crypto.SetOutputEncoding(2);
		}

        if (crypto.SignFile(input, output, true)) {
            goto err;
        }
        printf("The file was signed successfully.\n\n");
    } else {
        // Verify 
        if (optext(argc, argv, "-encoding")) {
            crypto.SetInputEncoding(atoi(optval(argc, argv, "-encoding")));
        }
		else {
			crypto.SetInputEncoding(2);
		}

        if (crypto.VerifyDetachedFile(input, signature)) {
            goto err;
        }

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


