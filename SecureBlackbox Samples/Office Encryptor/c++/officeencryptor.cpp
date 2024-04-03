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
        "  officeencryptor -- SecureBlackbox OfficeEncryptor Demo Application\n\n"
        "SYNOPSIS\n"
        "  officeencryptor [-input input_file] [-output output_file] [-pass encryption_password] [-enctype encryption_type] [-encalg encryption_algorithm] \n\n"
        "DESCRIPTION\n"
        "  OfficeEncryptor demonstrates the usage of OfficeEncryptor from SecureBlackbox.\n"
        "  Used to encrypt office documents. \n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to encrypt (Required).\n\n"
        "  -output       Where the encrypted file will be saved (Required).\n\n"
        "  -pass         Password for file encryption (Required).\n\n"
        "  -enctype      The type of encryption to use. Enter the corresponding number. Valid values:\n\n"
        "                  0 - OET_DEFAULT\n"
        "                  1 - OET_BINARY_RC4\n"
        "                  2 - OET_BINARY_RC4CRYPTO_API\n"
        "                  3 - OET_OPEN_XMLSTANDARD\n"
        "                  4 - OET_OPEN_XMLAGILE\n"
        "                  5 - OET_OPEN_DOCUMENT\n\n"
        "  -encalg       The encryption algorithm to use. Valid values: RC2, RC4, DES, 3DES, AES128, AES192, AES256, Blowfish \n\n"
        "EXAMPLES\n"
        "  officeencryptor -input C:\\office\\helloworld.doc -output C:\\office\\helloworld.enc -pass mypassword \n\n"
        "  officeencryptor -input C:\\office\\helloworld.doc -output C:\\office\\helloworld.enc -pass mypassword -enctype 2 -encalg AES128 \n\n"
    );
}

int main(int argc, char** argv) {
    OfficeEncryptor encryptor;

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

    char* pass = optval(argc, argv, "-pass");
    if (!strcmp(pass, "")) {
        printf("-pass is required.");
        displayHelp();
        goto done;
    }

    // Additional options
    if (optext(argc, argv, "-enctype")) {
        encryptor.SetEncryptionType(atoi(optval(argc, argv, "-enctype")));
    }

    if (optext(argc, argv, "-encalg")) {
        encryptor.SetEncryptionAlgorithm(optval(argc, argv, "-encalg"));
    }

    // Required options
    encryptor.SetInputFile(input);
    encryptor.SetOutputFile(output);
    encryptor.SetPassword(pass);

    // Encrypt
    if (encryptor.Encrypt()) {
        goto done;
    }
    printf("Office document successfully encrypted.\n");

done:
    if (encryptor.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", encryptor.GetLastErrorCode(), encryptor.GetLastError());
    }
    getchar();
}


