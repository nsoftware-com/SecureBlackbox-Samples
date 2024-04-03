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
        "  officedecryptor -- SecureBlackbox OfficeDecryptor Demo Application\n\n"
        "SYNOPSIS\n"
        "  officedecryptor [-input input_file] [-output output_file] [-pass decryption_password] \n\n"
        "DESCRIPTION\n"
        "  OfficeDecryptor demonstrates the usage of OfficeDecryptor from SecureBlackbox.\n"
        "  Used to decrypt office documents. \n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to decrypt (Required).\n\n"
        "  -output       Where the decrypted file will be saved (Required).\n\n"
        "  -pass         Password for file encryption (Required).\n\n"
    );
}

int main(int argc, char** argv) {
    OfficeDecryptor decryptor;

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

    // Required options
    decryptor.SetInputFile(input);
    decryptor.SetOutputFile(output);
    decryptor.SetPassword(pass);

    // Decrypt
    if (decryptor.Decrypt()) {
        goto done;
    }
    printf("Office document successfully decrypted.\n");

done:
    if (decryptor.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", decryptor.GetLastErrorCode(), decryptor.GetLastError());
    }
    getchar();
}


