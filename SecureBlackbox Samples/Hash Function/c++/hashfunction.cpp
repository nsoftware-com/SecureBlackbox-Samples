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
}

static bool optext(int argc, char** argv, const char* option) {
    for (int x = 0; x < argc; x++) {
        if (!strcmp(argv[x], option)) {
            return true;
        }
    }
    return false;
}

using namespace ArgParser;

void displayHelp() {
    printf(
        "NAME\n"
        "  hashfunction -- SecureBlackbox HashFunction Demo Application\n\n"
        "SYNOPSIS\n"
        "  hashfunction [-input input_file] [-pass password] [-encoding encoding_type]\n\n"
        "DESCRIPTION\n"
        "  HashFunction demonstrates the usage of HashFunction from SecureBlackbox.\n"
        "  Used to create an hash from file.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to hash (Required).\n\n"
        "  -pass         The password for derive key.\n\n"
        "  -encoding     The encoding of hash. Valid values:\n\n"
        "                  0 - CET_DEFAULT\n"
        "                  1 - CET_BINARY\n"
        "                  2 - CET_BASE_64\n"
        "                  3 - CET_COMPACT\n"
        "                  4 - CET_JSON\n\n"
        "EXAMPLES\n"
        "  hashfunction -input C:\\hash\\helloworld.txt \n\n"
        "  hashfunction -input C:\\hash\\helloworld.txt -encoding 2 -pass mypassword \n\n"
    );
}

int main(int argc, char** argv) {
    HashFunction hash;
    CryptoKeyManager cm;

    // Validate input
    if (argc < 2) {
        displayHelp();
        goto done;
    }

    char* input = optval(argc, argv, "-input");
    if (!strcmp(input, "")) {
        printf("-input is required.");
        displayHelp();
        goto done;
    }

    char* pass = optval(argc, argv, "-pass");
    if (strcmp(pass, "")) {
        cm.DeriveKey(128, pass, "");

        hash.SetKeyHandle(cm.GetKeyHandle());
    }

    // Additional options
    if (optext(argc, argv, "-encoding")) {
        hash.SetOutputEncoding(atoi(optval(argc, argv, "-encoding")));
    }

    // Calculate hash
    char* output = hash.HashFile(input);

    if (hash.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", hash.GetLastErrorCode(), hash.GetLastError());
    } else {
        printf("Calculated hash: %s.\n", output);
    }

done:
    getchar();
}


