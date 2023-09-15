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
        "  messagedecompressor -- SecureBlackbox MessageDecompressor Demo Application\n\n"
        "SYNOPSIS\n"
        "  messagedecompressor [-input input_file] [-output output_file]\n\n"
        "DESCRIPTION\n"
        "  MessageDecompressor demonstrates the usage of MessageDecompressor from SecureBlackbox.\n"
        "  Used to decompressed PKCS#7 messages.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to decompress (Required).\n\n"
        "  -output       Where the original file will be saved (Required).\n\n"
    );
}

int main(int argc, char** argv) {
    MessageDecompressor decompressor;

    // Validate input
    if (argc < 4) {
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

    // Required options
    decompressor.SetInputFile(input);
    decompressor.SetOutputFile(output);

    // Decompress
    if (decompressor.Decompress()) {
        goto done;
    }
    printf("The file successfully decompressed.\n");

done:
    if (decompressor.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", decompressor.GetLastErrorCode(), decompressor.GetLastError());
    }
    getchar();
}


