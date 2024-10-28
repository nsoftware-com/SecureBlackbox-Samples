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
        "  messagecompressor -- SecureBlackbox MessageCompressor Demo Application\n\n"
        "SYNOPSIS\n"
		"  messagecompressor <-input input_file> <-output output_file> [-level level]\n\n"
        "DESCRIPTION\n"
        "  MessageCompressor demonstrates the usage of MessageCompressor from SecureBlackbox.\n"
        "  Used to create compressed PKCS#7 messages.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to compress (Required).\n\n"
        "  -output       Where the compressed file will be saved (Required).\n\n"
        "  -level        The comression level. \n\n"
        "EXAMPLES\n"
        "  messagecompressor -input C:\\pkcs7\\helloworld.txt -output C:\\pkcs7\\compress.pkcs7 \n\n"
        "  messagecompressor -input C:\\pkcs7\\helloworld.txt -output C:\\pkcs7\\compress.pkcs7 -level 7 \n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
    MessageCompressor compressor;

    // Validate input
    if (argc < 2) {
        displayHelp("");
        goto done;
    }

    char* input = optval(argc, argv, "-input");
    if (!strcmp(input, "")) {
        displayHelp("-input is required.");
        goto done;
    }

    char* output = optval(argc, argv, "-output");
    if (!strcmp(output, "")) {
        displayHelp("-output is required.");
        goto done;
    }

    // Additional options
    if (optext(argc, argv, "-level")) {
        compressor.SetCompressionLevel(atoi(optval(argc, argv, "-level")));
    }

    // Required options
    compressor.SetInputFile(input);
    compressor.SetOutputFile(output);

    // Compress
    if (compressor.Compress()) {
        goto err;
    }
    printf("The file successfully compressed.\n\n");

err:
    if (compressor.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", compressor.GetLastErrorCode(), compressor.GetLastError());
    }
done:
	printf("Press Enter to exit the demo.\n");
    getchar();
}


