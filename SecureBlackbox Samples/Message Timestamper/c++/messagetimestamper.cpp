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
        "  messagetimestamper -- SecureBlackbox MessageTimestamper Demo Application\n\n"
        "SYNOPSIS\n"
		"  messagetimestamper <-input input_file> <-output output_file> <-tsserver timestamp_server> [-detached] \n\n"
        "DESCRIPTION\n"
        "  MessageTimestamper demonstrates the usage of MessageTimestamper from SecureBlackbox.\n"
        "  Used to create timestamped PKCS#7 messages. \n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to timestamped (Required).\n\n"
        "  -output       Where the timestamping file will be saved (Required).\n\n"
        "  -tsserver     A timestamp server to use during timestamping (Required).\n\n"
        "  -detached     Whether to use detached timestamping.\n\n"
        "EXAMPLES\n"
        "  messagetimestamper -input C:\\pkcs7\\helloworld.txt -output C:\\pkcs7\\mymes.pkcs7 -tsserver http://timestamp.wosign.com \n\n"
        "  messagetimestamper -input C:\\pkcs7\\helloworld.txt -output C:\\pkcs7\\mymes.pkcs7 -tsserver http://timestamp.wosign.com -detached \n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
    MessageTimestamper timestamper;

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

    char* tsserver = optval(argc, argv, "-tsserver");
    if (!strcmp(tsserver, "")) {
        displayHelp("-tsserver is required.");
        goto done;
    }

    // Additional options
    if (optext(argc, argv, "-detached")) {
        timestamper.SetDetached(true);
    }

    // Required options
    timestamper.SetInputFile(input);
    timestamper.SetOutputFile(output);
    timestamper.SetTimestampServer(tsserver);

    // Timestamp
    if (timestamper.Timestamp()) {
        goto err;
    }
    printf("The file successfully timestamped.\n\n");

err:
    if (timestamper.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", timestamper.GetLastErrorCode(), timestamper.GetLastError());
    }
done:
	printf("Press Enter to exit the demo.\n");
    getchar();
}


