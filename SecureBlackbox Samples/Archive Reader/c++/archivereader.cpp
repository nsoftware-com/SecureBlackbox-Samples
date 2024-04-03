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
        "  archivereader -- SecureBlackbox ArchiveReader Demo Application\n\n"
        "SYNOPSIS\n"
        "  archivereader [-arctype archive_type] [-input input_file] [-output output_path] [-pass decryption_password]\n\n"
        "DESCRIPTION\n"
        "  ArchiveReader demonstrates the usage of ArchiveReader from SecureBlackbox.\n"
        "  Used to extract files from archive.\n\n"
        "  The options are as follows:\n\n"
        "  -arctype      The type of archive (Required). Valid values:\n\n"
        "                  1 - AFT_ZIP\n"
        "                  2 - AFT_GZIP\n"
        "                  3 - AFT_BZIP_2\n"
        "                  4 - AFT_TAR\n"
        "                  5 - AFT_TAR_GZIP\n"
        "                  6 - AFT_TAR_BZIP_2\n\n"
        "  -input        An input archive file (Required).\n\n"
        "  -output       Where the extracted files will be saved.\n\n"
        "  -pass         The password for the encrypted archive.\n\n"
        "EXAMPLES\n"
        "  archivereader -arctype 1 -input C:\\archive\\helloworld.zip -output C:\\archive \n\n"
        "  archivereader -arctype 5 -input C:\\archive\\helloworld.tar -pass mypassword \n\n"
    );
}

int main(int argc, char** argv) {
    ArchiveReader archive;
    char *input, *output;
    int arctype;

    // Validate input
    if (argc < 4) {
        displayHelp();
        goto done;
    }

    if (optext(argc, argv, "-arctype")) {
        arctype = atoi(optval(argc, argv, "-arctype"));
    } else {
        printf("-arctype is required.");
        displayHelp();
        goto done;
    }

    input = optval(argc, argv, "-input");
    if (!strcmp(input, "")) {
        printf("-input is required.");
        displayHelp();
        goto done;
    }

    output = optval(argc, argv, "-output");

    // Additional options
    archive.SetDecryptionPassword(optval(argc, argv, "-pass"));

    // Open archive
    if (archive.Open(arctype, input)) goto done;

    printf("List of files in the archive\n");
    for (int x = 0; x < archive.GetFileCount(); x++) {
        printf("    %s\n", archive.GetFilePath(x));
    }
    printf("\n");

    // Extract all files
    if (archive.ExtractAll(output, 0)) {
        goto done;
    }

    printf("All files from archive extracted.\n");

done:
    if (archive.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", archive.GetLastErrorCode(), archive.GetLastError());
    }
    getchar();
}


