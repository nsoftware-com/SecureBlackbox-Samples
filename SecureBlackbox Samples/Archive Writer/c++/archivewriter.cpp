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
#include <io.h>
#include <iostream>

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
        "  archivewriter -- SecureBlackbox ArchiveWriter Demo Application\n\n"
        "SYNOPSIS\n"
		"  archivewriter <-arctype archive_type> <-input input1 input2 ....> <-output output_file> [-pass encryption_password] [-enctype encryption_type]\n\n"
        "DESCRIPTION\n"
        "  ArchiveWriter demonstrates the usage of ArchiveWriter from SecureBlackbox.\n"
        "  Used to create archive.\n\n"
        "  The options are as follows:\n\n"
        "  -arctype      The type of archive (Required). Valid values:\n\n"
        "                  1 - AFT_ZIP\n"
        "                  2 - AFT_GZIP\n"
        "                  3 - AFT_BZIP_2\n"
        "                  4 - AFT_TAR\n"
        "                  5 - AFT_TAR_GZIP\n"
        "                  6 - AFT_TAR_BZIP_2\n\n"
        "  -input        List of files or directories compressing to archive (Required).\n\n"
        "  -output       Where the archive file will be saved (Required).\n\n"
        "  -pass         The password for the encryption.\n\n"
        "  -enctype      The encryption type. Valid values: \n\n"
        "                  0 - AET_DEFAULT\n"
        "                  1 - AET_NO_ENCRYPTION\n"
        "                  2 - AET_GENERIC\n"
        "                  3 - AET_WIN_ZIP\n"
        "                  4 - AET_STRONG\n\n"
        "EXAMPLES\n"
        "  archivewriter -arctype 1 -input C:\\archive\\helloworld.txt -output C:\\archive\\myarchive.zip \n\n"
        "  archivewriter -arctype 5 -input C:\\archive\\helloworld.txt C:\\archive\\temp -output C:\\archive\\myarchive.tar -enctype 2 -pass mypassword \n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

bool FileExists(const char* fname) {
    _finddata_t data;
    long nFind = _findfirst(fname, &data);
    if (nFind != -1) {
        _findclose(nFind);

        if (data.attrib & _A_SUBDIR) {
            return false;
        } else {
            return true;
        }
    }
    return false;
}

std::string getFileName(const char* s) {
    std::string str(s);
    const std::size_t found = str.find_last_of("/\\");
    if (found != std::string::npos) {
        str = str.substr(found + 1);
    }
    return str;
}

int main(int argc, char** argv) {
    ArchiveWriter archive;
    int arctype;

    // Validate input
    if (argc <= 1) {
        displayHelp("");
        goto done;
    }

    if (optext(argc, argv, "-arctype")) {
        arctype = atoi(optval(argc, argv, "-arctype"));
    } else {
        displayHelp("-arctype is required.");
        goto done;
    }

    if (!strcmp(optval(argc, argv, "-input"), "")) {
        displayHelp("-input is required.");
        goto done;
    }

    char* output = optval(argc, argv, "-output");
    if (!strcmp(output, "")) {
        displayHelp("-output is required.");
        goto done;
    }

    // Additional options
    if (optext(argc, argv, "-enctype")) {
        archive.SetEncryptionType(atoi(optval(argc, argv, "-enctype")));
        archive.SetEncryptionPassword(optval(argc, argv, "-pass"));
    }

    // Create archive
    if (archive.CreateNew(arctype)) goto err;

    // Add input files
    for (int x = 0; x < argc; x++)
        if (!strcmp(argv[x], "-input") && x != argc - 1) {
            for (int i = x + 1; i < argc; i++) {
                if (argv[i][0] == '-') break;

                if (FileExists(argv[i])) // file exists
                {
                    archive.AddFile(getFileName(argv[i]).c_str(), argv[i]);
                } else {
                    archive.AddFiles("", argv[i], true);
                }
            }

            break;
        }

    printf("List of files added to the archive\n");
    for (int x = 0; x < archive.GetFileCount(); x++) {
        printf("    %s\n", archive.GetFilePath(x));
    }
    printf("\n");

    // Save archive
    if (archive.Save(output)) goto err;

    printf("Archive created.\n\n");

err:
    if (archive.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", archive.GetLastErrorCode(), archive.GetLastError());
    }
done:
	printf("Press Enter to exit the demo.\n");
    getchar();
}


