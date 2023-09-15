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
#define LINE_LEN 100

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

JAdESSigner jadessigner;
CertificateManager certificatemanager;

int showHelp() {
    printf("Usage:\n"
        "\t./jadessigner\t-dataFile <dataFile> -outputFile <outputFile>\n"
        "\t\t\t-signingKey <path> -keyPassword <password> [-option ]?\n"
    );
    printf("Options: \n");
    printf("\t-dataFile           (string) The payload to be signed.\n");
    printf("\t-outputFile         (string) Where to save the JWS/JAdES signature.\n");
    printf("\t-signingKey         (string) The key file to be imported.\n");
    printf("\t-keyPassword        (string) The key password.\n");
    printf("\t-sigLevel           (int) The JAdES signature level.\n");
    printf("\t-detached           (switch) Whether a detached signature should be produced.\n");
    printf("\t-compactForm        (switch) Whether the JWS compact serialization should be used.\n");
    printf("\t-flattenedSignature (switch) Whether the flattened signature should be created.\n");
    exit(1);
}

int main(int argc, char** argv) {
    if (argc < 9) {
        fprintf(stderr, "Error: Missing arguments.\n");
        showHelp();
    }

    optext(argc, argv, "-dataFile") ? jadessigner.SetDataFile(optval(argc, argv, "-dataFile")) : showHelp();
    optext(argc, argv, "-outputFile") ? jadessigner.SetOutputFile(optval(argc, argv, "-outputFile")) : showHelp();
    optext(argc, argv, "-sigLevel")
        ? jadessigner.SetNewSigLevel(atoi(optval(argc, argv, "-sigLevel")))
        : jadessigner.SetNewSigLevel(JASL_BASELINE_B);
    optext(argc, argv, "-detached")
      ? jadessigner.SetDetached(true)
      : jadessigner.SetDetached(false);
    optext(argc, argv, "-compactForm")
      ? jadessigner.SetCompactForm(true)
      : jadessigner.SetCompactForm(false);
    optext(argc, argv, "-flattenedSignature")
      ? jadessigner.SetFlattenedSignature(true)
      : jadessigner.SetFlattenedSignature(false);

    if (optext(argc, argv, "-signingKey") && optext(argc, argv, "-keyPassword")) {
        char* signingKeyPath = optval(argc, argv, "-signingKey");
        char* signingKeyPassword = optval(argc, argv, "-keyPassword");
        certificatemanager.ImportFromFile(signingKeyPath, signingKeyPassword);
        jadessigner.SetSigningCertHandle(certificatemanager.GetCertHandle());
    } else {
        showHelp();
    }

    const int retcode = jadessigner.Sign();

    if (retcode) {
        printf("Error [%d]: %s", jadessigner.GetLastErrorCode(), jadessigner.GetLastError());
        return 0;
    } else {
        printf("JWS/JAdES signature successfully created.\n");
    };

    fprintf(stderr, "\npress <return> to continue...\n");
    getchar();
    return 0;
}


