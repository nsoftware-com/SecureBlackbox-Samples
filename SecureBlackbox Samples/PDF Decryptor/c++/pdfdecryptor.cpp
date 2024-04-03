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
PDFDecryptor pdfdecryptor;
CertificateManager certificatemanager;

int showHelp() {
    printf("Usage:\n"
        "\t./pdfdecryptor\t-inputFile <inputFile> -outputFile <outputFile>\n"
        "\t\t\t(-keyPath <path> -keyPassword <password> | -userPassword <password>)\n");
    printf("Options: \n");
    printf("\t-inputFile     (string) The PDF file to be decrypted.\n");
    printf("\t-outputFile    (string) Where to save the decrypted document.\n");
    printf("\t-keyPath       (string) The key file to be imported.\n");
    printf("\t-keyPassword   (string) The key password.\n");
    printf("\t-userPassword  (string) The password used for decryption.\n");
    exit(1);
}

int main(int argc, char** argv) {
    if (argc < 7) {
        fprintf(stderr, "Error: Missing arguments.\n");
        showHelp();
    }

    optext(argc, argv, "-inputFile") ? pdfdecryptor.SetInputFile(optval(argc, argv, "-inputFile")) : showHelp();
    optext(argc, argv, "-outputFile") ? pdfdecryptor.SetOutputFile(optval(argc, argv, "-outputFile")) : showHelp();

    if (optext(argc, argv, "-keyPath") && optext(argc, argv, "-keyPassword")) {
        char* keyPath = optval(argc, argv, "-keyPath");
        char* keyPassword = optval(argc, argv, "-keyPassword");
        certificatemanager.ImportFromFile(keyPath, keyPassword);
        pdfdecryptor.SetDecryptionCertCount(1);
        pdfdecryptor.SetDecryptionCertHandle(0, certificatemanager.GetCertHandle());
    } else if (optext(argc, argv, "-userPassword")) {
        char* password = optval(argc, argv, "-userPassword");
        pdfdecryptor.SetPassword(password);
    } else {
        showHelp();
    }
    const int retcode = pdfdecryptor.Decrypt();

    if (retcode) {
        printf("Error [%d]: %s", pdfdecryptor.GetLastErrorCode(), pdfdecryptor.GetLastError());
        return 0;
    } else {
        printf("The document was decrypted successfully.\n");
    };

    fprintf(stderr, "\npress <return> to continue...\n");
    getchar();
    return 0;

}


