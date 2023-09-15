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
PDFEncryptor pdfencryptor;
CertificateManager certificatemanager;

int showHelp() {
    printf("Usage:\n"
        "\t./pdfencryptor\t-inputFile <inputFile> -outputFile <outputFile>\n"
        "\t\t\t(-keyPath <path> -keyPassword <password> | -userPassword <password>) [-options ]?\n");
    printf("Options: \n");
    printf("\t-inputFile           (string) The PDF file to be encrypted.\n");
    printf("\t-outputFile          (string) Where to save the encrypted document.\n");
    printf("\t-keyPath             (string) The key file to be imported.\n");
    printf("\t-keyPassword         (string) The key password.\n");
    printf("\t-userPassword        (string) The password used for encryption.\n");
    printf("\t-encryptionAlgorithm (string) The encryption algorithm used to encrypt the document.\n");
    printf("\t-noMetadata          (switch) Specifies metadata should not be encrypted.\n");
    exit(1);
}

int main(int argc, char** argv) {
    if (argc < 7) {
        fprintf(stderr, "Error: Missing arguments.\n");
        showHelp();
    }

    optext(argc, argv, "-inputFile") ? pdfencryptor.SetInputFile(optval(argc, argv, "-inputFile")) : showHelp();
    optext(argc, argv, "-outputFile") ? pdfencryptor.SetOutputFile(optval(argc, argv, "-outputFile")) : showHelp();
    optext(argc, argv, "-encryptionAlgorithm")
        ? pdfencryptor.SetEncryptionAlgorithm(optval(argc, argv, "-encryptionAlgorithm"))
        : pdfencryptor.SetEncryptionAlgorithm("AES256");
    optext(argc, argv, "-noMetadata") ? pdfencryptor.SetEncryptMetadata(false) : pdfencryptor.SetEncryptMetadata(true);

    if (optext(argc, argv, "-keyPath") && optext(argc, argv, "-keyPassword")) {
        char* keyPath = optval(argc, argv, "-keyPath");
        char* keyPassword = optval(argc, argv, "-keyPassword");
        certificatemanager.ImportFromFile(keyPath, keyPassword);
        pdfencryptor.SetEncryptionType(PET_CERTIFICATE);
        pdfencryptor.SetEncryptionCertCount(1);
        pdfencryptor.SetEncryptionCertificateHandle(certificatemanager.GetCertHandle());
    } else if (optext(argc, argv, "-userPassword")) {
        char* password = optval(argc, argv, "-userPassword");
        pdfencryptor.SetEncryptionType(PET_PASSWORD);
        pdfencryptor.SetUserPassword(password);
    } else {
        showHelp();
    }

    const int retcode = pdfencryptor.Encrypt();

    if (retcode) {
        printf("Error [%d]: %s", pdfencryptor.GetLastErrorCode(), pdfencryptor.GetLastError());
        return 0;
    } else {
        printf("The document was encrypted successfully.\n");
    };

    fprintf(stderr, "\npress <return> to continue...\n");
    getchar();
    return 0;
}


