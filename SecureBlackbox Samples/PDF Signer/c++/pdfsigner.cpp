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

PDFSigner pdfsigner;
CertificateManager certificatemanager;

int showHelp() {
    printf("Usage:\n"
        "\t./pdfsigner\t-inputFile <inputFile> -outputFile <outputFile>\n"
        "\t\t\t-signingKey <path> -keyPassword <password> [-option ]?\n"
    );
    printf("Options: \n");
    printf("\t-inputFile        (string) The PDF file to be signed.\n");
    printf("\t-outputFile       (string) Where to save the signed document.\n");
    printf("\t-signingKey       (string) The key file to be imported.\n");
    printf("\t-keyPassword      (string) The key password.\n");
    printf("\t-sigLevel         (int) The PAdES signature level.\n");
    printf("\t-sigAuthorName    (string) The name of the signer who produced this signature.\n");
    printf(
        "\t-sigReason        (int) Specifies the reason of the signing, for example to confirm the document correctness.\n");
    printf("\t-sigSignatureName (string) Specifies the signature identifier in the PDF-file.\n");
    printf("\t-autoCollectRevocationInfo      (switch) Whether revocation info should be collected automatically\n");
    exit(1);
}

int main(int argc, char** argv) {
    if (argc < 9) {
        fprintf(stderr, "Error: Missing arguments.\n");
        showHelp();
    }

    optext(argc, argv, "-inputFile") ? pdfsigner.SetInputFile(optval(argc, argv, "-inputFile")) : showHelp();
    optext(argc, argv, "-outputFile") ? pdfsigner.SetOutputFile(optval(argc, argv, "-outputFile")) : showHelp();
    optext(argc, argv, "-sigLevel")
        ? pdfsigner.SetNewSigLevel(atoi(optval(argc, argv, "-sigLevel")))
        : pdfsigner.SetNewSigLevel(PSL_LEGACY);
    optext(argc, argv, "-sigAuthorName")
        ? pdfsigner.SetNewSigAuthorName(optval(argc, argv, "-sigAuthorName"))
        : pdfsigner.SetNewSigAuthorName("");
    optext(argc, argv, "-sigReason")
        ? pdfsigner.SetNewSigReason(optval(argc, argv, "-sigReason"))
        : pdfsigner.SetNewSigReason("");
    optext(argc, argv, "-sigSignatureName")
        ? pdfsigner.SetNewSigSignatureName(optval(argc, argv, "-sigSignatureName"))
        : pdfsigner.SetNewSigSignatureName("");
    optext(argc, argv, "-autoCollectRevocationInfo")
        ? pdfsigner.Config("AutoCollectRevocationInfo=true")
        : pdfsigner.Config("AutoCollectRevocationInfo=false");

    if (optext(argc, argv, "-signingKey") && optext(argc, argv, "-keyPassword")) {
        char* signingKeyPath = optval(argc, argv, "-signingKey");
        char* signingKeyPassword = optval(argc, argv, "-keyPassword");
        certificatemanager.ImportFromFile(signingKeyPath, signingKeyPassword);
        pdfsigner.SetSigningChainCount(1);
        pdfsigner.SetSigningCertHandle(certificatemanager.GetCertHandle());
        if (certificatemanager.GetCertKeyAlgorithm() == "id-dsa") {
            pdfsigner.SetNewSigHashAlgorithm("SHA1");
        }
    } else {
        showHelp();
    }

    const int retcode = pdfsigner.Sign();

    if (retcode) {
        printf("Error [%d]: %s", pdfsigner.GetLastErrorCode(), pdfsigner.GetLastError());
        return 0;
    } else {
        printf("The document was signed successfully.\n");
    };

    fprintf(stderr, "\npress <return> to continue...\n");
    getchar();
    return 0;
}


