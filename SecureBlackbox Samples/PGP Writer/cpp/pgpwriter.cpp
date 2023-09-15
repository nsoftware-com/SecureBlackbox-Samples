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

class MyWriter : public PGPWriter {
public:

    char* keypass = "";

    int FireKeyPassphraseNeeded(PGPWriterKeyPassphraseNeededEventParams* e) override {
        e->Passphrase = keypass;
        return 0;
    }
};

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
        "  pgpwriter -- SecureBlackbox PGPWriter Demo Application\n\n"
        "SYNOPSIS\n"
        "  pgpwriter [-input input_file] [-output output_file] [-pubkey public_key_file] [-seckey secret_key_file]\n"
        "            [-keypass keys_password] [-pass encryption_password] \n\n"
        "DESCRIPTION\n"
        "  PGPWriter demonstrates the usage of PGPWriter from SecureBlackbox.\n"
        "  Used to create encrypted and signed OpenPGP-compliant files. \n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the ASiC will be saved (Required).\n\n"
        "  -pubkey       The public key used to encrypt file (Required).\n\n"
        "  -seckey       The secret (private) key used to sign file (Required).\n\n"
        "  -keypass      The password for the keys (Required).\n\n"
        "  -pass         The password for encryption (Required).\n\n"
    );
}

int main(int argc, char** argv) {
    MyWriter writer;
    PGPKeyring keyring;

    // Validate input
    if (argc < 13) {
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

    char* pubkey = optval(argc, argv, "-pubkey");
    if (!strcmp(pubkey, "")) {
        printf("-pubkey is required.");
        displayHelp();
        goto done;
    }

    char* seckey = optval(argc, argv, "-seckey");
    if (!strcmp(seckey, "")) {
        printf("-seckey is required.");
        displayHelp();
        goto done;
    }

    char* keypass = optval(argc, argv, "-keypass");
    if (!strcmp(keypass, "")) {
        printf("-keypass is required.");
        displayHelp();
        goto done;
    }

    char* pass = optval(argc, argv, "-pass");
    if (!strcmp(pass, "")) {
        printf("-pass is required.");
        displayHelp();
        goto done;
    }

    // Load keys
    if (keyring.Load(pubkey, seckey)) {
        goto done;
    }

    // Required options
    writer.SetEncryptingKeyCount(1);
    writer.SetEncryptingKeyHandle(0, keyring.GetPublicKeyHandle(0));
    writer.SetSigningKeyCount(1);
    writer.SetSigningKeyHandle(0, keyring.GetSecretKeyHandle(0));

    writer.SetPassphrase(pass);

    writer.keypass = keypass;

    // Encrypt & Sign
    writer.SetInputFile(input);
    writer.SetOutputFile(output);
    if (writer.EncryptAndSign()) {
        goto done;
    }
    printf("The file were encrypted and signed successfully.\n");

done:
    if (writer.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", writer.GetLastErrorCode(), writer.GetLastError());
    }
    getchar();
}


