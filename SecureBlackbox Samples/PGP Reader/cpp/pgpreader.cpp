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

class MyReader : public PGPReader {
public:
    char* keypass = "";

    int FireKeyPassphraseNeeded(PGPReaderKeyPassphraseNeededEventParams* e) override {
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
        "  pgpreader -- SecureBlackbox PGPReader Demo Application\n\n"
        "SYNOPSIS\n"
        "  pgpreader [-input input_file] [-output output_file] [-pubkey public_key_file] [-seckey secret_key_file]\n"
        "            [-keypass keys_password] [-pass encryption_password] \n\n"
        "DESCRIPTION\n"
        "  PGPReader demonstrates the usage of PGPReader from SecureBlackbox.\n"
        "  Used to decrypted and verify OpenPGP-compliant files. \n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to decrypt and verify (Required).\n\n"
        "  -output       Where the decrypted file will be saved (Required).\n\n"
        "  -pubkey       The public key used to verify file (Required).\n\n"
        "  -seckey       The secret (private) key used to decrypt file (Required).\n\n"
        "  -keypass      The password for the keys (Required).\n\n"
        "  -pass         The password for decryption (Required).\n\n"
    );
}

const char* translateSigValidity(int type) {
    switch (type) {
    case PSV_VALID: return "Valid";
        break;
    case PSV_CORRUPTED: return "Corrupted";
        break;
    case PSV_UNKNOWN_ALGORITHM: return "Unknown algorithm";
        break;
    case PSV_NO_KEY: return "No key";
        break;
    default: return "Unknown";
        break;
    }
}

int main(int argc, char** argv) {
    MyReader reader;
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
    reader.SetDecryptingKeyCount(1);
    reader.SetDecryptingKeyHandle(0, keyring.GetSecretKeyHandle(0));
    reader.SetVerifyingKeyCount(1);
    reader.SetVerifyingKeyHandle(0, keyring.GetPublicKeyHandle(0));

    reader.SetPassphrase(pass);

    reader.keypass = keypass;

    // Decrypt & Verify
    reader.SetInputFile(input);
    reader.SetOutputFile(output);
    if (reader.DecryptAndVerify()) {
        goto done;
    }

    printf("Signatures:\n");


    for (int x = 0; x < reader.GetSignatureCount(); x++) {
        char* username = "No name";
        for (int y = 0; y < keyring.GetPublicKeyCount(); y++) {
            if (!keyring.GetPublicKeyIsSubkey(y) && !strcmp(keyring.GetPublicKeyKeyID(y),
                                                            reader.GetSignatureSignerKeyID(x))) {
                username = keyring.GetPublicKeyUsername(y);
                break;
            }
        }
        printf("	%s - %s \n",
               username, translateSigValidity(reader.GetSignatureValidity(x)));
    }

    printf("The file were decrypted successfully.\n");

done:
    if (reader.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", reader.GetLastErrorCode(), reader.GetLastError());
    }
    getchar();
}


