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
        "  symmetriccrypto -- SecureBlackbox SymmetricCrypto Demo Application\n\n"
        "SYNOPSIS\n"
        "  symmetriccrypto -e/-d [-input input_file] [-output output_file] [-pass encryption_password] [-encoding encoding]\n\n"
        "DESCRIPTION\n"
        "  SymmetricCrypto demonstrates the usage of SymmetricCrypto from SecureBlackbox.\n"
        "  Used to sign and verify files.\n\n"
        "  The options are as follows:\n\n"
        "  -e            Encrypt input file and save to output \n\n"
        "  -d            Decrypt input file and save to output \n\n"
        "  -input        An input file to encrypt or decrypt (Required). \n\n"
        "  -output       Where the encrypted or decrypted file will be saved (Required). \n\n"
        "  -pass         The password for encryption/decryption (Required).\n\n"
        "  -encoding     The encoding of hash. Valid values:\n\n"
        "                  0 - CET_DEFAULT\n"
        "                  1 - CET_BINARY\n"
        "                  2 - CET_BASE_64\n"
        "                  3 - CET_COMPACT\n"
        "                  4 - CET_JSON\n\n"
        "EXAMPLES\n"
        "  symmetriccrypto -e -input C:\\cypto\\helloworld.txt -output C:\\cypto\\helloworld.enc -pass mypassword \n\n"
        "  symmetriccrypto -d -input C:\\cypto\\helloworld.enc -output C:\\cypto\\helloworld.txt -pass mypassword -encalg 3DES -encoding 2 \n\n"
    );
}

int main(int argc, char** argv) {
    SymmetricCrypto crypto;
    CryptoKeyManager ckm;
    char* signature;
    bool encrypt = false;
    bool decrypt = false;

    // Validate input
    if (argc < 8) {
        displayHelp();
        goto done;
    }

    encrypt = optext(argc, argv, "-e");
    decrypt = optext(argc, argv, "-d");

    if (!(encrypt || decrypt)) {
        printf("-e or -d is required.\n");
        displayHelp();
        goto done;
    }

    if (encrypt && decrypt) {
        printf("Use only one -e or -d parameter.\n");
        displayHelp();
        goto done;
    }

    char* input = optval(argc, argv, "-input");
    if (!strcmp(input, "")) {
        printf("-input is required.\n");
        displayHelp();
        goto done;
    }

    char* output = optval(argc, argv, "-output");
    if (!strcmp(output, "")) {
        printf("-output is required.\n");
        displayHelp();
        goto done;
    }

    char* pass = optval(argc, argv, "-pass");
    if (!strcmp(pass, "")) {
        printf("-pass is required.\n");
        displayHelp();
        goto done;
    }

    crypto.SetEncryptionAlgorithm("AES256");

    // PasswordToKey
    int keybits = 256;
    if (crypto.GetInputEncoding() == CET_COMPACT || crypto.GetInputEncoding() == CET_JSON || crypto.GetOutputEncoding() ==
        CET_COMPACT || crypto.GetOutputEncoding() == CET_JSON) {
        keybits = 512;
    }

    ckm.DeriveKey(keybits, pass, "");

    char* iv = new char[16];
    ckm.SetKeyIV(iv, 16);

    if (crypto.SetKeyHandle(ckm.GetKeyHandle())) {
        goto done;
    }

    // Encrypt 
    if (encrypt) {
        if (optext(argc, argv, "-encoding")) {
            crypto.SetOutputEncoding(atoi(optval(argc, argv, "-encoding")));
        }

        if (crypto.EncryptFile(input, output)) {
            goto done;
        }
        printf("The file was encrypted successfully.\n");
    } else {
        // Decrypt 
        if (optext(argc, argv, "-encoding")) {
            crypto.SetInputEncoding(atoi(optval(argc, argv, "-encoding")));
        }

        if (crypto.DecryptFile(input, output)) {
            goto done;
        }
        printf("The file was decrypted successfully.\n");
    }

done:
    if (crypto.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", crypto.GetLastErrorCode(), crypto.GetLastError());
    }
    getchar();
}


