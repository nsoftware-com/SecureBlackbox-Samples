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
        "  jwencryption -- SecureBlackbox SymmetricCrypto Demo Application\n\n"
        "SYNOPSIS\n"
        "  jwencryption -e/-d [-input input_data] [-pass encryption_password] [-compact] [-encalg encryption_algorithm]\n\n"
        "DESCRIPTION\n"
        "  This sample illustrates how to encrypt text to a JW token with a password.\n"
        "  Used to encrypt and decrypt data.\n\n"
        "  The options are as follows:\n\n"
        "  -e            Whether to encrypt input data. \n\n"
        "  -d            Whether to decrypt input data. \n\n"
        "  -input        An input data to encrypt/decrypt (Required). \n\n"
        "  -pass         Password for encryption (Required). \n\n"
        "  -compact      Whether to use compact format \n\n"
        "  -encalg       The encryption algorithm to use. Valid values: RC2, RC4, DES, 3DES, AES128, AES192, AES256, Blowfish \n\n"
        "EXAMPLES\n"
        "	jwencryption -e -input \"And now that you don’t have to be perfect, you can be good.\" -pass mypassword -encalg AES256 \n\n"
        "	jwencryption -d -input eyJhbGciOiJkaXIiLCJlbmMiOiJBMjU2Q0JDLUhTNTEyIn0..kuN2U -pass mypassword -compact \n\n"
    );
}

int main(int argc, char** argv) {
    SymmetricCrypto crypto;
    CryptoKeyManager ckm;
    char* output;
    bool encrypt = false;
    bool decrypt = false;
    bool compact = false;

    // Validate input
    if (argc < 6) {
        displayHelp();
        goto done;
    }

    if (optext(argc, argv, "-e")) {
        encrypt = true;
    }

    if (optext(argc, argv, "-d")) {
        decrypt = true;
    }

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

    char* pass = optval(argc, argv, "-pass");
    if (!strcmp(pass, "")) {
        printf("-pass is required.\n");
        displayHelp();
        goto done;
    }

    if (optext(argc, argv, "-compact")) {
        compact = true;
    }

    char* encalg = optval(argc, argv, "-encalg");
    if (strcmp(encalg, "")) {
        crypto.SetEncryptionAlgorithm(encalg);
    }

    // PasswordToKey
    ckm.DeriveKey(256, pass, "");

    char* iv = new char[16];
    ckm.SetKeyIV(iv, 16);

    if (crypto.SetKeyHandle(ckm.GetKeyHandle())) {
        goto done;
    }

    // Encrypt 
    if (encrypt) {
        if (compact) {
            crypto.SetOutputEncoding(3); //cetCompact
        } else {
            crypto.SetOutputEncoding(4); //cetJSON
        }

        output = crypto.Encrypt(input, strlen(input));
        char* res = new char[1000];
        strcpy(res, "Encrypted token: ");
        strcat(res, output);
        printf(res);
        delete[] res;
    } else {
        // Decrypt 
        if (compact) {
            crypto.SetInputEncoding(3); //cetCompact
        } else {
            crypto.SetInputEncoding(4); //cetJSON
        }

        output = crypto.Decrypt(input, strlen(input));
        if (!output) {
            goto done;
        }
        char* res = new char[1000];
        strcpy(res, "Decrypted string: ");
        strcat(res, output);
        printf(res);
        delete[] res;
    }

done:
    if (crypto.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", crypto.GetLastErrorCode(), crypto.GetLastError());
    }
    getchar();
}


