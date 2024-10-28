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

void displayHelp(const char* errMes) {
    printf(
        "NAME\n"
        "  pgpreader -- SecureBlackbox PGPReader Demo Application\n\n"
        "SYNOPSIS\n"
		"  pgpreader <-input input_file> <-output output_file> <-pubkey public_key_file> <-seckey secret_key_file>\n"
		"            <-keypass keys_password> <-pass encryption_password> \n\n"
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

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

const char* translateSigValidity(int type) {
    switch (type) {
    case SVT_VALID: return "Valid";
        break;
	case SVT_FAILURE: return "Failure";
		break;
    case SVT_CORRUPTED: return "Corrupted";
        break;
    case SVT_REFERENCE_CORRUPTED: return "Reference Corrupted";
        break;
    case SVT_SIGNER_NOT_FOUND: return "Signing key not found, unable to verify";
        break;
    default: return "Unknown";
        break;
    }
}

int main(int argc, char** argv) {
    MyReader reader;
    PGPKeyring keyring;

    // Validate input
    if (argc < 2) {
        displayHelp("");
        goto done;
    }

    char* input = optval(argc, argv, "-input");
    if (!strcmp(input, "")) {
        displayHelp("-input is required.");
        goto done;
    }

    char* output = optval(argc, argv, "-output");
    if (!strcmp(output, "")) {
        displayHelp("-output is required.");
        goto done;
    }

    char* pubkey = optval(argc, argv, "-pubkey");
    if (!strcmp(pubkey, "")) {
        displayHelp("-pubkey is required.");
        goto done;
    }

    char* seckey = optval(argc, argv, "-seckey");
    if (!strcmp(seckey, "")) {
        displayHelp("-seckey is required.");
        goto done;
    }

    char* keypass = optval(argc, argv, "-keypass");
    if (!strcmp(keypass, "")) {
        displayHelp("-keypass is required.");
        goto done;
    }

    char* pass = optval(argc, argv, "-pass");
    if (!strcmp(pass, "")) {
        displayHelp("-pass is required.");
        goto done;
    }

    // Load keys
    if (keyring.ImportFromFile(pubkey)) {
		printf("Error: [%i] %s\n\n", keyring.GetLastErrorCode(), keyring.GetLastError());
        goto done;
    }

	if (keyring.ImportFromFile(seckey)) {
		printf("Error: [%i] %s\n\n", keyring.GetLastErrorCode(), keyring.GetLastError());
		goto done;
	}

    // Required options
    reader.SetVerifyingKeyCount(keyring.GetKeyCount());
    reader.SetDecryptingKeyCount(0);
	for (int y = 0; y < keyring.GetKeyCount(); y++) {
		reader.SetVerifyingKeyHandle(y, keyring.GetKeyHandle(y));
		if (keyring.GetKeyIsSecret(y))
		{
			reader.SetDecryptingKeyCount(reader.GetDecryptingKeyCount() + 1);
			reader.SetDecryptingKeyHandle(reader.GetDecryptingKeyCount() - 1, keyring.GetKeyHandle(y));
		}
	}

    reader.SetPassphrase(pass);

    reader.keypass = keypass;

    // Decrypt & Verify
    reader.SetInputFile(input);
    reader.SetOutputFile(output);
    if (reader.DecryptAndVerify()) {
        goto err;
    }

    printf("Signatures:\n");


    for (int x = 0; x < reader.GetSignatureCount(); x++) {
        char* username = "No name";
        for (int y = 0; y < keyring.GetKeyCount(); y++) {
            if (keyring.GetKeyIsPublic(y) && !keyring.GetKeyIsSubkey(y) && !strcmp(keyring.GetKeyKeyID(y),
                                                            reader.GetSignatureSignerKeyID(x))) {
                username = keyring.GetKeyUsername(y);
                break;
            }
        }
        printf("	%s - %s \n",
               username, translateSigValidity(reader.GetSignatureValidity(x)));
    }

    printf("The file were decrypted successfully.\n\n");

err:
    if (reader.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", reader.GetLastErrorCode(), reader.GetLastError());
    }
done:
	printf("Press Enter to exit the demo.\n");
    getchar();
}


