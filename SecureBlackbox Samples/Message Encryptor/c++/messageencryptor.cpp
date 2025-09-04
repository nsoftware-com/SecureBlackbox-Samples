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

void displayHelp(const char* errMes) {
    printf(
        "NAME\n"
        "  messageencryptor -- SecureBlackbox MessageEncryptor Demo Application\n\n"
        "SYNOPSIS\n"
		"  messageencryptor <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password] [-encalg encryption_algorithm] \n\n"
        "DESCRIPTION\n"
        "  MessageEncryptor demonstrates the usage of MessageEncryptor from SecureBlackbox.\n"
        "  Used to create encrypted ('enveloped') PKCS#7 messages. \n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to encrypt (Required).\n\n"
        "  -output       Where the encrypted file will be saved (Required).\n\n"
        "  -cert         The certificate used to encrypt file (Required).\n\n"
        "  -certpass     The password for the encryption certificate.\n\n"
        "  -encalg       The encryption algorithm. Valid values: 3DES, RC4, RC2, AES128, AES192, AES256, Twofish128 \n\n"
        "EXAMPLES\n"
        "  messageencryptor -input C:\\pkcs7\\helloworld.txt -output C:\\pkcs7\\enc.pkcs7 -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n"
        "  messageencryptor -input C:\\pkcs7\\helloworld.txt -output C:\\pkcs7\\enc.pkcs7 -cert C:\\certs\\mycert.pfx -certpass mypassword -alg AES128 \n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
    MessageEncryptor encryptor;
    CertificateManager cm;

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

    char* cert = optval(argc, argv, "-cert");
    if (!strcmp(cert, "")) {
        displayHelp("-cert is required.");
        goto done;
    }

    char* certpass = optval(argc, argv, "-certpass");

    // Additional options
    if (optext(argc, argv, "-encalg")) {
        encryptor.SetEncryptionAlgorithm(optval(argc, argv, "-encalg"));
    }

    // Required options
    encryptor.SetInputFile(input);
    encryptor.SetOutputFile(output);
    cm.ImportFromFile(cert, certpass);
	encryptor.SetEncryptionCertCount(1);
    encryptor.SetEncryptionCertHandle(0, cm.GetCertHandle());

    // Encrypt
    if (encryptor.Encrypt()) {
        goto err;
    }
    printf("The file successfully encrypted.\n\n");

err:
    if (encryptor.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", encryptor.GetLastErrorCode(), encryptor.GetLastError());
    }
done:
	printf("Press Enter to exit the demo.\n");
    getchar();
}


