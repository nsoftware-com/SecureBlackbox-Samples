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
		"  pdfencryptor -- SecureBlackbox PDFEncryptor Demo Application\n\n"
		"SYNOPSIS\n"
		"  pdfencryptor <-input input_file> <-output output_file> [-cert certificate_file] [-certpass certificate_password]\n"
		"       [-pass password] [-encalg encryption_algorithm] \n\n"
		"DESCRIPTION\n"
		"  This sample illustrates the use of PDFEncryptor component for encrypting PDF documents.\n\n"
		"  The options are as follows:\n\n"
		"  -input        An input PDF file to encrypt (Required).\n\n"
		"  -output       Where the encrypted file will be saved (Required).\n\n"
		"  -cert         The certificate used to encrypt file.\n\n"
		"  -certpass     The password for the encryption certificate.\n\n"
		"  -pass         The password used to encrypt file.\n\n"
		"  -encalg       The encryption algorithm. Valid values: 3DES, RC4, RC2, AES128, AES192, AES256, Twofish128 \n\n"
		"EXAMPLES\n"
		"  pdfencryptor -input C:\\helloworld.pdf -output C:\\enc.pdf -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n"
		"  pdfencryptor -input C:\\helloworld.pdf -output C:\\enc.pdf -pass mypassword -alg AES128 \n\n"
	);

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
	PDFEncryptor encryptor;
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
	char* certpass = optval(argc, argv, "-certpass");
	char* pass = optval(argc, argv, "-pass");
	if (strcmp(cert, "")) {
		cm.ImportFromFile(cert, certpass);
		encryptor.SetEncryptionCertificateHandle(cm.GetCertHandle());
	}
	else if (strcmp(pass, "")) {
		encryptor.SetUserPassword(pass);
	}
	else {
		displayHelp("-cert or -pass is required.");
		goto done;
	}

    encryptor.SetInputFile(input);
    encryptor.SetOutputFile(output);

    optext(argc, argv, "-encalg")
        ? encryptor.SetEncryptionAlgorithm(optval(argc, argv, "-encalg"))
        : encryptor.SetEncryptionAlgorithm("AES256");

    if (encryptor.Encrypt()) {
		goto err;
    } else {
        printf("The document was encrypted successfully.\n\n");
    };

err:
	if (encryptor.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", encryptor.GetLastErrorCode(), encryptor.GetLastError());
	}
done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


