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

void displayHelp(const char* errMes) {
	printf(
		"NAME\n"
		"  pdfdecryptor -- SecureBlackbox PDFDecryptor Demo Application\n\n"
		"SYNOPSIS\n"
		"  pdfdecryptor <-input input_file> <-output output_file> [-cert certificate_file] [-certpass certificate_password] [-pass password]\n\n"
		"DESCRIPTION\n"
		"  This sample illustrates the use of PDFDecryptor component for decrypting PDF documents.\n\n"
		"  The options are as follows:\n\n"
        "  -input        An input file to decrypt (Required).\n\n"
        "  -output       Where the decrypted file will be saved (Required).\n\n"
		"  -cert         The certificate used for decryption.\n\n"
		"  -certpass     The password for the decrypting certificate.\n\n"
		"  -pass         The password used for decryption.\n\n"
		"EXAMPLES\n"
		"  pdfdecryptor -input C:\\myfile.pdf -output C:\\out.pdf -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
		"  pdfdecryptor -input C:\\myfile.pdf -output C:\\out.pdf -pass mypassword\n\n"
	);

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
	PDFDecryptor decryptor;
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
		decryptor.SetDecryptionCertificateHandle(cm.GetCertHandle());
	}
	else if(strcmp(pass, "")) {
		decryptor.SetPassword(pass);
	}
	else {
		displayHelp("-cert or -pass is required.");
		goto done;
	}

	decryptor.SetInputFile(input);
	decryptor.SetOutputFile(output);

    if (decryptor.Decrypt()) {
		goto err;
    } else {
        printf("The document was decrypted successfully.\n\n");
    };

err:
	if (decryptor.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", decryptor.GetLastErrorCode(), decryptor.GetLastError());
	}
done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


