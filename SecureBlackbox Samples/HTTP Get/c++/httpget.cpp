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
#include <stdlib.h>
#include "../../include/secureblackbox.h"
#define LINE_LEN 100

class MyHTTP : public HTTPClient
{
public:

    int FireTLSCertValidate(HTTPClientTLSCertValidateEventParams* e) override {
        if (e->Accept) {
            return 0;
        }
        printf("Server provided the following certificate:\nIssuer: %s\nSubject: %s\n",
               this->GetTLSServerCertIssuerRDN(0), this->GetTLSServerCertSubjectRDN(0));
        printf("Would you like to continue? [y/n] ");
        if (getchar() == 'y') {
            e->Accept = true;
        } else {
            exit(0);
        }
        return 0;
    }
};

void displayHelp() {
	printf(
		"NAME\n"
		"  httpget -- SecureBlackbox HTTPClient Demo Application\n\n"
		"SYNOPSIS\n"
		"  httpget <url>\n\n"
		"DESCRIPTION\n"
		"  This sample illustrates the ways of making GET requests with HTTPSClient.\n\n"
		"  The options are as follows:\n\n"
		"  url       The local host of server (Required).\n\n"
		"EXAMPLES\n"
		"  httpget https://www.google.com\n\n"
	);
}

int main(int argc, char** argv) {
	MyHTTP client;

	if (argc < 2) {
		displayHelp();
		goto done;

	}

	if (client.Get(argv[1])) // Got an error.  The user is done.
	{
		goto err;
	}

	printf("Server Response:\n\n%s\n\n", client.GetOutputString());

err:
	if (client.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", client.GetLastErrorCode(), client.GetLastError());
	}
done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


