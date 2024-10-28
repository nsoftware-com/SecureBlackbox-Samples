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

class MyHTTPServer : public HTTPServer
{
public:

    int FireGetRequest(HTTPServerGetRequestEventParams* e) override {
        printf("Request received (page: %s)\r\n", e->URI);

        this->SetResponseStatus(e->ConnectionID, 200);
        this->SetResponseString(e->ConnectionID,
                                "<html><head><title>SecureBlackbox HTTP server</title></head><body><h1>Hello there!</h1></body></html>",
                                "text/html", "");

        e->Handled = true;

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
}

static bool optext(int argc, char** argv, const char* option) {
	for (int x = 0; x < argc; x++) {
		if (!strcmp(argv[x], option)) {
			return true;
		}
	}
	return false;
}

using namespace ArgParser;

void displayHelp() {
	printf(
		"NAME\n"
		"  httpserver -- SecureBlackbox HTTPServer Demo Application\n\n"
		"SYNOPSIS\n"
		"  httpserver <listening_port> [-cert certificate_file] [-certpass certificate_password]\n\n"
		"DESCRIPTION\n"
		"  HTTPServer demonstrates the usage of HTTPServer from SecureBlackbox.\n"
		"  The options are as follows:\n\n"
		"  -cert           The certificate used in server.\n\n"
		"  -certpass       The password for the certificate.\n\n"
		"EXAMPLES\n"
		"  httpserver 80 \n\n"
		"  httpserver 8080 -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
	);
}

int main(int argc, char* argv[]) {
    MyHTTPServer httpserver;
	CertificateManager cm;

	// Validate input
	if (argc < 2) {
		displayHelp();
		goto done;
	}

    const int port = atoi(argv[1]);
    httpserver.SetPort(port);

	char* cert = optval(argc, argv, "-cert");
	char* certpass = optval(argc, argv, "-certpass");

	if (strcmp(cert, "")) {
		cm.ImportFromFile(cert, certpass);
		httpserver.SetTLSServerCertCount(1);
		httpserver.SetTLSServerCertHandle(0, cm.GetCertHandle());
	}

    if (httpserver.Start() == 0) {
        printf("HTTP server started on port %d. Press enter to stop server.\r\n", port);
        getchar();

        httpserver.Stop();

        printf("Server stopped. \n\n");
    } else {
        printf("Failed to run the server\n");
		printf("Error: [%i] %s\n\n", httpserver.GetLastErrorCode(), httpserver.GetLastError());
		goto done;
    }

done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


