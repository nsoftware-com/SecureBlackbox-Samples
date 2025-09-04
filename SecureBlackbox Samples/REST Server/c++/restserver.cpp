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
        "  restserver -- SecureBlackbox RESTServer Demo Application\n\n"
        "SYNOPSIS\n"
        "  restserver <listening_port> [-tlsmode tlsmode] [-cert certificate_file] [-certpass certificate_password] \n\n"
        "DESCRIPTION\n"
        "  RESTServer demonstrates the usage of RESTServer from SecureBlackbox.\n"
        "  The options are as follows:\n\n"
        "  -tlsmode        The TLS mode. Enter the corresponding string. Valid values: none, explicit, implicit.\n\n"
        "  -cert           The certificate used in server.\n\n"
        "  -certpass       The password for the certificate.\n\n"
        "EXAMPLES\n"
        "  restserver 80 \n\n"
        "  restserver 8080 -tlsmode implicit -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

class MyRESTServer : public RESTServer {
public:

    int FireAccept(RESTServerAcceptEventParams* e) override {
        e->Accept = true;
        printf("Client connected from %s:%d\r\n", e->RemoteAddress, e->RemotePort);

        return 0;
    }

    int FireGetRequest(RESTServerGetRequestEventParams* e) override {
        printf("Get request from %n\r\n", e->ConnectionID);

        return 0;
    }

    int FirePostRequest(RESTServerPostRequestEventParams* e) override {
        printf("Post request from %n\r\n", e->ConnectionID);

        return 0;
    }

    int FireDeleteRequest(RESTServerDeleteRequestEventParams* e) override {
        printf("Delete request from %n\r\n", e->ConnectionID);

        return 0;
    }
};

int main(int argc, char** argv) {
    MyRESTServer server;
    CertificateManager cm;

    // Validate input
    if (argc < 2) {
        displayHelp("");
		goto done;
    }

    const int port = atoi(argv[1]);

    server.SetPort(port);

    char* cert = optval(argc, argv, "-cert");
    char* certpass = optval(argc, argv, "-certpass");

    // Additional options
	server.SetTLSTLSMode(SM_NO_TLS);
	if (optext(argc, argv, "-tlsmode")) {
		char* tlsmode = optval(argc, argv, "-tlsmode");
		if (!strcmp(tlsmode, "none")) {
			server.SetTLSTLSMode(SM_NO_TLS);
		}
		else if (!strcmp(tlsmode, "explicit")) {
			server.SetTLSTLSMode(SM_EXPLICIT_TLS);
		}
		else if (!strcmp(tlsmode, "implicit")) {
			server.SetTLSTLSMode(SM_IMPLICIT_TLS);
		}
	}

    if (strcmp(cert, "")) {
        cm.ImportFromFile(cert, certpass);
        server.SetTLSServerCertCount(1);
        server.SetTLSServerCertHandle(0, cm.GetCertHandle());
    } else {
		if (server.GetTLSTLSMode() != SM_NO_TLS) {
			displayHelp("The server cannot support TLS without a valid server certificate. Please provide a certificate file via the cert and certpass parameters.");
			goto done;
		}
	}

    if (server.Start() == 0) {
        printf("REST server started on port %d. Press enter to stop server.\n", port);
        getchar();

        server.Stop();

        printf("Server stopped.\n\n");

		goto done;
    } else {
		goto err;
    }
err:
	if (server.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", server.GetLastErrorCode(), server.GetLastError());
	}
done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


