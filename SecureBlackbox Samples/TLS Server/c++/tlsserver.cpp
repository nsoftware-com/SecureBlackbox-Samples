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
#include <ctype.h>
#include <fstream>
#include <stdlib.h>
#include <cstring>
#include "../../include/secureblackbox.h"

#define LINE_LEN 80

class MyTLSServer : public TLSServer {

    int FireTLSShutdown(TLSServerTLSShutdownEventParams* e) override {
        printf(" [%lld] Secure session closed\n", e->ConnectionID);
        return 0;
    };

    int FireTLSEstablished(TLSServerTLSEstablishedEventParams* e) override {
        printf(" [%lld] Secure session established\n", e->ConnectionID);
        return 0;
    }

    int FireError(TLSServerErrorEventParams* e) override {
        printf(" [%lld] Error %i: %s\n", e->ConnectionID, e->ErrorCode, e->Description);
        return 0;
    }

    int FireData(TLSServerDataEventParams* e) override {
        const int len = e->lenBuffer;
        char* bufstr = new char[len + 1];
        strncpy(bufstr, e->Buffer, len); // get string from buffer
        bufstr[len] = '\0';
        printf("[%lld] C->S: %s\n", e->ConnectionID, bufstr);

        char* rev = new char[len + 1];
        strcpy(rev, bufstr);
        for (int i = 0, j = len - 1; i < j; i++, j--) {
            std::swap(rev[i], rev[j]);
        }
        SendText(e->ConnectionID, rev);
        printf("[%lld] S->C: %s\n", e->ConnectionID, rev);
        delete[] rev;
        delete[] bufstr;
        return 0;
    }

    int FireAccept(TLSServerAcceptEventParams* e) override {
        e->Accept = true;
        printf("Accepted a new client from %s:%i\n", e->RemoteAddress, e->RemotePort);
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
		"  tlsserver -- SecureBlackbox TLSServer Demo Application\n\n"
		"SYNOPSIS\n"
		"  tlsserver <listening_port> [-cert certificate_file] [-certpass certificate_password] [-tlsmode tlsmode]\n\n"
		"DESCRIPTION\n"
		"  TLSServer demonstrates the usage of TLSServer from SecureBlackbox.\n"
		"  The options are as follows:\n\n"
		"  -cert           The certificate used in ftp server.\n\n"
		"  -certpass       The password for the certificate.\n\n"
		"  -tlsmode        The TLS mode. Enter the corresponding string. Valid values: none, implicit.\n\n"
		"EXAMPLES\n"
		"  tlsserver 80 \n\n"
		"  tlsserver 8080 -cert C:\\certs\\mycert.pfx -certpass mypassword -tlsmode implicit \n\n"
	);
}

int main(int argc, char* argv[]) {
	MyTLSServer server;
	CertificateManager cm;
	char command[LINE_LEN]; // user's command
	char msg[10000]; // storage for mail message
	
	// Validate input
	if (argc < 2) {
		displayHelp();
		goto done;
	}

	int port = atoi(argv[1]);

	server.SetPortA(port);

	char* cert = optval(argc, argv, "-cert");
	char* certpass = optval(argc, argv, "-certpass");

	if (strcmp(cert, "")) {
		cm.ImportFromFile(cert, certpass);
		server.SetTLSServerCertCount(1);
		server.SetTLSServerCertHandle(0, cm.GetCertHandle());
	}

	// Additional options
	if (optext(argc, argv, "-tlsmode")) {
		char* tlsmode = optval(argc, argv, "-tlsmode");
		if (!strcmp(tlsmode, "none")) {
			server.SetTLSTLSMode(SM_NO_TLS);
		}
		else if (!strcmp(tlsmode, "implicit")) {
			server.SetTLSTLSMode(SM_IMPLICIT_TLS);
		}
	}

	if (server.Start() == 0) {
		printf("FTP server started on port %d. Press enter to stop server.\n", port);
		getchar();

		server.Stop();

		printf("Server stopped. \n\n");
	}
	else {
		printf("Failed to run the server.\n");
		printf("Error: [%i] %s\n\n", server.GetLastErrorCode(), server.GetLastError());
	}

done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


