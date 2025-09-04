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
#include "../../include/secureblackbox.h"
#include <cstring>

#define LINE_LEN 80

namespace ArgParser {
	static char* optval(int argc, char** argv, const char* option) {
		for (int x = 0; x < argc; x++) {
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

class MyDTLSServer : public DTLSServer {
    int FireTLSShutdown(DTLSServerTLSShutdownEventParams* e) override {
        printf(" [%lld] Secure session closed\n", e->ConnectionID);
        return 0;
    };

    int FireTLSEstablished(DTLSServerTLSEstablishedEventParams* e) override {
        printf(" [%lld] Secure session established\n", e->ConnectionID);
        return 0;
    }

    int FireError(DTLSServerErrorEventParams* e) override {
        printf(" [%lld] Error %i: %s\n", e->ConnectionID, e->ErrorCode, e->Description);
        return 0;
    }

    int FireData(DTLSServerDataEventParams* e) override {
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

    int FireAccept(DTLSServerAcceptEventParams* e) override {
        e->Accept = true;
        printf("Accepted a new client from %s:%i\n", e->RemoteAddress, e->RemotePort);
        return 0;
    }
};

void displayHelp(const char* errMes) {
	printf(
		"NAME\n"
		"  dtlsserver -- SecureBlackbox DTLSServer Demo Application\n\n"
		"SYNOPSIS\n"
		"  dtlsserver <listening_port> <-cert certificate_file> [-certpass certificate_password]\n\n"
		"DESCRIPTION\n"
		"  This sample is a very simple echo server that shows how to use the DTLSServer component\n"
		"  to receive messages from DTLS clients and send them back to the clients.\n\n"
		"  The options are as follows:\n\n"
        "  -cert        The certificate used in dtls server.(Required).\n\n"
        "  -certpass    The password for the certificate.\n\n"
		"EXAMPLES\n"
		"  dtlsserver 4433 -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
	);

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char* argv[]) {
	MyDTLSServer server;
	CertificateManager cm;

	// Validate input
	if (argc < 2) {
		displayHelp("");
		goto done;
	}

	char* cert = optval(argc, argv, "-cert");
	if (!strcmp(cert, "")) {
		displayHelp("-cert is required.");
		goto done;
	}

	char* certpass = optval(argc, argv, "-certpass");

	server.SetPort(atoi(argv[1]));

	if (!cm.ImportFromFile(cert, certpass)) {
		server.SetTLSServerCertCount(server.GetTLSServerCertCount() + 1);
		server.SetTLSServerCertHandle(server.GetTLSServerCertCount() - 1, cm.GetCertHandle());
		printf("Server certificate loaded\n");
	}
	else {
		printf("Failed to load certificate.\n");
		goto err;
	}

	if (server.Start()) {
		goto err;
	}
	printf("The server started successfully!\n");
	printf("Type \"stop\" to stop the server.\n\n");

	char command[LINE_LEN]; // user's command
	char msg[10000]; // storage for mail message

	while (1) {
		strcpy(msg, "");
		printf("> ");
		fgets(command, LINE_LEN, stdin);
		command[max(0, strlen(command) - 1)] = '\0';
		for (int i = 0; i < strlen(command); i++) {
			command[i] = tolower(command[i]);
		}

		if (!strcmp(command, "?")) {
			printf("  stop          stop server\n");
		}
		else if (!strcmp(command, "stop")) {
			if (server.GetActive()) {
				if (server.Stop() == 0) {
					printf("The server was stopped!\n\n");
					goto done;
				}
				else {
					goto err;
				}
			}
		}
		else {
			printf("Bad command!\n");
		}
	}

err:
	if (server.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", server.GetLastErrorCode(), server.GetLastError());
	}
done:
	if (server.GetActive()) {
		server.Stop();
	}
	printf("Press Enter to exit the demo.\n");
	getchar();
}




