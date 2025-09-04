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

void displayHelp(const char* errMes) {
    printf(
        "NAME\n"
        "  ftpserver -- SecureBlackbox FTPServer Demo Application\n\n"
        "SYNOPSIS\n"
        "  ftpserver <listening_port> [-users users_file] [-userspass users_password] [-cert certificate_file]\n"
        "              [-certpass certificate_password] [-tlsmode tlsmode] [-allowanon]\n\n"
        "DESCRIPTION\n"
        "  FTPServer demonstrates the usage of FTPServer from SecureBlackbox.\n"
        "  The options are as follows:\n\n"
        "  -users          An file with users information.\n\n"
        "  -userspass      The password for users file.\n\n"
        "  -cert           The certificate used in ftp server.\n\n"
        "  -certpass       The password for the certificate.\n\n"
		"  -tlsmode        The TLS mode. Enter the corresponding string. Valid values: none, explicit, implicit.\n\n"
        "  -allowanon      Whether to allow connection from anonymous user.\n\n"
        "EXAMPLES\n"
        "  ftpserver 80 \n\n"
        "  ftpserver 8080 -users C:\\ftpserver\\users.dat -cert C:\\certs\\mycert.pfx -certpass mypassword \n"
        "             -tlsmode implicit -allowanon \n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

class MyFTPServer : public FTPServer
{
public:

    int FireConnect(FTPServerConnectEventParams* e) override {
        printf("Client connected from %s:%d\r\n", e->RemoteAddress, e->Port);

        return 0;
    }

    int FireAuthAttempt(FTPServerAuthAttemptEventParams* e) override {
        if (e->Allow) {
            printf("Access granted to user: %s\r\n", e->Username);
        } else {
            printf("Access denied for user: %s\r\n", e->Username);
        }

        return 0;
    }
};

int main(int argc, char** argv) {
    MyFTPServer server;
    CertificateManager cm;
    UserManager um;

    // Validate input
    if (argc < 2) {
        displayHelp("");
		goto done;
    }

    int port = atoi(argv[1]);

    server.SetPortA(port);

    char* users = optval(argc, argv, "-users");
    if (strcmp(users, "")) {
        char* userspass = optval(argc, argv, "-userspass");

        um.ImportFromFile(users, userspass, true);

        server.SetUserCount(um.GetUserCount());
        for (int x = 0; x < um.GetUserCount(); x++) {
            server.SetUserHandle(x, um.GetUserHandle(x));
        }
    }

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

    char* cert = optval(argc, argv, "-cert");
    char* certpass = optval(argc, argv, "-certpass");

    if (strcmp(cert, "")) {
        cm.ImportFromFile(cert, certpass);
        server.SetTLSServerCertCount(1);
        server.SetTLSServerCertHandle(0, cm.GetCertHandle());
	}
	else {
		if (server.GetTLSTLSMode() != SM_NO_TLS) {
			displayHelp("The server cannot support TLS without a valid server certificate. Please provide a certificate file via the cert and certpass parameters.");
			goto done;
		}
	}

    if (optext(argc, argv, "-allowanon")) {
        server.SetAllowAnonymous(true);
    }

    if (server.Start() == 0) {
        printf("FTP server started on port %d. Press enter to stop server.\n", port);
        getchar();

        server.Stop();

        printf("Server stopped. \n\n");
    } else {
        printf("Failed to run the server.\n");
		printf("Error: [%i] %s\n", server.GetLastErrorCode(), server.GetLastError());
		goto done;
    }

done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


