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
};

using namespace ArgParser;

void displayHelp(const char* errMes) {
    printf(
        "NAME\n"
        "  sftpserver -- SecureBlackbox SFTPServer Demo Application\n\n"
        "SYNOPSIS\n"
        "  sftpserver <listening_port> [-users users_file] [-userspass users_password] [-key key_file] [-keypass key_password]\n"
        "             [-basedir base_directory]\n\n"
        "DESCRIPTION\n"
        "  SFTPServer demonstrates the usage of SFTPServer from SecureBlackbox.\n"
        "  The options are as follows:\n\n"
        "  -users          A file containing user login parameters.\n\n"
        "  -userspass      The password for the users file.\n\n"
        "  -key            A file containing the private host key.\n\n"
        "  -keypass        The password for the key file.\n\n"
        "  -basedir        The base directory of the server.\n\n"
        "EXAMPLES\n"
        "  sftpserver 22 \n\n"
        "  sftpserver 2222 -users C:\\sftpserver\\users.dat -key C:\\certs\\mykey.key -keypass mypassword -basedir D:\\temp \n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

class MySFTPServer : public SFTPServer {
public:

    int FireConnect(SFTPServerConnectEventParams* e) override {
        printf("Client connected from %s:%d\r\n", e->RemoteAddress, e->RemotePort);

        return 0;
    }

    int FireAuthAttempt(SFTPServerAuthAttemptEventParams* e) override {
        if (e->Accept) printf("Access granted to user %s\r\n", e->Username);
        else printf("Access denied for user %s\r\n", e->Username);

        return 0;
    }
};

int main(int argc, char** argv) {
    MySFTPServer server;
    SSHKeyManager km;
    UserManager um;

    // Validate input
    if (argc < 2) {
        displayHelp("");
		goto done;
    }

    const int port = atoi(argv[1]);
    server.SetPortA(port);

    char* basedir = optval(argc, argv, "-basedir");
    if (strcmp(basedir, "")) {
        server.SetBaseDir(basedir);
    }

    char* users = optval(argc, argv, "-users");
    if (strcmp(users, "")) {
        char* userspass = optval(argc, argv, "-userspass");

        um.ImportFromFile(users, userspass, true);

        server.SetUserCount(um.GetUserCount());
        for (int x = 0; x < um.GetUserCount(); x++) server.SetUserHandle(x, um.GetUserHandle(x));
    }

    char* key = optval(argc, argv, "-key");
    char* keypass = optval(argc, argv, "-keypass");

    if (strcmp(key, "")) {
        km.ImportFromFile(key, keypass);
        server.SetServerKeyCount(1);
        server.SetServerKeyHandle(0, km.GetKeyHandle());
    }

    if (server.Start() == 0) {
        printf("SFTP server started on port %d. Press enter to stop server.\n", port);
        getchar();

        server.Stop();

        printf("Server stopped.\n\n");
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


