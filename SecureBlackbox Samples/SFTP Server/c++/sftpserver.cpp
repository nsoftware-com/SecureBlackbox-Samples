/*
 * SecureBlackbox 2022 C++ Edition - Sample Project
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

void displayHelp() {
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
        "  sftpserver 2222 -users C:\\sftpserver\\users.dat -cert C:\\certs\\mycert.pfx -certpass mypassword -basedir D:\\temp \n\n"
    );
}

class MySFTPServer : public SFTPServer {
public:

    int FireConnect(SFTPServerConnectEventParams* e) override {
        wprintf(L"Client connected from %s:%d)\r\n", e->RemoteAddress, e->RemotePort);

        return 0;
    }

    int FireAuthAttempt(SFTPServerAuthAttemptEventParams* e) override {
        if (e->Accept) wprintf(L"Access granted to user %s)\r\n", e->Username);
        else wprintf(L"Access denied for user %s)\r\n", e->Username);

        return 0;
    }
};

int main(int argc, char** argv) {
    MySFTPServer server;
    SSHKeyManager km;
    UserManager um;

    // Validate input
    if (argc < 2) {
        displayHelp();
        getchar();
        return 1;
    }

    const int port = atoi(argv[1]);

    // Load private key


    server.SetPortA(port);

    char* basedir = optval(argc, argv, "-basedir");
    if (strcmp(basedir, "")) {
        server.SetBaseDir(basedir);
    }

    char* users = optval(argc, argv, "-users");
    if (strcmp(users, "")) {
        char* userspass = optval(argc, argv, "-userspass");

        um.Load(users, userspass);

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

    const int res = server.Start();
    if (res == 0) {
        wprintf(L"SFTP server started on port %d. Press enter to stop and exit.\r\n", port);
        getchar();

        server.Stop();

        wprintf(L"Server stopped. Bye.\r\n");
        return 0;
    } else {
        wprintf(L"Failed to run the server, error %d\r\n", res);
        getchar();

        return 2;
    }
}


