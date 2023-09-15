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
        "  ftpserver -- SecureBlackbox FTPServer Demo Application\n\n"
        "SYNOPSIS\n"
        "  ftpserver <listening_port> [-users users_file] [-userspass users_password] [-cert certificate_file] [-certpass certificate_password]\n"
        "             [-implicitssl] [-allowanonymous]\n\n"
        "DESCRIPTION\n"
        "  FTPServer demonstrates the usage of FTPServer from SecureBlackbox.\n"
        "  The options are as follows:\n\n"
        "  -users          An file with users information.\n\n"
        "  -userspass      The password for users file.\n\n"
        "  -cert           The certificate used in ftp server.\n\n"
        "  -certpass       The password for the certificate.\n\n"
        "  -implicitssl    Whether to use implicit ssl.\n\n"
        "  -allowanonymous Whether to allow connection from anonymous user.\n\n"
        "EXAMPLES\n"
        "  ftpserver 80 \n\n"
        "  ftpserver 8080 -users C:\\ftpserver\\users.dat -cert C:\\certs\\mycert.pfx -certpass mypassword \\\n"
        "             -implicitssl -allowanonymous \n\n"
    );
}

class MyFTPServer : public FTPServer
{
public:

    int FireConnect(FTPServerConnectEventParams* e) override {
        wprintf(L"Client connected from %s:%d)\r\n", e->RemoteAddress, e->Port);

        return 0;
    }

    int FireAuthAttempt(FTPServerAuthAttemptEventParams* e) override {
        if (e->Allow) {
            wprintf(L"Access granted to user %s)\r\n", e->Username);
        } else {
            wprintf(L"Access denied for user %s)\r\n", e->Username);
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
        displayHelp();
        getchar();
        return 1;
    }

    int port = atoi(argv[1]);

    server.SetPortA(port);

    char* users = optval(argc, argv, "-users");
    if (strcmp(users, "")) {
        char* userspass = optval(argc, argv, "-userspass");

        um.Load(users, userspass);

        server.SetUserCount(um.GetUserCount());
        for (int x = 0; x < um.GetUserCount(); x++) {
            server.SetUserHandle(x, um.GetUserHandle(x));
        }
    }

    char* cert = optval(argc, argv, "-cert");
    char* certpass = optval(argc, argv, "-certpass");

    if (strcmp(cert, "")) {
        cm.ImportFromFile(cert, certpass);
        server.SetServerCertCount(1);
        server.SetServerCertHandle(0, cm.GetCertHandle());
    }

    // Additional options
    if (optext(argc, argv, "-implicitssl")) {
        server.SetImplicitSSL(true);
    }
    if (optext(argc, argv, "-allowanonymous")) {
        server.SetAllowAnonymous(true);
    }

    const int res = server.Start();
    if (res == 0) {
        wprintf(L"FTP server started on port %d. Press enter to stop and exit.\r\n", port);
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


