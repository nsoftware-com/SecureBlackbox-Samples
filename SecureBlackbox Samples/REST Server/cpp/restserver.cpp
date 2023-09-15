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

void displayHelp() {
    printf(
        "NAME\n"
        "  restserver -- SecureBlackbox RESTServer Demo Application\n\n"
        "SYNOPSIS\n"
        "  restserver <listening_port> [-tls]  [-cert certificate_file] [-certpass certificate_password] \n\n"
        "DESCRIPTION\n"
        "  RESTServer demonstrates the usage of RESTServer from SecureBlackbox.\n"
        "  The options are as follows:\n\n"
        "  -tls            Whether to use tls.\n\n"
        "  -cert           The certificate used in ftp server.\n\n"
        "  -certpass       The password for the certificate.\n\n"
        "EXAMPLES\n"
        "  restserver 80 \n\n"
        "  restserver 8080 -tls -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n"
    );
}

class MyRESTServer : public RESTServer {
public:

    int FireAccept(RESTServerAcceptEventParams* e) override {
        e->Accept = true;
        wprintf(L"Client connected from %s:%d)\r\n", e->RemoteAddress, e->RemotePort);

        return 0;
    }

    int FireGetRequest(RESTServerGetRequestEventParams* e) override {
        wprintf(L"Get request from %n)\r\n", e->ConnectionID);

        return 0;
    }

    int FirePostRequest(RESTServerPostRequestEventParams* e) override {
        wprintf(L"Post request from %n)\r\n", e->ConnectionID);

        return 0;
    }

    int FireDeleteRequest(RESTServerDeleteRequestEventParams* e) override {
        wprintf(L"Delete request from %n)\r\n", e->ConnectionID);

        return 0;
    }
};

int main(int argc, char** argv) {
    MyRESTServer server;
    CertificateManager cm;

    // Validate input
    if (argc < 2) {
        displayHelp();
        getchar();
        return 1;
    }

    const int port = atoi(argv[1]);

    server.SetPort(port);

    char* cert = optval(argc, argv, "-cert");
    char* certpass = optval(argc, argv, "-certpass");

    // Additional options
    server.SetTLSTLSMode(optext(argc, argv, "-tls"));

    if (strcmp(cert, "")) {
        cm.ImportFromFile(cert, certpass);
        server.SetServerCertCount(1);
        server.SetServerCertHandle(0, cm.GetCertHandle());
    }

    const int res = server.Start();
    if (res == 0) {
        wprintf(L"REST server started on port %d. Press enter to stop and exit.\r\n", port);
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


