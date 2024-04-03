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
#include <string.h>
#include <stdlib.h>
#include "../../include/secureblackbox.h"

class MyHTTPServer : public HTTPServer
{
public:

    int FireGetRequest(HTTPServerGetRequestEventParams* e) override {
        wprintf(L"Request received (page: %s)\r\n", e->URI);

        this->SetResponseStatus(e->ConnectionID, 200);
        this->SetResponseString(e->ConnectionID,
                                "<html><head><title>SecureBlackbox HTTP server</title></head><body><h1>Hello there!</h1></body></html>",
                                "text/html");

        e->Handled = true;

        return 0;
    }
};

int main(int argc, char* argv[]) {
    if (argc < 2) {
        wprintf(L"Usage: httpserver <listening-port>\r\n\r\n");
        wprintf(L"Example: httpserver 80\r\n\r\n");
        wprintf(L"Press enter to continue...\r\n");
        getchar();

        return 1;
    }

    const int port = atoi(argv[1]);

    MyHTTPServer httpserver;

    httpserver.SetPort(port);

    const int res = httpserver.Start();
    if (res == 0) {
        wprintf(L"HTTP server started on port %d. Press enter to stop and exit.\r\n", port);
        getchar();

        httpserver.Stop();

        wprintf(L"Server stopped. Bye.\r\n");
        return 0;
    } else {
        wprintf(L"Failed to run the server, error %d\r\n", res);

        return 2;
    }
}


