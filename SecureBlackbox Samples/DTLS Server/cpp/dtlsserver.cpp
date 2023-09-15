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
#include <ctype.h>
#include <fstream>
#include <stdlib.h>
#include "../../include/secureblackbox.h"
#include <cstring>

#define LINE_LEN 80

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

int main(int argc, char* argv[]) {

    if (argc < 2) {
        fprintf(stderr, "\n");
        fprintf(stderr, "  port         the port of server\n");
        fprintf(stderr, "  filename     the certificate path\n");
        fprintf(stderr, "  password     the certificate password\n");
        fprintf(stderr, "  \nExample    dtlsserver port filename.cert\n");
        fprintf(stderr, "  \nExample    dtlsserver port filename.cert \"123456\"\n");
        printf("Press enter to continue.\n");
        getchar();
    } else {
        MyDTLSServer server;
        CertificateManager certManager;
        int ret_code = 0;

        char command[LINE_LEN]; // user's command
        char msg[10000]; // storage for mail message

        if (server.GetActive()) {
            printf("Server already started\n");
            goto done;
        }
        if (argc > 2) {
            ret_code = certManager.ImportFromFile(argv[2], argc > 3 ? argv[3] : "");
        } else {
            printf("A server certificate is required for this demo.\n");
            goto done;
        }

        if (certManager.GetLastErrorCode() == 0) {
            server.SetServerCertCount(server.GetServerCertCount() + 1);
            server.SetServerCertHandle(server.GetServerCertCount() - 1, certManager.GetCertHandle());
            printf("Server certificate loaded\n");
        } else {
            printf("Failed to load certificate.\n");
            goto done;
        }
        for (int i = 0; i < strlen(argv[1]); i++) {
            if (!isdigit(argv[1][i])) {
                printf("Wrong port.\n");
                goto done;
            }
        }
        server.SetPort(atoi(argv[1]));

        if (ret_code = server.Start()) {
            goto done;
        }

        printf("The server started successfully!\n");
        printf("Type \"stop\" to stop the server.\n\n");

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
            } else if (!strcmp(command, "stop")) {
                if (server.GetActive()) {
                    ret_code = server.Stop();
                    if (ret_code == 0) {
                        printf("The server was stopped!");
                    } else {
                        goto done;
                    }
                }
                exit(0);
            } else {
                printf("Bad command!\n");
            }
        }
    done:
        if (ret_code) // Got an error.  The user is done.
        {
            if (server.GetActive()) {
                server.Stop();
            }
            printf("\nError: %d\n", ret_code);
            if (server.GetLastError()) {
                printf(" \"%s\"\n", server.GetLastError());
            }
        }
        printf("Exiting... (press enter)\n");
        getchar();
        exit(ret_code);
    }
}


