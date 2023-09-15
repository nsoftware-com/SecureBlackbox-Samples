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

int main(int argc, char* argv[]) {

    if (argc < 2) {
        fprintf(stderr, "\n");
        fprintf(stderr, "  <port>       port number to listen on\n");
        fprintf(stderr, "  <tls>        talk TLS (1) or plain TCP (0, default)\n");
        fprintf(stderr, "  if <tls> is 1, please provide the following settings:\n");
        fprintf(stderr, "  filename     the certificate file path (PFX or PEM)\n");
        fprintf(stderr, "  password     the certificate password (optional)\n");
        fprintf(stderr, "  \nExample    tlsserver 16443 1 cert.pfx\n");
        fprintf(stderr, "  \nExample    tlsserver 16443 1 cert.pfx 123456\n");
        fprintf(stderr, "  \nExample    tlsserver 16080\n");
        printf("Press enter to continue.\n");
        getchar();
    } else {
        MyTLSServer server;
        CertificateManager certManager;
        int ret_code = 0;

        char command[LINE_LEN]; // user's command
        char msg[10000]; // storage for mail message

        if (server.GetActive()) {
            goto done;
        }
        if (argc > 2) {
            const bool useTls = atoi(argv[2]);
            server.SetUseTLS(useTls);
            if (useTls) {
                if (argc > 3) {
                    ret_code = certManager.ImportFromFile(argv[3], argv[4]);
                } else {
                    ret_code = certManager.ImportFromFile(argv[3], "");
                }
                if (certManager.GetLastErrorCode() == 0) {
                    server.SetServerCertCount(1);
                    server.SetServerCertHandle(0, certManager.GetCertHandle());
                } else {
                    printf("Failed to load certificate.\n");
                    goto done;
                }
            }
        }
        for (int i = 0; i < strlen(argv[1]); i++) {
            if (!isdigit(argv[1][i])) {
                printf("Wrong port.\n");
                goto done;
            }
        }
        server.SetPort(atoi(argv[1]));
        ret_code = server.Start();

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
                        printf("The server has stopped!");
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
        if (ret_code) // Got an error.  Exiting.
        {
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


