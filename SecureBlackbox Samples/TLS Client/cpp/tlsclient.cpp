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
#include <stdlib.h>
#include <cstring>
#include "../../include/secureblackbox.h"

#define LINE_LEN 80


class MyTLSClient : public TLSClient {

protected:
    int FireCertificateValidate(TLSClientCertificateValidateEventParams* e) override {
        e->Accept = true;
        return 0;
    }
};

int main(int argc, char* argv[]) {

    if (argc < 3) {
        fprintf(stderr, "\n");
        fprintf(stderr, "  <host>         the local host of server\n");
        fprintf(stderr, "  <port>         the port of server\n");
        fprintf(stderr, "  <tls>          1 for TLS, 0 (default) for plain TCP\n");
        fprintf(stderr, "  \nExample    tlsclient 127.0.0.1 80\n");
        fprintf(stderr, "  \nExample    tlsclient 127.0.0.1 443 1\n");
        printf("Press enter to continue.\n");
        getchar();
    } else {
        MyTLSClient client;
        int ret_code = 0;

        char command[LINE_LEN]; // user's command
        char msg[10000]; // storage for mail message
        client.SetTLSAutoValidateCertificates(false);
        client.SetSocketTimeout(4000);
        client.SetTLSTLSMode(SM_NO_TLS);
        if (argc > 3 && atoi(argv[3])) {
            client.SetTLSTLSMode(SM_IMPLICIT_TLS);
        }

        if (ret_code = client.Connect(argv[1], atoi(argv[2]))) {
            goto done;
        }
        printf("Connected successfully!\n");
        printf("Type \"?\" for a list of commands.\n\n");

        while (1) {
            if (ret_code) {
                goto done;
            }

            strcpy(msg, "");
            printf("> ");
            fgets(command, LINE_LEN, stdin);
            command[strlen(command) - 1] = '\0';
            char* argument = strtok(command, " \t\n");

            if (!strcmp(command, "?")) {
                printf("  send <message>        send a letter to the server and receive a response\n");
                printf("  q                     quit\n");
            } else if (!strcmp(command, "send")) {
                if (argument = strtok(NULL, " \t\n")) {
                    if (ret_code = client.SendText(argument) == 0) {
                        printf("C->S: %s\n", argument);
                        const int maxPartSize = 1000;
                        printf("S->C: ");
                        client.ReceiveData(maxPartSize);
                        while (strlen(client.GetOutputString()) == 0) {
                            if (ret_code = client.ReceiveData(maxPartSize)) {
                                goto done;
                            }
                        }
                        printf("%s\n", client.GetOutputString());
                    } else {
                        goto done;
                    }
                }
            } else if (!strcmp(command, "q")) {
                goto done;
            } else {
                printf("Bad command!\n");
            }
        }
    done:
        if (ret_code) // Got an error.  The user is done.
        {
            printf("\nError: %d\n", ret_code);
            if (client.GetLastError()) {
                printf(" \"%s\"\n", client.GetLastError());
            }
        }
        printf("Exiting... (press enter)\n");
        getchar();
        exit(ret_code);
    }
}


