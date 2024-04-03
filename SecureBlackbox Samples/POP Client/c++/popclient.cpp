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

#include <sstream>

#define LINE_LEN 80

enum { HEADERS, FULL, REPLY };

class MyPOP3Client : public POP3Client {

public:
    bool isConnected;

protected:
    int FireCertificateValidate(POP3ClientCertificateValidateEventParams* e) override {
        e->Accept = true;
        return 0;
    };
};

int main(int argc, char* argv[]) {
    if (argc < 5) {
        fprintf(stderr, "* This demo shows how to use POP component to view messages in the mailbox. *\n");
        fprintf(stderr, "usage: popclient host port user password\n");
        fprintf(stderr, "\n");
        fprintf(stderr, "  host        the name or address of a mail server (internet post office server)\n");
        fprintf(stderr, "  port        the port of a mail server\n");
        fprintf(stderr, "  user        the user identifier for the mailbox\n");
        fprintf(stderr, "  password    the password for the mailbox user\n");
        fprintf(stderr, "  default - no tls, optionally you can use tls mode: 0 - no tls, 1 - explicit, 2 - implicit;\n");
        fprintf(stderr, "\nExample: popclient mail.local 995 username password 2\n\n");
        printf("Press enter to continue.");
        getchar();
    } else {
        MyPOP3Client pop; // POP object
        int ret_code = 0; // Return code:
        // = 0 when there is no error
        // = error code when there is an error

        char command[LINE_LEN]; // user's command

        char msg[10000]; // storage for mail message

        pop.Config("RequestUIDs=True");
        pop.SetTLSTLSMode(SM_NO_TLS);

        if (argc == 6) {
            const int tlsMode = atoi(argv[5]);
            if (tlsMode > 0 && tlsMode < 3 && strlen(argv[5]) == 1) {
                pop.SetTLSTLSMode(tlsMode == 1 ? SM_EXPLICIT_TLS : SM_IMPLICIT_TLS);
            }
        }
        pop.SetUsername(argv[3]);
        pop.SetPassword(argv[4]);

        std::stringstream geek(argv[2]);

        int port = 110;
        geek >> port;
        if (ret_code = pop.Connect(argv[1], port)) {
            goto done;
        }
        pop.isConnected = true;
        printf("Connected successfully!\n");
        printf("Type \"?\" for a list of commands.\n\n");

        while (1) {
            strcpy(msg, "");
            printf("> ");
            fgets(command, LINE_LEN, stdin);
            command[strlen(command) - 1] = '\0';
            char* argument = strtok(command, " \t\n");

            if (!strcmp(command, "?")) {
                printf("Mail Commands\n"
                    "l                               listing messages in mbox\n"
                    "h <message number>              print out message headers\n"
                    "f <message number>              print out full message info\n"
                    "d <message number>              delete message\n"
                    "q                               quit, saving unresolved messages in mbox\n");
            }
            else if (!strcmp(command, "l")) {
                if (ret_code = pop.ListMessages()) {
                    goto done;
                }
                if (pop.GetMsgInfoCount() != 0) {
                    printf("Uid     Size\n");
                    for (int i = 0; i < pop.GetMsgInfoCount(); i++) {
                        printf("%s     %lld\n", pop.GetMsgInfoUID(i), pop.GetMsgInfoSize(i));
                    }
                }
                else {
                    printf("No messages on the server.\n");
                }
            } else if (!strcmp(command, "d")) {
                if (argument = strtok(NULL, " \t\n")) {
                    if (ret_code = pop.DeleteMessage(atoi(argument))) {
                        goto done;
                    }
                } else {
                    printf("No message specified.\n");
                }
            } else if (!strcmp(command, "h") || !strcmp(command, "f")) {
                if (argument = strtok(NULL, " \t\n")) {
                    if (ret_code = pop.ReceiveMessage(atoi(argument))) {
                        goto done;
                    }
                    printf("Message info:\n");
                    printf("From: %s\n", pop.GetMsgFrom());
                    printf("To: %s\n", pop.GetMsgSendTo());
                    printf("Date: %s\n", pop.GetMsgDate());
                    printf("Subject: %s\n", pop.GetMsgSubject());

                    switch (pop.GetMsgPriority()) {
                    case MP_LOWEST:
                        printf("Priority: %s\n", "[lowest]");
                        break;
                    case MP_LOW:
                        printf("Priority: %s\n", "[low]");
                        break;
                    case MP_NORMAL:
                        printf("Priority: %s\n", "[normal]");
                        break;
                    case MP_HIGH:
                        printf("Priority: %s\n", "[HIGH]");
                        break;
                    case MP_HIGHEST:
                        printf("Priority: %s\n", "[HIGHEST]");
                        break;
                    }
                    if (!strcmp(command, "f")) {
                        printf("Plain text: %s\n\n", pop.GetMsgPlainText());
                        printf("Html text: %s\n\n", pop.GetMsgHtmlText());
                    }
                } else {
                    printf("No message specified.\n");
                }
            } else if (!strcmp(command, "q")) {
                printf("Save changes to inbox? (y/n): ");
                if (getchar() == 'n') {
                    ret_code = pop.Undelete();
                }
                goto done;
            } else if (!strcmp(command, "")) {
                // do nothing
            } else {
                printf("Bad command / Not implemented in demo.\n");
            } // end of command checking
        } // end of main while loop

    done:
        if (ret_code) {
            // Got an error.  The user is done.
            printf("\nError: %d", ret_code);
            if (pop.GetLastError()) {
                printf(" \"%s\"\n", pop.GetLastError());
            }
            printf("Exiting... (press enter)\n");
            getchar();
        }
        pop.Disconnect();
        pop.isConnected = false;
        exit(static_cast<bool>(ret_code));
    }
}


