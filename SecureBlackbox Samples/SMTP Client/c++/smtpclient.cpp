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
#include "../../include/secureblackbox.h"

int main(int argc, char* argv[]) {
    if (argc < 12) {
        fprintf(stderr, "* This demo shows how to use SMTP component to send messages in the mailbox. *\n");
        fprintf(stderr, "usage: smtp host port user password tls mode \"from name\" \"from address\" \"to name\" \"to address\" subject text\n\n");
        fprintf(stderr, "\n");
        fprintf(stderr, "  host             the name or address of a mail server (internet post office server)\n");
        fprintf(stderr, "  port             the port of a mail server\n");
        fprintf(stderr, "  user             the user identifier for the mailbox\n");
        fprintf(stderr, "  password         the password for the mailbox user\n");
        fprintf(stderr, "  tls mode:        0 - no tls, 1 - explicit, 2 - implicit\n");
        fprintf(stderr, "  from name        the sender name\n");
        fprintf(stderr, "  from address     the sender address\n");
        fprintf(stderr, "  to name          the receiver name\n");
        fprintf(stderr, "  to address       the receiver address\n");
        fprintf(stderr, "  subject          the letter subject\n");
        fprintf(stderr, "  priority         the priority of letter: default is normal(2), 0 - the lowest, 4 - the highest\n");
        fprintf(stderr, "  text             the text message\n");
        fprintf(stderr, "\nExample: smtp mail.local 587 username password 1 John john@mail.com Ben ben@mail.com \"Good news\" \"Hello from smtp client\"\n\n");
        printf("Press enter to continue.");
        getchar();
    } else {
        SMTPClient client; // SMTP object
        MailWriter writer; // MailWriter object
        int ret_code = 0; // Return code:
        // = 0 when there is no error
        // = error code when there is an error

        writer.SetFromAddrCount(1);
        writer.SetFromAddrDisplayName(0, argv[6]);
        writer.SetFromAddrAddress(0, argv[7]);
        client.SetMsgFrom(writer.GetMsgFrom());

        writer.SetSendToAddrCount(1);
        writer.SetSendToAddrDisplayName(0, argv[8]);
        writer.SetSendToAddrAddress(0, argv[9]);
        client.SetMsgSendTo(writer.GetMsgSendTo());

        client.SetMsgSubject(argv[10]);
        if (argc == 13) {
            if (atoi(argv[11]) == 0) {
                client.SetMsgPriority(MP_LOWEST);
            } else if (atoi(argv[11]) == 1) {
                client.SetMsgPriority(MP_LOW);
            } else if (atoi(argv[11]) == 3) {
                client.SetMsgPriority(MP_HIGH);
            } else if (atoi(argv[11]) == 4) {
                client.SetMsgPriority(MP_HIGHEST);
            }
        }

        if (argc > 11) {
            client.SetMsgPlainText(argv[argc - 1]);
        } else {
            client.SetMsgPlainText("");
        }

        const int tlsMode = atoi(argv[5]);
        if (tlsMode > 0 && tlsMode < 3 && strlen(argv[5]) == 1) {
            client.SetTLSTLSMode(tlsMode == 1 ? SM_EXPLICIT_TLS : SM_IMPLICIT_TLS);
        }

        client.SetUsername(argv[3]);
        client.SetPassword(argv[4]);

        if (ret_code = client.Connect(argv[1], atoi(argv[2]))) {
            goto done;
        } else {
            client.SendMessage();
            printf("Message has been sent successfully!\n");
        }

    done:
        if (ret_code) {
            // Got an error.  The user is done.
            printf("\nError: %d", ret_code);
            if (client.GetLastError()) {
                printf(" \"%s\"\n", client.GetLastError());
            }
            printf("Exiting... (press enter)\n");
            getchar();
        }
        client.Disconnect();
        exit(static_cast<bool>(ret_code));
    }
}

