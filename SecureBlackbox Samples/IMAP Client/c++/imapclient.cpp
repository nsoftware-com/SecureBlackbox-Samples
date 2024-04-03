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
#include <string.h>
#include <stdlib.h>
#include "../../include/secureblackbox.h"

#define LINE_LEN 80

void loadMessages(IMAPClient& client) {
    printf("Messages:\n");
    for (int i = 0; i < client.GetMsgInfoCount(); i++) {
        printf("From: %s\n, To: %s\n, Subject: %s\n, Date: %s\n, Size: %lld\n",
               client.GetMsgInfoFrom(i), client.GetMsgInfoSentTo(i), client.GetMsgInfoSubject(i),
               client.GetMsgInfoDate(i), client.GetMsgInfoSize(i));
    }
}

void loadMailboxes(IMAPClient& client) {
    printf("Name: %s\n Total: %i Unseen: %i Recent: %i Read-only: %s\n ", client.GetCurrMailboxName(),
           client.GetCurrMailboxTotalMessages(), client.GetCurrMailboxUnseenMessages(),
           client.GetCurrMailboxRecentMessages(),
           client.GetCurrMailboxReadOnly() ? "Yes" : "No");
}

int main(int argc, char* argv[]) {

    if (argc < 5) {
        fprintf(stderr, "usage: imap host port user password\n");
        fprintf(stderr, "\n");
        fprintf(stderr, "  host        the name or address of a mail server (internet post office server)\n");
        fprintf(stderr, "  port        the port of a mail server\n");
        fprintf(stderr, "  user        the user identifier for the mailbox\n");
        fprintf(stderr, "  password    the password for the mailbox user\n");
        fprintf(stderr, "  default - no tls, optionally you can use tls mode: 0 - no tls, 1 - explicit, 2 - implicit;\n");
        fprintf(stderr, "\nExample: imap mail.local 995 username password 2\n\n");
        printf("Press enter to continue.\n");
        getchar();
    } else {
        IMAPClient client;
        int ret_code = 0;

        client.SetTLSRevocationCheck(CRC_ALL_CRL);
        client.SetTLSTLSMode(SM_NO_TLS);

        if (argc > 5) {
            const int tlsMode = atoi(argv[5]);
            if (tlsMode > 0 && tlsMode < 3 && strlen(argv[5]) == 1) {
                client.SetTLSTLSMode(tlsMode == 1 ? SM_EXPLICIT_TLS : SM_IMPLICIT_TLS);
            }
        }
        client.SetUsername(argv[3]);
        client.SetPassword(argv[4]);
        if (ret_code = client.Connect(argv[1], atoi(argv[2]))) {
            goto done;
        }

        printf("Connected successfully.\n");

        char command[LINE_LEN]; // user's command
        char buffer[LINE_LEN]; // text buffer

        int msgnum = 0; // current message number for next command

        printf("Type \"?\" for a list of commands.\n\n");
        while (1) {
            printf("> ");
            fgets(command, LINE_LEN, stdin);
            buffer[strlen(command) - 1] = '\0';
            char* argument = strtok(command, " \n\t");

            if (!strcmp(command, "?")) {
                printf("IMAP Commands\n"
                    "l                               list all mailboxes\n"
                    "s <mailbox id>                  select mailbox\n"
                    "e <mailbox id>                  examine mailbox\n"
                    "c <mailbox>                     create mailbox\n"
                    "d <mailbox id>                  delete mailbox\n"
                    "n                               goto and type next message\n"
                    "p                               goto and type previous message\n"
                    "pnum                            print out active message number\n"  
                    "h                               print out active message headers\n"
                    "fa                              give head lines of messages(list all messages)\n"
                    "fd                              give head lines of deleted messages(list deleted messages)\n"
                    "fn                              give head lines of new messages(list new messages)\n"
                    "fr                              give head lines of recent messages(list recent messages)\n"
                    "fu                              give head lines of unseen messages(list unseen messages)\n"
                    "md                              mark message deleted\n"
                    "pu                              purge messages\n"
                    "r <filename>                    receive file\n"
                    "po <filename>                   post file\n"
                    "q                               quit, saving unresolved messages in mbox\n"
                    "<message number>                switch to new active message\n");
            } else if (!strcmp(command, "s")) {
                argument = strtok(NULL, " \t\n");
                ret_code = client.SelectMailbox(client.GetMailboxInfoName(atoi(argument)));
                if (ret_code == 0) {
                    loadMailboxes(client);
                } else {
                    goto done;
                }
            } else if (!strcmp(command, "e")) {
                argument = strtok(NULL, " \t\n");
                ret_code = client.ExamineMailbox(client.GetMailboxInfoName(atoi(argument)));
                if (ret_code == 0) {
                    loadMailboxes(client);
                } else {
                    goto done;
                }
            } else if (!strcmp(command, "c")) {
                argument = strtok(NULL, " \t\n");
                ret_code = client.CreateMailbox(argument);
                if (ret_code == 0) {
                    loadMailboxes(client);
                } else {
                    goto done;
                }
            } else if (!strcmp(command, "d")) {
                argument = strtok(NULL, " \t\n");
                ret_code = client.DeleteMailbox(client.GetMailboxInfoName(atoi(argument)));
                if (ret_code == 0) {
                    loadMailboxes(client);
                } else {
                    goto done;
                }
            } else if (!strcmp(command, "fa")) {
                ret_code = client.ListAllMessages();
                if (ret_code == 0) {
                    loadMessages(client);
                } else {
                    goto done;
                }
            } else if (!strcmp(command, "fd")) {
                ret_code = client.ListDeletedMessages();
                if (ret_code == 0) {
                    loadMessages(client);
                } else {
                    goto done;
                }
            } else if (!strcmp(command, "fn")) {
                ret_code = client.ListNewMessages();
                if (ret_code == 0) {
                    loadMessages(client);
                } else {
                    goto done;
                }
            } else if (!strcmp(command, "fr")) {
                ret_code = client.ListRecentMessages();
                if (ret_code == 0) {
                    loadMessages(client);
                } else {
                    goto done;
                }
            } else if (!strcmp(command, "fu")) {
                ret_code = client.ListUnseenMessages();
                if (ret_code == 0) {
                    loadMessages(client);
                } else {
                    goto done;
                }
            } else if (!strcmp(command, "md")) {
                ret_code = client.MarkMessageDeleted(client.GetMsgInfoUID(msgnum));
                if (ret_code == 0) {
                    loadMailboxes(client);
                } else {
                    goto done;
                }
            } else if (!strcmp(command, "pu")) {
                ret_code = client.PurgeMessages();
                if (ret_code == 0) {
                    loadMailboxes(client);
                    loadMessages(client);
                } else {
                    goto done;
                }
            } else if (!strcmp(command, "r")) {
                argument = strtok(NULL, " \t\n");
                ret_code = client.ReceiveFile(client.GetMsgInfoUID(msgnum), argument);
                if (ret_code == 0) {
                    printf("The message has been received successfully.\n");
                } else {
                    goto done;
                }
            } else if (!strcmp(command, "po")) {
                argument = strtok(NULL, " \t\n");
                ret_code = client.PostFile(argument, 0, "");
                if (ret_code == 0) {
                    loadMessages(client);
                    printf("The message has been posted successfully.\n");
                } else {
                    goto done;
                }
            } else if (!strcmp(command, "md")) {
                ret_code = client.MarkMessageDeleted(client.GetMsgInfoUID(msgnum));
                if (ret_code == 0) {
                    loadMailboxes(client);
                } else {
                    goto done;
                }
            } else if (!strcmp(command, "h")) {
                if (client.GetMsgInfoCount() > msgnum) {
                    printf("From: %s\n, To: %s\n, Subject: %s\n, Date: %s\n, Size: %lld\n",
                           client.GetMsgInfoFrom(msgnum), client.GetMsgInfoSentTo(msgnum),
                           client.GetMsgInfoSubject(msgnum),
                           client.GetMsgInfoDate(msgnum), client.GetMsgInfoSize(msgnum));
                } else {
                    printf("No info with active message number\n");
                    goto done;
                }

            } else if (!strcmp(command, "l")) {
                if (ret_code = client.ListMailboxes()) {
                    goto done;
                };
                for (int i = 0; i < client.GetMailboxInfoCount(); i++) {
                    char* HasChildren = client.GetMailboxInfoHasChildren(i) ? "Yes" : "No";
                    char* Marked = client.GetMailboxInfoUnmarked(i) ? "No" : "Yes";
                    char* Select = client.GetMailboxInfoNoSelect(i) ? "No" : "Yes";
                    char* Inferiors = client.GetMailboxInfoNoInferiors(i) ? "No" : "Yes";
                    printf("Id=%i Name=%s Children=%s Marked=%s Select=%s Inferiors=%s\n", i, client.GetMailboxInfoName(i), HasChildren, Marked, Select,
                           Inferiors);
                }
            } else if (!strcmp(command, "n")) {
                if (msgnum < client.GetMsgInfoCount() - 1) {
                    msgnum++;
                } else {
                    printf(
                        "The active letter cannot be assigned to switch to the next letter, since the current letter is the last one.\n");
                }
                sprintf(buffer, "%i", msgnum);
            } else if (!strcmp(command, "p")) {
                if (msgnum > 0) {
                    msgnum--;
                } else {
                    printf(
                        "The active letter cannot be assigned to switch to the previous letter, since the current letter is the first.\n");
                }
                sprintf(buffer, "%i", msgnum);
            }
            else if (!strcmp(command, "pnum")) {
                printf("%i", msgnum);
            } else if (!strcmp(command, "q")) {
                ret_code = client.Disconnect();
                exit(0);
            } else if (!strcmp(command, "")) {
                // do nothing
            } else {
                if (isdigit((int)command[0])) {
                    // allow user to enter only the number
                    //  of the message they want to view
                    int newNumb = atoi(command);
                    if (newNumb >= 0 && newNumb < client.GetMsgInfoCount()) {
                        msgnum = newNumb;
                    } else {
                        printf("Not valid number\n");
                    }
                } else {
                    printf("Bad command / Not implemented in demo.\n");
                }
            } // end of command checking
        } // end of main while loop

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


