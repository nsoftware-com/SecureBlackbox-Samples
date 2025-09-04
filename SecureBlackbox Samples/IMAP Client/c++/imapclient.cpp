/*
 * SecureBlackbox 2024 C++ Edition - Sample Project
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

namespace ArgParser {
	static char* optval(int argc, char** argv, const char* option) {
		for (int x = 0; x < argc; x++) {
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
		"  imapclient -- SecureBlackbox IMAPClient Demo Application\n\n"
		"SYNOPSIS\n"
		"  imapclient <host> <port> [-username username] [-password password] [-tlsmode tlsmode]\n\n"
		"DESCRIPTION\n"
		"  This sample how to deal with IMAP4 servers. It can list mailboxes on the server, \n"
		"  list messages in the selected mailbox, download them from the server and upload local messages to the server.\n\n"
		"  The options are as follows:\n\n"
		"  host        The name or address of a mail server (Required).\n\n"
		"  port        The port of a mail server (Required).\n\n"
		"  -username   The user identifier for the mailbox.\n\n"
		"  -password   The password for the mailbox user.\n\n"
		"  -tlsmode    The TLS mode. Enter the corresponding string. Valid values: none, explicit, implicit.\n\n"
		"EXAMPLES\n"
		"  imapclient mail.local 995\n\n"
		"  imapclient mail.local 12345 -username testuser -password pass -tlsmode implicit\n\n"
	);
}

int main(int argc, char* argv[]) {
	IMAPClient client;

	if (argc < 3) {
		displayHelp();
		goto done;
	}

	char* username = optval(argc, argv, "-username");
	if (strcmp(username, "")) {
		client.SetUsername(username);
	}

	char* password = optval(argc, argv, "-password");
	if (strcmp(password, "")) {
		client.SetPassword(password);
	}

	client.SetTLSTLSMode(SM_NO_TLS);
	if (optext(argc, argv, "-tlsmode")) {
		char* tlsmode = optval(argc, argv, "-tlsmode");
		if (!strcmp(tlsmode, "none")) {
			client.SetTLSTLSMode(SM_NO_TLS);
		}
		else if (!strcmp(tlsmode, "explicit")) {
			client.SetTLSTLSMode(SM_EXPLICIT_TLS);
		}
		else if (!strcmp(tlsmode, "implicit")) {
			client.SetTLSTLSMode(SM_IMPLICIT_TLS);
		}
	}

	client.SetTLSRevocationCheck(CRC_ALL_CRL);

	if (client.Connect(argv[1], atoi(argv[2]))) {
		goto err;
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
		}
		else if (!strcmp(command, "s")) {
			argument = strtok(NULL, " \t\n");
			if (client.SelectMailbox(client.GetMailboxInfoName(atoi(argument))) == 0) {
				loadMailboxes(client);
			}
			else {
				goto err;
			}
		}
		else if (!strcmp(command, "e")) {
			argument = strtok(NULL, " \t\n");
			if (client.ExamineMailbox(client.GetMailboxInfoName(atoi(argument))) == 0) {
				loadMailboxes(client);
			}
			else {
				goto err;
			}
		}
		else if (!strcmp(command, "c")) {
			argument = strtok(NULL, " \t\n");
			if (client.CreateMailbox(argument) == 0) {
				loadMailboxes(client);
			}
			else {
				goto err;
			}
		}
		else if (!strcmp(command, "d")) {
			argument = strtok(NULL, " \t\n");
			if (client.DeleteMailbox(client.GetMailboxInfoName(atoi(argument))) == 0) {
				loadMailboxes(client);
			}
			else {
				goto err;
			}
		}
		else if (!strcmp(command, "fa")) {
			if (client.ListAllMessages() == 0) {
				loadMessages(client);
			}
			else {
				goto err;
			}
		}
		else if (!strcmp(command, "fd")) {
			if (client.ListDeletedMessages() == 0) {
				loadMessages(client);
			}
			else {
				goto err;
			}
		}
		else if (!strcmp(command, "fn")) {
			if (client.ListNewMessages() == 0) {
				loadMessages(client);
			}
			else {
				goto err;
			}
		}
		else if (!strcmp(command, "fr")) {
			if (client.ListRecentMessages() == 0) {
				loadMessages(client);
			}
			else {
				goto err;
			}
		}
		else if (!strcmp(command, "fu")) {
			if (client.ListUnseenMessages() == 0) {
				loadMessages(client);
			}
			else {
				goto err;
			}
		}
		else if (!strcmp(command, "md")) {
			if (client.MarkMessageDeleted(client.GetMsgInfoUID(msgnum)) == 0) {
				loadMailboxes(client);
			}
			else {
				goto err;
			}
		}
		else if (!strcmp(command, "pu")) {
			if (client.PurgeMessages() == 0) {
				loadMailboxes(client);
				loadMessages(client);
			}
			else {
				goto err;
			}
		}
		else if (!strcmp(command, "r")) {
			argument = strtok(NULL, " \t\n");
			if (client.ReceiveFile(client.GetMsgInfoUID(msgnum), argument) == 0) {
				printf("The message has been received successfully.\n");
			}
			else {
				goto err;
			}
		}
		else if (!strcmp(command, "po")) {
			argument = strtok(NULL, " \t\n");
			if (client.PostFile(argument, 0, "") == 0) {
				loadMessages(client);
				printf("The message has been posted successfully.\n");
			}
			else {
				goto err;
			}
		}
		else if (!strcmp(command, "md")) {
			if (client.MarkMessageDeleted(client.GetMsgInfoUID(msgnum)) == 0) {
				loadMailboxes(client);
			}
			else {
				goto err;
			}
		}
		else if (!strcmp(command, "h")) {
			if (client.GetMsgInfoCount() > msgnum) {
				printf("From: %s\n, To: %s\n, Subject: %s\n, Date: %s\n, Size: %lld\n",
					client.GetMsgInfoFrom(msgnum), client.GetMsgInfoSentTo(msgnum),
					client.GetMsgInfoSubject(msgnum),
					client.GetMsgInfoDate(msgnum), client.GetMsgInfoSize(msgnum));
			}
			else {
				printf("No info with active message number\n");
				goto done;
			}

		}
		else if (!strcmp(command, "l")) {
			if (client.ListMailboxes()) {
				goto err;
			};
			for (int i = 0; i < client.GetMailboxInfoCount(); i++) {
				char* HasChildren = client.GetMailboxInfoHasChildren(i) ? "Yes" : "No";
				char* Marked = client.GetMailboxInfoUnmarked(i) ? "No" : "Yes";
				char* Select = client.GetMailboxInfoNoSelect(i) ? "No" : "Yes";
				char* Inferiors = client.GetMailboxInfoNoInferiors(i) ? "No" : "Yes";
				printf("Id=%i Name=%s Children=%s Marked=%s Select=%s Inferiors=%s\n", i, client.GetMailboxInfoName(i), HasChildren, Marked, Select,
					Inferiors);
			}
		}
		else if (!strcmp(command, "n")) {
			if (msgnum < client.GetMsgInfoCount() - 1) {
				msgnum++;
			}
			else {
				printf(
					"The active letter cannot be assigned to switch to the next letter, since the current letter is the last one.\n");
			}
			sprintf(buffer, "%i", msgnum);
		}
		else if (!strcmp(command, "p")) {
			if (msgnum > 0) {
				msgnum--;
			}
			else {
				printf(
					"The active letter cannot be assigned to switch to the previous letter, since the current letter is the first.\n");
			}
			sprintf(buffer, "%i", msgnum);
		}
		else if (!strcmp(command, "pnum")) {
			printf("%i", msgnum);
		}
		else if (!strcmp(command, "q")) {
			client.Disconnect();
			goto err;
		}
		else if (!strcmp(command, "")) {
			// do nothing
		}
		else {
			if (isdigit((int)command[0])) {
				// allow user to enter only the number
				//  of the message they want to view
				int newNumb = atoi(command);
				if (newNumb >= 0 && newNumb < client.GetMsgInfoCount()) {
					msgnum = newNumb;
				}
				else {
					printf("Not valid number\n");
				}
			}
			else {
				printf("Bad command / Not implemented in demo.\n");
			}
		} // end of command checking
	} // end of main while loop

err:
	if (client.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", client.GetLastErrorCode(), client.GetLastError());
	}
done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


