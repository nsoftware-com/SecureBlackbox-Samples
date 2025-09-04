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
#include <string.h>
#include <stdlib.h>
#include <sstream>
#include "../../include/secureblackbox.h"

#define LINE_LEN 80

enum { HEADERS, FULL, REPLY };

class MyPOP3Client : public POP3Client {

public:
    bool isConnected;

protected:
    int FireTLSCertValidate(POP3ClientTLSCertValidateEventParams *e) override {
        e->Accept = true;
        return 0;
    };
};

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

void displayHelp(const char* errMes) {
	printf(
		"NAME\n"
		"  popclient -- SecureBlackbox POPClient Demo Application\n\n"
		"SYNOPSIS\n"
		"  popclient <host> <port> [-username username] [-password password] [-tlsmode tlsmode]\n\n"
		"DESCRIPTION\n"
		"  This sample how to use POP component to view messages in the mailbox.\n\n"
		"  The options are as follows:\n\n"
		"  host        The name or address of a mail server (Required).\n\n"
		"  port        The port of a mail server (Required).\n\n"
		"  -username   The user identifier for the mailbox.\n\n"
		"  -password   The password for the mailbox user.\n\n"
		"  -tlsmode    The TLS mode. Enter the corresponding string. Valid values: none, explicit, implicit.\n\n"
		"EXAMPLES\n"
		"  popclient mail.local 995\n\n"
		"  popclient mail.local 12345 -username testuser -password pass -tlsmode implicit\n\n"
	);

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char* argv[]) {
	MyPOP3Client client;
	char command[LINE_LEN]; // user's command
	char msg[10000]; // storage for mail message

	if (argc < 3) {
		displayHelp("");
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

	client.Config("RequestUIDs=True");
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
	
	if (client.Connect(argv[1], atoi(argv[2]))) {
		goto err;
	}

	client.isConnected = true;
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
			if (client.ListMessages()) {
				goto err;
			}
			if (client.GetMsgInfoCount() != 0) {
				printf("Uid     Size\n");
				for (int i = 0; i < client.GetMsgInfoCount(); i++) {
					printf("%s     %lld\n", client.GetMsgInfoUID(i), client.GetMsgInfoSize(i));
				}
			}
			else {
				printf("No messages on the server.\n");
			}
		}
		else if (!strcmp(command, "d")) {
			if (argument = strtok(NULL, " \t\n")) {
				if (client.DeleteMessage(atoi(argument))) {
					goto err;
				}
			}
			else {
				printf("No message specified.\n");
			}
		}
		else if (!strcmp(command, "h") || !strcmp(command, "f")) {
			if (argument = strtok(NULL, " \t\n")) {
				if (client.ReceiveMessage(atoi(argument))) {
					goto err;
				}
				printf("Message info:\n");
				printf("From: %s\n", client.GetMsgFrom());
				printf("To: %s\n", client.GetMsgSendTo());
				printf("Date: %s\n", client.GetMsgDate());
				printf("Subject: %s\n", client.GetMsgSubject());

				switch (client.GetMsgPriority()) {
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
					printf("Plain text: %s\n\n", client.GetMsgPlainText());
					printf("Html text: %s\n\n", client.GetMsgHtmlText());
				}
			}
			else {
				printf("No message specified.\n");
			}
		}
		else if (!strcmp(command, "q")) {
			printf("Save changes to inbox? (y/n): ");
			if (getchar() == 'n') {
				client.Undelete();
			}
			goto err;
		}
		else if (!strcmp(command, "")) {
			// do nothing
		}
		else {
			printf("Bad command / Not implemented in demo.\n");
		} // end of command checking
	} // end of main while loop

err:
	if (client.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", client.GetLastErrorCode(), client.GetLastError());
	}
	if (client.isConnected) {
		client.Disconnect();
		client.isConnected = false;
	}
done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


