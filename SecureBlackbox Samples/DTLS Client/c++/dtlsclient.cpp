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
#include <stdlib.h>
#include "../../include/secureblackbox.h"
#include <cstring>

#define LINE_LEN 80


class MyDTLSClient : public DTLSClient {

protected:
    int FireTLSCertValidate(DTLSClientTLSCertValidateEventParams* e) override {
        if (GetTLSServerCertCount() == 0) return 1;
        // do not do this in production code
        e->Accept = true;
        printf("Server certificate recived: %s\n", GetTLSServerCertIssuer(GetTLSServerCertCount() - 1));
        return 0;
    }
};

void displayHelp() {
	printf(
		"NAME\n"
		"  dtlsclient -- SecureBlackbox DTLSClient Demo Application\n\n"
		"SYNOPSIS\n"
		"  dtlsclient <host> <port>\n\n"
		"DESCRIPTION\n"
		"  This sample is a very simple program that shows how to use the DTLSClient component\n" 
		"  to send and receive messages to and from a DTLS server.\n\n"
		"  The options are as follows:\n\n"
		"  host       The local host of serve (Required).\n\n"
		"  port       The port of server (Required).\n\n"
		"EXAMPLES\n"
		"  dtlsclient localhost 4433\n\n"
	);
}

int main(int argc, char* argv[]) {
	MyDTLSClient client;

	if (argc < 3) {
		displayHelp();
		goto done;
	}

	char command[LINE_LEN]; // user's command
	char msg[10000]; // storage for mail message
	client.SetTLSAutoValidateCertificates(false);
	client.SetSocketTimeout(4000);

	if (client.Connect(argv[1], atoi(argv[2]))) {
		goto err;
	}
	printf("Connected successfully!\n");
	printf("Type \"?\" for a list of commands.\n\n");

	while (1) {
		if (client.GetLastErrorCode()) {
			goto err;
		}

		strcpy(msg, "");
		printf("> ");
		fgets(command, LINE_LEN, stdin);
		command[strlen(command) - 1] = '\0';
		char* argument = strtok(command, " \t\n");

		if (!strcmp(command, "?")) {
			printf("  send <message>        send a letter to the server and receive a response\n");
			printf("  q                     quit\n");
		}
		else if (!strcmp(command, "send")) {
			if (argument = strtok(NULL, " \t\n")) {
				if (client.SendText(argument) == 0) {
					printf("C->S: %s\n", argument);
					const int maxPartSize = 1000;
					printf("S->C: ");
					client.ReceiveData(maxPartSize);
					while (strlen(client.GetOutputString()) == 0) {
						if (client.ReceiveData(maxPartSize)) {
							goto err;
						}
					}
					printf("%s\n", client.GetOutputString());
				}
				else {
					goto err;
				}
			}
		}
		else if (!strcmp(command, "q")) {
			goto quit;
		}
		else {
			printf("Bad command!\n");
		}
	}
err:
	if (client.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", client.GetLastErrorCode(), client.GetLastError());
	}
done:
	printf("Press Enter to exit the demo.\n");
	getchar();
quit:
	if (client.GetConnected()) {
		client.Disconnect();
	}
}


