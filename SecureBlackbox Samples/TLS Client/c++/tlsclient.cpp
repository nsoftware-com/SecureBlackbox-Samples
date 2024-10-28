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
#include <cstring>
#include "../../include/secureblackbox.h"

#define LINE_LEN 80


class MyTLSClient : public TLSClient {

protected:
    int FireTLSCertValidate(TLSClientTLSCertValidateEventParams* e) override {
        e->Accept = true;
        return 0;
    }
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

void displayHelp() {
	printf(
		"NAME\n"
		"  tlsclient -- SecureBlackbox TLSClient Demo Application\n\n"
		"SYNOPSIS\n"
		"  tlsclient <host> <port> [-tlsmode tlsmode]\n\n"
		"DESCRIPTION\n"
		"  This sample illustrates basic TLS client operations.\n\n"
		"  The options are as follows:\n\n"
		"  host        The local host of serve (Required).\n\n"
		"  port        The port of server (Required).\n\n"
		"  -tlsmode    The TLS mode. Enter the corresponding string. Valid values: none, implicit.\n\n"
		"EXAMPLES\n"
		"  tlsclient localhost 21\n\n"
		"  tlsclient localhost 12345 -tlsmode implicit\n\n"
	);
}

int main(int argc, char* argv[]) {
	MyTLSClient client;
	char command[LINE_LEN]; // user's command
	char msg[10000]; // storage for mail message

	if (argc < 3) {
		displayHelp();
		goto done;
	}

	client.SetTLSAutoValidateCertificates(false);
	client.SetSocketTimeout(4000);
	
	if (optext(argc, argv, "-tlsmode")) {
		char* tlsmode = optval(argc, argv, "-tlsmode");
		if (!strcmp(tlsmode, "none")) {
			client.SetTLSTLSMode(SM_NO_TLS);
		}
		else if (!strcmp(tlsmode, "implicit")) {
			client.SetTLSTLSMode(SM_IMPLICIT_TLS);
		}
	}

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
					goto done;
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


