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
#include "../../include/secureblackbox.h"
#define LINE_LEN 100

class MyFTP : public FTPClient
{
public:

    bool verbose = false;

    int FireTLSCertValidate(FTPClientTLSCertValidateEventParams* e) override {
        if (e->Accept) {
            return 0;
        }
        printf("Server provided the following certificate:\nIssuer: %s\nSubject: %s\n",
               this->GetTLSServerCertIssuerRDN(0), this->GetTLSServerCertSubjectRDN(0));
        printf("Would you like to continue? [y/n] ");
        if (getchar() == 'y') {
            e->Accept = true;
        } else {
            exit(0);
        }
        return 0;
    }

    int FireControlSend(FTPClientControlSendEventParams* e) override {
        if (verbose) printf("%s\r\n", e->TextLine);
        return 0;
    }

    int FireControlReceive(FTPClientControlReceiveEventParams* e) override {
        if (verbose) printf("%s\r\n", e->TextLine);
        return 0;
    }

    int FireListEntry(FTPClientListEntryEventParams* e) override {
        printf("  %s\r\n", GetCurrListEntryUnparsedName());
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
		"  ftpclient -- SecureBlackbox FTPClient Demo Application\n\n"
		"SYNOPSIS\n"
		"  ftpclient <host> <port> [-username username] [-password password] [-tlsmode tlsmode]\n\n"
		"DESCRIPTION\n"
		"  This sample illustrates basic FTP client operations.\n\n"
		"  The options are as follows:\n\n"
		"  host        The local host of serve (Required).\n\n"
		"  port        The port of server (Required).\n\n"
		"  -username   The user identifier to use for login.\n\n"
		"  -password   The password to log in.\n\n"
		"  -tlsmode    The TLS mode. Enter the corresponding string. Valid values: none, explicit, implicit.\n\n"
		"EXAMPLES\n"
		"  ftpclient localhost 21\n\n"
		"  ftpclient localhost 12345 -username testuser -password pass -tlsmode implicit\n\n"
	);
}

int main(int argc, char** argv) {
	MyFTP client;
	char command[LINE_LEN];
	char server[LINE_LEN];
	char portText[LINE_LEN];
	char* localFile;
	char* remoteFile;

	if (argc < 3) {
		displayHelp();
		goto done;
	}

	client.SetTLSAutoValidateCertificates(FALSE);

	char* username = optval(argc, argv, "-username");
	if (strcmp(username, "")) {
		client.SetUsername(username);
	}

	char* password = optval(argc, argv, "-password");
	if (strcmp(password, "")) {
		client.SetPassword(password);
	}

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

	// connect
	printf("Connecting to ftp://%s@%s:%i\r\n", username, argv[1], atoi(argv[2]));
	
	if (client.Connect(argv[1], atoi(argv[2]))) {
		goto err;
	}

	// main loop to check for commands
	while (1) {
		printf("ftp> ");
		fgets(command, LINE_LEN, stdin);
		command[strlen(command) - 1] = '\0';
		char* argument = strtok(command, " \t\n");

		if (!strcmp(command, "ascii")) {
			client.SetTransferType(CTT_TEXT);
			printf("Transfer mode text.\r\n");
		}
		else if (!strcmp(command, "binary")) {
			client.SetTransferType(CTT_BINARY);
			printf("Transfer mode binary.\r\n");
		}
		else if (!strcmp(command, "put")) {
			localFile = strtok(nullptr, " \t\n");
			remoteFile = strtok(nullptr, " \t\n");
			if (client.UploadFile(localFile, remoteFile)) {
				printf("Uploaded file: %s -> %s\r\n", localFile, remoteFile);
			}
			else {
				printf("Failed to upload file.\r\n");
			}
		}
		else if (!strcmp(command, "get")) {
			remoteFile = strtok(nullptr, " \t\n");
			localFile = strtok(nullptr, " \t\n");
			if (client.DownloadFile(remoteFile, localFile)) {
				printf("Downloaded file: %s -> %s\r\n", remoteFile, localFile);
			}
			else {
				printf("Failed to download file.\r\n");
			}
		}
		else if (!strcmp(command, "cd")) {
			argument = strtok(nullptr, " \t\n");
			if (!client.ChangeDir(argument)) {
				printf("Changed directory: %s\r\n", argument);
			}
			else {
				printf("Failed to change directory.\r\n");
			}
		}
		else if (!strcmp(command, "ls")) {
			printf("Listing %s\r\n", client.GetCurrentDir());
			client.ListDir(1, 1);
		}
		else if (!strcmp(command, "mkdir")) {
			argument = strtok(nullptr, " \t\n");
			if (!client.MakeDir(argument)) {
				printf("Created directory: %s\r\n", argument);
			}
			else {
				printf("Failed to create directory.\r\n");
			}
		}
		else if (!strcmp(command, "pwd")) {
			printf("%s\r\n", client.GetCurrentDir());
		}
		else if (!strcmp(command, "rm")) {
			argument = strtok(nullptr, " \t\n");
			if (!client.DeleteFile(argument)) {
				printf("Deleted file: %s\r\n", argument);
			}
			else {
				printf("Failed to delete file.\r\n");
			}
		}
		else if (!strcmp(command, "rmdir")) {
			argument = strtok(nullptr, " \t\n");
			if (!client.DeleteDir(argument)) {
				printf("Deleted directory: %s\r\n", argument);
			}
			else {
				printf("Failed to delete directory.\r\n");
			}
		}
		else if (!strcmp(command, "verbose")) {
			if (client.verbose) {
				client.verbose = false;
				printf("Verbose mode off.\r\n");
			}
			else {
				client.verbose = true;
				printf("Verbose mode on.\r\n");
			}
		}
		else if (!strcmp(command, "bye") || !strcmp(command, "exit") || !strcmp(command, "quit")) {
			client.Disconnect();
			goto quit;
		}
		else if (!strcmp(command, "?") || !strcmp(command, "help") || !strcmp(command, "man")) {
			goto helpcmds;
		}
		else {
			printf("Command recognized. Choose from these:\n\n");
		helpcmds:
			printf("?         cd        man       quit    \n"
				"ascii     get       mkdir     rm      \n"
				"binary    help      put       rmdir   \n"
				"bye       ls        pwd       verbose \n");
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


