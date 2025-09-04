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
#include <iostream>
#include <fstream>
#include <string>
#include "../../include/secureblackbox.h"

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
		"  smtpclient -- SecureBlackbox SMTPClient Demo Application\n\n"
		"SYNOPSIS\n"
		"  smtpclient <host> <port> <from> <sender> <to> <receiver> <subject> <priority> [-username username]\n"
		"            [-password password] [-tlsmode tlsmode] [-plain plain_file] [-html html_file]\n\n"
		"DESCRIPTION\n"
		"  This sample shows how to compose an e-mail message. Also, it's possible to sign and/or encrypt the message with certificates.\n\n"
		"  The options are as follows:\n\n"		
		"  host        The name or address of a mail server (Required).\n\n"
		"  port        The port of a mail server (Required).\n\n"
		"  from        The sender mail address (Required).\n\n"
		"  sender      The sender name (Required).\n\n"
		"  to          The receiver mail address (Required).\n\n"
		"  receiver    The receiver name (Required).\n\n"
		"  subject     The letter subject (Required).\n\n"
		"  priority    The priority of letter. Enter the corresponding number from 0 (the lowest) to 4 (the highest) (Required).\n\n"
		"  -username   The user identifier for the mailbox.\n\n"
		"  -password   The password for the mailbox user.\n\n"
		"  -tlsmode    The TLS mode. Enter the corresponding string. Valid values: none, explicit, implicit.\n\n"
		"  -plain      The file with plain text message.\n\n"
		"  -html       The file with html text message.\n\n"
		"EXAMPLES\n"
		"  smtpclient mail.local 995 Sbb@mail.com SbbTeam user@mail.com User \"test example\" 2 -plain C:\\test.txt\n\n"
		"  smtpclient mail.local 12345 Sbb@mail.com SbbTeam user@mail.com User \"test example\" 3 -username testuser -password pass -tlsmode implicit\n\n"
	);

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char* argv[]) {
	SMTPClient client;
	MailWriter writer;

	if (argc < 9) {
		displayHelp("Required parameters are not specified.");
		goto done;
	}

	writer.SetFromAddrCount(1);
	writer.SetFromAddrAddress(0, argv[3]);
	writer.SetFromAddrDisplayName(0, argv[4]);
	client.SetMsgFrom(writer.GetMsgFrom());

	writer.SetSendToAddrCount(1);
	writer.SetSendToAddrAddress(0, argv[5]);
	writer.SetSendToAddrDisplayName(0, argv[6]);
	client.SetMsgSendTo(writer.GetMsgSendTo());

	client.SetMsgSubject(argv[7]);

	if (atoi(argv[8]) == 0) {
		client.SetMsgPriority(MP_LOWEST);
	}
	else if (atoi(argv[87]) == 1) {
		client.SetMsgPriority(MP_LOW);
	}
	else if (atoi(argv[8]) == 2) {
		client.SetMsgPriority(MP_NORMAL);
	}
	else if (atoi(argv[8]) == 3) {
		client.SetMsgPriority(MP_HIGH);
	}
	else if (atoi(argv[8]) == 4) {
		client.SetMsgPriority(MP_HIGHEST);
	}

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

	char* plain = optval(argc, argv, "-plain");
	if (strcmp(plain, "")) {
		std::ifstream in(plain);
		std::string contents((std::istreambuf_iterator<char>(in)),
			std::istreambuf_iterator<char>());
		client.SetMsgPlainText(contents.c_str());
	}

	char* html = optval(argc, argv, "-html");
	if (strcmp(html, "")) {
		std::ifstream in(html);
		std::string contents((std::istreambuf_iterator<char>(in)),
			std::istreambuf_iterator<char>());
		client.SetMsgHtmlText(contents.c_str());
	}

	if (client.Connect(argv[1], atoi(argv[2]))) {
		goto err;
	}
	else {
		client.SendMessage();
		printf("Message has been sent successfully!\n\n");
		client.Disconnect();
	}

err:
	if (writer.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", writer.GetLastErrorCode(), writer.GetLastError());
	}
done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}



