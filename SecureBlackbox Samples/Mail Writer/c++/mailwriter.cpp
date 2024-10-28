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
		"  mailwriter -- SecureBlackbox MailWriter Demo Application\n\n"
		"SYNOPSIS\n"
		"  mailwriter <from> <sender> <to> <cc> <bcc> <subject> <priority> <output>\n"
		"            [-plain plain_file] [-html html_file] [-scert certificate_file] [-scertpass certificate_password]\n"
		"            [-ecert certificate_file] [-ecertpass certificate_password] [-hashalg hashalg] [-encalg encalg]\n"
		"            [-format signature_format] [-a attach_file]\n\n"
		"DESCRIPTION\n"
		"  This sample shows how to compose an e-mail message. Also, it's possible to sign and/or encrypt the message with certificates.\n\n"
		"  The options are as follows:\n\n"
		"  from          The sender mail address (Required).\n\n"
		"  sender        The sender name (Required).\n\n"
		"  to            The recipient mail (Required).\n\n"
		"  cc            The carbon copy mail address (Required).\n\n"
		"  bcc           The blind carbon copy mail address (Required).\n\n"
		"  subject       The letter subject (Required).\n\n"
		"  priority      The priority of letter. Enter the corresponding number from 0 (the lowest) to 4 (the highest) (Required).\n\n"
		"  output        The output file (Required).\n\n"
		"  -plain        The file with plain text message.\n\n"
		"  -html         The file with html text message.\n\n"
		"  -scert        The certificate used to sign files.\n\n"
		"  -scertpass    The password for the signing certificate.\n\n"
		"  -ecert        The certificate used to encrypt files.\n\n"
		"  -ecertpass    The password for the encryption certificate.\n\n"
		"  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224, MD5\n\n"
		"  -encalg       The encryption algorithm. Enter the corresponding string. Valid values: DES, 3DES, AES128, AES192, AES256, Blowfish, Twofish, Camellia, Serpent\n\n"
		"  -format       The signature format. Enter the corresponding number. Valid values:\n\n"
		"                  0  - MS_MULTIPART_SIGNED\n"
		"                  1  - MS_SIGNED_DATA\n\n"
		"  -a            The attach file.\n\n"
		"EXAMPLES\n"
		"  mailwriter Sbb@mail.com SbbTeam user@mail.com \"alluser@mail.com allpeople@mail.com\" ghost@mail.com \"test example\" 2 mymail.eml\n\n"
		"  mailwriter Sbb@mail.com SbbTeam user@mail.com \"\" \"\" \"test example\" 1 mymail.eml\n"
		"          -plain C:\\test.txt -ecert C:\\certs\\mycert.pfx -ecertpass mypassword -hashalg SHA256 -format 0\n\n"
	);

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char* argv[]) {
	MailWriter writer;
	CertificateManager cm;

	if (argc < 9) {
		displayHelp("Required parameters are not specified.");
		goto done;
	}

	writer.SetMsgFrom(argv[1]);
	writer.SetMsgSender(argv[2]);
	writer.SetMsgSendTo(argv[3]);
	writer.SetMsgCc(argv[4]);
	writer.SetMsgBcc(argv[5]);
	writer.SetMsgSubject(argv[6]);
	if (atoi(argv[7]) == 0) {
		writer.SetMsgPriority(MP_LOWEST);
	}
	else if (atoi(argv[7]) == 1) {
		writer.SetMsgPriority(MP_LOW);
	}
	else if (atoi(argv[7]) == 2) {
		writer.SetMsgPriority(MP_NORMAL);
	}
	else if (atoi(argv[7]) == 3) {
		writer.SetMsgPriority(MP_HIGH);
	}
	else if (atoi(argv[7]) == 4) {
		writer.SetMsgPriority(MP_HIGHEST);
	}

	writer.SetSecSettingsSignBeforeEncrypt(false);
	writer.SetSecSettingsSignMessageHeader(false);

	// Additional options
	char* plain = optval(argc, argv, "-plain");
	if (strcmp(plain, "")) {
		std::ifstream in(plain);
		std::string contents((std::istreambuf_iterator<char>(in)),
			std::istreambuf_iterator<char>());
		writer.SetMsgPlainText(contents.c_str());
	}

	char* html = optval(argc, argv, "-html");
	if (strcmp(html, "")) {
		std::ifstream in(html);
		std::string contents((std::istreambuf_iterator<char>(in)),
			std::istreambuf_iterator<char>());
		writer.SetMsgHtmlText(contents.c_str());
	}

	char* scert = optval(argc, argv, "-scert");
	char* scertpass = optval(argc, argv, "-scertpass");
	if (strcmp(scert, "")) {
		cm.ImportFromFile(scert, scertpass);
		writer.SetSigningCertHandle(cm.GetCertHandle());
		writer.SetSecSettingsSign(true);
		writer.SetSecSettingsHashAlgorithm("SHA256");
	}

	char* ecert = optval(argc, argv, "-ecert");
	char* ecertpass = optval(argc, argv, "-ecertpass");
	if (strcmp(ecert, "")) {
		cm.ImportFromFile(ecert, ecertpass);
		writer.SetEncryptionCertCount(1);
		writer.SetEncryptionCertHandle(0, cm.GetCertHandle());
		writer.SetSecSettingsEncrypt(true);
		writer.SetSecSettingsEncryptionAlgorithm("AES128");
	}

	if (optext(argc, argv, "-hashalg")) {
		writer.SetSecSettingsHashAlgorithm(optval(argc, argv, "-hashalg"));
	}

	if (optext(argc, argv, "-encalg")) {
		writer.SetSecSettingsEncryptionAlgorithm(optval(argc, argv, "-encalg"));
	}

	if (optext(argc, argv, "-format")) {
		writer.SetSecSettingsSignatureFormat(atoi(optval(argc, argv, "-format")));
	}

	if (optext(argc, argv, "-a")) {
		writer.AttachFile(optval(argc, argv, "-a"));
	}

	if (writer.SaveToFile(argv[8]) == 0) {
		printf("A message has been assembled and saved successfully.\n\n");
	}
	else {
		printf("Failed to assemble and/or save a message.\n\n");
	}

err:
	if (writer.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", writer.GetLastErrorCode(), writer.GetLastError());
	}
done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


