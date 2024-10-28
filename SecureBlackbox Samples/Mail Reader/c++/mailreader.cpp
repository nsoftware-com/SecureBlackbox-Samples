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

namespace ArgParser {
	static char* optval(int argc, char** argv, const char* option) {
		for (int x = 0; x < argc - 1; x++) {
			if (!strcmp(argv[x], option)) {
				return argv[x + 1];
			}
		}
		return (char*)"";
	}
}

using namespace ArgParser;

class MyMailReader : public MailReader {
    int FireDecryptionInfoNeeded(MailReaderDecryptionInfoNeededEventParams* e) override {
        printf("************************************************************************\n");
        printf("Decryption needed! Please try again with correct path to certificate - the 2nd argument\n");
        printf("************************************************************************\n");
		
		printf("Press Enter to exit the demo.\n");
		getchar();
        exit(0);
    };
};

void displayHelp(const char* errMes) {
	printf(
		"NAME\n"
		"  mailreader -- SecureBlackbox MailReader Demo Application\n\n"
		"SYNOPSIS\n"
		"  mailreader <-input input_file> [-cert certificate_file] [-certpass certificate_password]\n\n"
		"DESCRIPTION\n"
		"  This sample shows how to parse an e-mail message, including signed  and/or encrypted messages.\n\n"
		"  The options are as follows:\n\n"
		"  -input        An input file to verify (Required).\n\n"
		"  -cert         The certificate used to decryption.\n\n"
		"  -certpass     The password for the certificate.\n\n"
		"EXAMPLES\n"
		"  mailreader -input C:\\mymail.eml -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
	);

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char* argv[]) {
	MyMailReader reader;
	CertificateManager cm;

	// Validate input
	if (argc < 2) {
		displayHelp("");
		goto done;
	}

	char* input = optval(argc, argv, "-input");
	if (!strcmp(input, "")) {
		displayHelp("-input is required.");
		goto done;
	}

	// Additional options
	char* cert = optval(argc, argv, "-cert");
	char* certpass = optval(argc, argv, "-certpass");
	if (strcmp(cert, "")) {
		cm.ImportFromFile(cert, certpass);
		reader.SetDecryptionCertHandle(cm.GetCertHandle());
	}

	if (reader.LoadFromFile(input)) {
		goto err;
	};

	printf("\nSender = %s\n", reader.GetMsgSender());
	printf("From = %s\n", reader.GetMsgFrom());
	printf("SendTo = %s\n", reader.GetMsgSendTo());
	printf("CC = %s\n", reader.GetMsgCc());
	printf("BCC = %s\n", reader.GetMsgBcc());
	printf("\n\t\t***Security Info***\n");
	printf("Encrypted = %s\n", reader.GetSecInfoEncrypted() ? "true" : "false");
	if (reader.GetSecInfoEncrypted()) {
		printf("Decryption Certificate = ");
		if (reader.GetDecryptionCertHandle() == NULL) {
			printf("[certificate not provided]\n");
		}
		else {
			printf("%s\n", reader.GetDecryptionCertSubjectRDN());
		}
		printf("Encryption algorithm = %s\n", reader.GetSecInfoEncryptionAlgorithm());

	}
	printf("Signed = %s\n", reader.GetSecInfoSigned() ? "true" : "false");
	if (reader.GetSecInfoSigned()) {
		printf("Signing Certificate = ");
		if ( reader.GetSigningCertHandle() == NULL) {
			printf("[certificate not found]\n");
		}
		else {
			printf("%s\n", reader.GetSigningCertSubjectRDN());
		}
		printf("Signature validation = ");
		switch (reader.GetSecInfoSignatureValidationResult()) {
		case 0:
			printf("VALID\n");
			break;

		case 1:
			printf("UNKNOWN\n");
			break;

		case 2:
			printf("CORRUPTED\n");
			break;

		case 3:
			printf("SIGNER NOT FOUND\n");
			break;

		case 4:
			printf("FAILURE\n");
			break;

		default:
			goto done;
			break;
		}

		printf("Hash algorithm = %s\n", reader.GetSecInfoHashAlgorithm());
	}
	printf("\t***End Security Info Block***\n\n");

	printf("Subject = %s\n", reader.GetMsgSubject());
	printf("Priority = ");
	switch (reader.GetMsgPriority()) {
	case 0:
		printf("LOWEST\n");
		break;

	case 1:
		printf("LOW\n");
		break;

	case 2:
		printf("NORMAL\n");
		break;

	case 3:
		printf("HIGH\n");
		break;

	case 4:
		printf("HIGHEST\n");
		break;

	default:
		goto done;
		break;
	}
	printf("Delivery receipt = %s\n", reader.GetMsgDeliveryReceipt() ? "true" : "false");
	printf("Read receipt = %s\n", reader.GetMsgReadReceipt() ? "true" : "false");
	printf("\n\t\t***Plain Text***\n");
	printf("%s\n", reader.GetMsgPlainText());
	printf("\t\t***Html Text***\n");
	printf("%s\n", reader.GetMsgHtmlText());
	printf("\nAttachments:\n");
	for (int i = 0; i < reader.GetMsgAttachmentCount(); i++) {
		printf("%s\t\tSize = %lld\n", reader.GetAttachFileName(i), reader.GetAttachSize(i));
	}
	printf("\n**************************END**************************\n\n");

err:
	if (reader.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", reader.GetLastErrorCode(), reader.GetLastError());
	}
done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


