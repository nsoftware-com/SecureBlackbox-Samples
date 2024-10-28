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
#include <stdlib.h>
#include <string.h>
#include <ctime>
#include <string>
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
		"  samlwriter -- SecureBlackbox SAMLWriter Demo Application\n\n"
		"SYNOPSIS\n"
		"  samlwriter <-type message_type> <output output_file> [-issuer issuer_url] [-dest destination_url] [-service service_url]\n"
		"             [-scert sign_certificate_file] [-scertpass sign_certificate_password] [-hashalg hashalg]\n"
		"             [-ecert encrypt_certificate_file] [-ecertpass encrypt_certificate_password]\n\n"
		"DESCRIPTION\n"
		"  This sample illustrates how to use SAMLWriter to create SAML messages.\n\n"
		"  The options are as follows:\n\n"
		"  -type        The type of SAML message (Required). Enter the corresponding number. Valid values:\n\n"
		"                  0  - AuthnRequest\n"
		"                  1  - LogoutRequest\n"
		"                  2  - AttributeQuery\n"
		"                  3  - SubjectQuery\n"
		"                  4  - Assertion\n"
		"                  5  - Enveloped Assertion\n\n"
		"  -output      Where the SAML message file will be saved (Required).\n\n"
		"  -issuer      The issuer URL.\n\n"
		"  -dest        The destination URL.\n\n"
		"  -service     The service URL.\n\n"
		"  -scert       The certificate used to sign files.\n\n"
		"  -scertpass   The password for the signing certificate.\n\n"
		"  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224, MD5\n\n"
		"  -ecert       The certificate used to encrypt files.\n\n"
		"  -ecertpass   The password for the encrypting certificate.\n\n"
		"EXAMPLES\n"
		"  samlwriter -type 0 -output C:\\mess.saml -ecert C:\\certs\\mycert.pfx -ecertpass mypassword\n\n"
		"  samlwriter -type 4 -output C:\\mess.saml -scert C:\\certs\\mycert.pfx -scertpass mypassword -hashalg SHA256 \n"
		"           -issuer http://saml.localservice.com/metadata/ -dest http://saml.remoteservice.com/sso -service http://saml.localservice.com/acs\n\n"
	);

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

SAMLWriter writer;

char* issuer = "";
char* dest = "";
char* service = "";
char* scert = "";
char* scertpass = "";
char* ecert = "";
char* ecertpass = "";
char* hashalg = "SHA256";
char* output = "";

const std::string currentDateTime(int num) {
	time_t     now = time(0);
	struct tm  tstruct;
	char       buf[80];
	tstruct = *localtime(&now);
	tstruct.tm_year = tstruct.tm_year + num;

	strftime(buf, sizeof(buf), "%Y-%m-%d.%X", &tstruct);

	return buf;
}

void applySecurity() {
	if (strcmp(scert, "")) {
		CertificateManager scm;
		scm.ImportFromFile(scert, scertpass);

		writer.SetMessageSigned(true);
		writer.SetSigningCertHandle(scm.GetCertHandle());
		writer.SetSecurityDigestMethod(hashalg);
	}

	if (strcmp(ecert, "")) {
		CertificateManager ecm;
		ecm.ImportFromFile(ecert, ecertpass);

		writer.SetEncryptionCertHandle(ecm.GetCertHandle());
		writer.SetSecurityEncryptionMethod("AES128");
		writer.SetAssertionAssertionType(CSAT_ENCRYPTED_ASSERTION);
	}
}

void createAuthnRequest() {
	// creating a message of AuthnRequest type
	writer.CreateNew(CSTY_AUTHN_REQUEST);

	// main message properties
	writer.SetMessageID("my-message-id-123");
	writer.SetMessageIssueInstant(currentDateTime(0).c_str());
	writer.SetMessageDestination(dest);
	writer.SetMessageIssuer(writer.FormatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

	// adding a subject confirmation
	writer.SetMessageSubject(writer.GetMessageIssuer());
	writer.AddSubjectConfirmation("scmethod", "http://scaddress.com", "screcipient", "",
		currentDateTime(-1).c_str(), currentDateTime(1).c_str(), "", "sctype", "scdata");

	// adding a couple of conditions
	writer.AddCondition(CSCT_AUDIENCE_RESTRICTION, "PSCU:saml20:dev");
	writer.AddCondition(CSCT_NOT_BEFORE, currentDateTime(0).c_str());

	// setting up authnrequest parameters
	writer.SetAuthnRequestAssertionConsumerServiceIndex(0);
	writer.SetAuthnRequestAssertionConsumerServiceURL(service);
	writer.SetAuthnRequestProtocolBinding("urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST");
	writer.SetAuthnRequestProviderName("My Application");
	writer.SetAuthnRequestNameIDPolicyFormat("urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress");
	writer.SetAuthnRequestNameIDPolicyAllowCreate(true);
	writer.SetAuthnRequestContextComparison(CACCT_EXACT);
	writer.SetAuthnRequestContextClassRefs("urn:oasis:names:tc:SAML:2.0:ac:classes:PasswordProtectedTransport");
	writer.SetAuthnRequestContextRefType(CACRT_CLASS);

	// applying security
	applySecurity();

	// Saving the output
	writer.SaveFile(output);
}

void createLogoutRequest() {
	// creating a message of LogoutRequest type
	writer.CreateNew(CSTY_LOGOUT_REQUEST);

	// main message properties
	writer.SetMessageID("my-message-id-123");
	writer.SetMessageIssueInstant(currentDateTime(0).c_str());
	writer.SetMessageDestination(dest);
	writer.SetMessageIssuer(writer.FormatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

	// setting up logoutrequest parameters
	writer.SetLogoutRequestNameID(writer.FormatID("id-abcdefghijkl", "", "urn:oasis:names:tc:SAML:2.0:nameid-format:transient", "", "", ""));
	writer.SetLogoutRequestNotOnOrAfter(currentDateTime(0).c_str());
	writer.SetLogoutRequestReason("Requested by user");
	writer.SetLogoutRequestSessionIndexes("id-01234567890");

	// applying security
	applySecurity();

	// Saving the output
	writer.SaveFile(output);
}

void createAttributeQuery() {
	// creating a message of AttributeQuery type
	writer.CreateNew(CSTY_ATTRIBUTE_QUERY);

	// main message properties
	writer.SetMessageID("my-message-id-123");
	writer.SetMessageIssueInstant(currentDateTime(0).c_str());
	writer.SetMessageDestination(dest);
	writer.SetMessageIssuer(writer.FormatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

	// setting up attributequery parameters: a couple of attributes we want
	writer.AddAttribute("clientID", "JohnDoe123", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
		"xs:string", 0);
	writer.AddAttribute("clientCategorty", "Business", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
		"xs:string", 0);

	// ... and a subject confirmation
	writer.SetMessageSubject(writer.GetMessageIssuer());
	writer.AddSubjectConfirmation("scmethod", "http://scaddress.com", "screcipient", "",
		currentDateTime(-1).c_str(), currentDateTime(1).c_str(), "", "sctype", "scdata");

	// applying security
	applySecurity();

	// Saving the output
	writer.SaveFile(output);
}

void createSubjectQuery() {
	// creating a message of SubjectQuery type
	writer.CreateNew(CSTY_SUBJECT_QUERY);

	// main message properties
	writer.SetMessageID("my-message-id-123");
	writer.SetMessageIssueInstant(currentDateTime(0).c_str());
	writer.SetMessageDestination(dest);
	writer.SetMessageIssuer(writer.FormatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

	// setting up subjectquery parameters: a couple of subject confirmations
	writer.SetMessageSubject(writer.GetMessageIssuer());
	writer.AddSubjectConfirmation("scmethod1", "http://scaddress.com", "screcipient", "",
		currentDateTime(-1).c_str(), currentDateTime(1).c_str(), "", "sctype", "scdata");
	writer.AddSubjectConfirmation("scmethod2", "Sandford, Gloucestershire", "screcipient", "",
		currentDateTime(-1).c_str(), currentDateTime(3).c_str(), "", "sctype", "scdata");

	// applying security
	applySecurity();

	// Saving the output
	writer.SaveFile(output);
}

void createAssertion() {
	// creating a message of Assertion type
	writer.CreateNew(CSTY_ASSERTION);

	// main message properties
	writer.SetMessageID("my-message-id-123");
	writer.SetMessageIssueInstant(currentDateTime(0).c_str());
	writer.SetMessageDestination(dest);
	writer.SetMessageIssuer(writer.FormatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

	// a message may contain multiple complex assertions, each of which
	// should be enveloped into BeginAssertion/CompleteAssertion calls.
	writer.BeginAssertion();
	writer.SetAssertionIssuer(writer.GetMessageIssuer()); // keeping it simple
	writer.SetAssertionAssertionType(CSAT_ASSERTION);

	// An assertion may contain multiple attributes within multiple statements.
	// If we add an attribute without adding a statement first, a new statement
	// will be created automatically, and the attribute added to it.
	// For example, the below two attributes go to the first statement,
	// which is created automatically:
	writer.AddAttribute("clientID", "JohnDoe123", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
		"xs:string", 0);
	writer.AddAttribute("clientCategorty", "Business", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
		"xs:string", 0);

	// Let"s add another statement
	writer.AddAttributeStatement(); // returns the index, which is going to be 1

									// Adding one attribute with two values - these are going to be merged
									// because their names are identical.
	writer.AddAttribute("eduPersonAffiliation", "users", "", "", 1);
	writer.AddAttribute("eduPersonAffiliation", "examplerole1", "", "", 1);

	// adding an authentication statement
	writer.AddAuthnStatement(currentDateTime(0).c_str(), "id-1234567890",
		currentDateTime(1).c_str(), "urn:oasis:names:tc:SAML:2.0:ac:classes:Password");

	// adding an authorization statement
	writer.AddAuthzDecisionStatement(CSADN_PERMIT, "Evidence", "Resource", "namespace=ns1;value=value1");

	// adding assertion conditions (audience and time scope)
	writer.AddCondition(CSCT_AUDIENCE_RESTRICTION, "PSCU:saml20:dev");
	writer.AddCondition(CSCT_NOT_ON_OR_AFTER, currentDateTime(1).c_str());

	// setting custom assertion ID (optional)
	writer.SetAssertionID("unique-id-123456");

	// adding subject confirmations
	writer.SetAssertionSubject(writer.FormatID("Subject", "Subject", "", "", "", ""));
	writer.AddSubjectConfirmation("scmethod1", "http://scaddress.com", "screcipient", "",
		currentDateTime(-1).c_str(), currentDateTime(1).c_str(), "", "sctype", "scdata");

	// applying security: as it applies to the assertion, it should precede
	// the CompleteAssertion() call
	applySecurity();

	// adding ("committing") the formed assertion to the SAML message
	writer.CompleteAssertion();

	// Saving the output
	writer.SaveFile(output);
}

void createEnvelopedAssertion() {
	// creating a message of Response type
	writer.CreateNew(CSTY_RESPONSE);

	// main message properties
	writer.SetMessageID("my-message-id-123");
	writer.SetMessageIssueInstant(currentDateTime(0).c_str());
	writer.SetMessageDestination(dest);
	writer.SetMessageIssuer(writer.FormatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

	// a message may contain multiple complex assertions, each of which
	// should be enveloped into BeginAssertion/CompleteAssertion calls.
	writer.BeginAssertion();
	writer.SetAssertionIssuer(writer.GetMessageIssuer()); // keeping it simple
	writer.SetAssertionAssertionType(CSAT_ASSERTION);

	// An assertion may contain multiple attributes within multiple statements.
	// If we add an attribute without adding a statement first, a new statement
	// will be created automatically, and the attribute added to it.
	// For example, the below two attributes go to the first statement,
	// which is created automatically:
	writer.AddAttribute("clientID", "JohnDoe123", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
		"xs:string", 0);
	writer.AddAttribute("clientCategorty", "Business", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
		"xs:string", 0);

	// adding an authentication statement
	writer.AddAuthnStatement(currentDateTime(0).c_str(), "id-1234567890",
		currentDateTime(1).c_str(), "urn:oasis:names:tc:SAML:2.0:ac:classes:Password");

	// adding assertion conditions (audience and time scope)
	writer.AddCondition(CSCT_AUDIENCE_RESTRICTION, "PSCU:saml20:dev");
	writer.AddCondition(CSCT_NOT_ON_OR_AFTER, currentDateTime(1).c_str());

	// setting custom assertion ID (optional)
	writer.SetAssertionID("unique-id-123456");

	// adding subject confirmations
	writer.SetAssertionSubject(writer.FormatID("Subject", "Subject", "", "", "", ""));
	writer.AddSubjectConfirmation("scmethod1", "http://scaddress.com", "screcipient", "",
		currentDateTime(-1).c_str(), currentDateTime(1).c_str(), "", "sctype", "scdata");

	// applying security: as it applies to the assertion, it should precede
	// the CompleteAssertion() call
	applySecurity();

	// adding ("committing") the formed assertion to the SAML message
	writer.CompleteAssertion();

	// Saving the output
	writer.SaveFile(output);
}

int main(int argc, char** argv) {
	if (argc < 2) {
		displayHelp("");
		goto done;
	}

	int type = -1;
	if (optext(argc, argv, "-type")) {
		type = atoi(optval(argc, argv, "-type"));
	}
	else {
		displayHelp("-type is required.");
		goto done;
	}

	output = optval(argc, argv, "-output");
	if (!strcmp(output, "")) {
		displayHelp("-output is required.");
		goto done;
	}

	issuer = optval(argc, argv, "-issuer");

	dest = optval(argc, argv, "-dest");

	service = optval(argc, argv, "-service");

	scert = optval(argc, argv, "-scert");
	scertpass = optval(argc, argv, "-scertpass");

	ecert = optval(argc, argv, "-ecert");
	ecertpass = optval(argc, argv, "-ecertpass");
	
	if (optext(argc, argv, "-hashalg")) {
		hashalg = optval(argc, argv, "-hashalg");
	}

	switch (type) {
	case 0:
		createAuthnRequest();
		break;
	case 1:
		createLogoutRequest();
		break;
	case 2:
		createAttributeQuery();
		break;
	case 3:
		createSubjectQuery();
		break;
	case 4:
		createAssertion();
		break;
	case 5:
		createEnvelopedAssertion();
		break;
	default:
		displayHelp("Invalid type value.");
		goto done;
	}

    if (writer.GetLastErrorCode()) {
		goto err;
    } else {
        printf("The SAML message was created successfully.\n\n");
    };

err:
	if (writer.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", writer.GetLastErrorCode(), writer.GetLastError());
	}

done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


