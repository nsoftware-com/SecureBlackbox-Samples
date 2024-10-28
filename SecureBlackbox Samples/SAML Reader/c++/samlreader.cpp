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

class MySAMLReader : public SAMLReader {
	int FireEncrypted(SAMLReaderEncryptedEventParams *e) override {
		printf("The message is encrypted.\n");
		if (e->NeedCredential)
			printf("The decryption certificate is not available. If you did provide a decryption certificate, make sure it matches the private key used by the message creator.\n");
		else
			printf("The decryption certificate provided matches.\n");

		return 0;
	};

	int FireSignatureFound(SAMLReaderSignatureFoundEventParams *e) override {
		printf("A signature was found in the entity being processed.\n");
		if (e->Scope == 2) // sssAssertion
			printf("The signature covers an assertion.\n");
		else if (e->Scope == 1) // sssMessage
			printf("The signature covers the whole message.\n");
		else if (e->Scope == 3) // sssBinding
			printf("The signature was made over the binding.\n");
		else
			printf("The signature covers something else - this doesn't look right.\n");

		if (e->CertFound) {
			printf("The verification certificate is available, either by being included in the signature or provided by you.\n");
			e->Validate = true;
		}
		else
		{
			printf("The verification certificate was not found - will be unable to validate the signature.\n");
			e->Validate = false;
		}
		
		return 0;
	};

	int FireSignatureValidated(SAMLReaderSignatureValidatedEventParams *e) override {	
		switch (e->ValidationResult)
		{
		case SVT_VALID:
			printf("The signature has been validated. Validation result: VALID\n");
			break;
		case SVT_CORRUPTED:
			printf("The signature has been validated. Validation result: CORRUPTED\n");
			break;
		case SVT_SIGNER_NOT_FOUND:
			printf("The signature has been validated. Validation result: SIGNING CERTIFICATE NOT FOUND\n");
			break;
		case SVT_FAILURE:
			printf("The signature has been validated. Validation result: FAILED TO VALIDATE\n");
			break;
		case SVT_REFERENCE_CORRUPTED:
			printf("The signature has been validated. Validation result: REFERENCE CORRUPTED\n");
			break;
		default:
			printf("The signature has been validated. Validation result: UNKNOWN\n");
			break;
		} 
		
		return 0;
	};
};

void outputSubjectConfirmations(SAMLReader reader) {
	printf("  Subject confirmations:\n");
	for (int i = 0; i < reader.GetSubjectConfirmationCount(); i++) {
		printf("    Subject confirmation #%i:\n", (i + 1));
		printf("      Address: %s\n", reader.GetSubjectConfirmationAddress(i));
		printf("      Data: %s\n", reader.GetSubjectConfirmationData(i));
		printf("      Data type: %s\n", reader.GetSubjectConfirmationDataType(i));
		printf("      ID: %s\n", reader.GetSubjectConfirmationID(i));
		printf("      Method: %s\n", reader.GetSubjectConfirmationMethod(i));
		printf("      Not before: %s\n", reader.GetSubjectConfirmationNotBefore(i));
		printf("      Not after: %s\n", reader.GetSubjectConfirmationNotOnOrAfter(i));
	}
	printf("\n");
}

void outputConditions(SAMLReader reader) {
	printf("  Conditions:\n");

	for (int i = 0; i < reader.GetConditionCount(); i++) {
		printf("    Condition #%i:\n", (i + 1));

		switch (reader.GetConditionConditionType(i)) {
		case CSCT_AUDIENCE_RESTRICTION:
			printf("      Condition type: Audience restriction\n");
			break;
		case CSCT_ONE_TIME_USE:
			printf("      Condition type: One-time use\n");
			break;
		case CSCT_PROXY_RESTRICTION:
			printf("      Condition type: Proxy restriction\n");
			break;
		case CSCT_NOT_BEFORE:
			printf("      Condition type: Not before\n");
			break;
		case CSCT_NOT_ON_OR_AFTER:
			printf("      Condition type: Not after\n");
			break;
		}

		printf("      Condition: %s\n", reader.GetConditionCondition(i));
	}
	printf("\n");
}

void outputAttributes(SAMLReader reader) {
	printf("  Attributes:\n");
	for (int i = 0; i < reader.GetAttributeCount(); i++) {
		printf("    Attribute #%i:\n", (i + 1));
		printf("      Name: %s\n", reader.GetAttributeName(i));
		printf("      Name Format: %s\n", reader.GetAttributeNameFormat(i));
		printf("      Friendly Name:%s\n ", reader.GetAttributeFriendlyName(i));
		printf("      Statement Index: %i\n", reader.GetAttributeStatementIndex(i));
		printf("      Value(s): %s\n", reader.GetAttributeValues(i));
	}
	printf("\n");
}

void outputSubjectQuery(SAMLReader reader) {
	outputSubjectConfirmations(reader);
}

void outputAttributeQuery(SAMLReader reader) {
	outputAttributes(reader);
	outputSubjectConfirmations(reader);
}

void outputAuthnRequest(SAMLReader reader) {
	printf("  Assertion Consumer Service URL: %s\n", reader.GetAuthnRequestAssertionConsumerServiceURL());
	printf("  Protocol binding: %s\n", reader.GetAuthnRequestProtocolBinding());
	printf("  Provider name: %s\n", reader.GetAuthnRequestProviderName());
	printf("  NameID policy format: %s\n", reader.GetAuthnRequestNameIDPolicyFormat());
	printf("  Context class refs: %s\n", reader.GetAuthnRequestContextClassRefs());
	outputSubjectConfirmations(reader);
	outputConditions(reader);
}

void outputLogoutRequest(SAMLReader reader) {
	printf("  NameID: %s\n", reader.GetLogoutRequestNameID());
	printf("  Not on or after: %s\n", reader.GetLogoutRequestNotOnOrAfter());
	printf("  Reason: %s\n", reader.GetLogoutRequestReason());
	printf("  Session index(es): %s\n\n", reader.GetLogoutRequestSessionIndexes());
}

void outputStatements(SAMLReader reader) {
	printf("  Statements:\n");

	for (int i = 0; i < reader.GetStatementCount(); i++) {
		printf("    Statement #%i:\n", (i + 1));
		switch (reader.GetStatementStatementType(i)) {
		case CSAST_AUTHN:
			printf("      Type: AuthnStatement\n");
			printf("      Context class ref: %s\n", reader.GetStatementAuthnContextClassRef(i));
			printf("      Instant: %s\n", reader.GetStatementAuthnInstant(i));
			printf("      Session index: %s\n", reader.GetStatementAuthnSessionIndex(i));
			printf("      Not on or after: %s\n", reader.GetStatementAuthnSessionNotOnOrAfter(i));
			break;
		case CSAST_ATTRIBUTE:
			printf("      Type: AttributeStatement (see attributes below)\n");
			break;
		case CSAST_AUTHZ_DECISION:
			printf("      Type: AuthzDecisionStatement\n");
			printf("      Actions: %s\n", reader.GetStatementAuthzActions(i));
			printf("      Evidence: %s\n", reader.GetStatementAuthzDecisionEvidence(i));
			switch (reader.GetStatementAuthzDecision(i)) {
			case CSADN_PERMIT:
				printf("      Decision: Permit\n");
				break;
			case CSADN_DENY:
				printf("      Decision: Deny\n");
				break;

			default:
				printf("      Decision: Indeterminate\n");
				break;
			}
			break;
		default:
			printf("      Type: unknown\n");
			break;
		}
	}
	printf("\n");
}

void outputAssertion(SAMLReader reader) {
	// outputting currently selected ("pinned") assertion
	printf("  Issuer: %s\n", reader.GetPinnedAssertionIssuer());
	printf("  Assertion ID: %s\n", reader.GetPinnedAssertionID());
	printf("  Subject: %s\n\n", reader.GetPinnedAssertionSubject());

	outputStatements(reader);
	outputAttributes(reader);
	outputConditions(reader);
	outputSubjectConfirmations(reader);
}

void outputResponse(SAMLReader reader) {
	// outputting all assertions, one by one
	printf("  Assertions found: %i\n", reader.GetAssertionCount());
	
	for (int i = 0; i < reader.GetAssertionCount(); i++) {
		printf("    Assertion #%i\n", i);
		reader.PinAssertion(i); // selecting the assertion to process
		outputAssertion(reader);
	}
}

void displayHelp(const char* errMes) {
	printf(
		"NAME\n"
		"  samlreader -- SecureBlackbox SAMLReader Demo Application\n\n"
		"SYNOPSIS\n"
		"  samlreader <-input input_file>  [-vcert verify_certificate_file] [-vcertpass verify_certificate_password]\n"
		"             [-dcert decrypt_certificate_file] [-dcertpass decrypt_certificate_password]\n\n"
		"DESCRIPTION\n"
		"  This sample illustrates the use of SAMLReader to read and decompose SAML messages.\n\n"
		"  The options are as follows:\n\n"
		"  -input       An input file to read (Required).\n\n"
		"  -vcert       The certificate used to verify files.\n\n"
		"  -vcertpass   The password for the verifying certificate.\n\n"
		"  -dcert       The certificate used to decrypt files.\n\n"
		"  -dcertpass   The password for the decrypting certificate.\n\n"
		"EXAMPLES\n"
		"  samlreader -input C:\\mess.saml\n\n"
		"  samlreader -input C:\\mess.saml -vcert C:\\certs\\mycert.pfx -vcertpass mypassword\n\n"
	);

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
	MySAMLReader reader;

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

	if (optext(argc, argv, "-vcert")) {
		CertificateManager vcm;
		vcm.ImportFromFile(optval(argc, argv, "-vcert"), optval(argc, argv, "-vcertpass"));
		reader.SetCertCount(1);
		reader.SetCertHandle(0, vcm.GetCertHandle());
	}

	if (optext(argc, argv, "-dcert")) {
		CertificateManager dcm;
		dcm.ImportFromFile(optval(argc, argv, "-dcert"), optval(argc, argv, "-dcertpass"));
		reader.SetDecryptionCertificateHandle(dcm.GetCertHandle());
	}

	// opening the message
	printf("Starting message processing.\n\n");

	if (reader.OpenFile(input)) {
		goto err;
	}

	// outputting the details
	printf("Message loaded successfully.\n");
	printf("General details: \n");
	printf("  Destination: %s\n", reader.GetMessageDestination());
	printf("  Issue instant: %s\n", reader.GetMessageIssueInstant());
	printf("  ID: %s\n", reader.GetMessageID());
	printf("  Issuer: %s\n", reader.GetMessageIssuer());
	printf("  In response to: %s\n", reader.GetMessageInResponseTo());

	switch (reader.GetMessageContentType())
	{
	case CSTY_NONE:
		printf("Message type: Unknown\n\n");
		break;
	case CSTY_ASSERTION_IDREQUEST:
		printf("Message type: AssertionIDRequest\n\n");
		break;
	case CSTY_SUBJECT_QUERY:
		printf("Message type: SubjectQuery\n\n");
		outputSubjectQuery(reader);
		break;
	case CSTY_AUTHN_QUERY:
		printf("Message type: AuthnQuery\n\n");
		break;
	case CSTY_ATTRIBUTE_QUERY:
		printf("Message type: AttributeQuery\n\n");
		outputAttributeQuery(reader);
		break;
	case CSTY_AUTHZ_DECISION_QUERY:
		printf("Message type: AuthzDecisionQuery\n\n");
		break;
	case CSTY_AUTHN_REQUEST:
		printf("Message type: AuthnRequest\n\n");
		outputAuthnRequest(reader);
		break;
	case CSTY_MANAGE_NAME_IDREQUEST:
		printf("Message type: ManageNameIDRequest\n\n");
		break;
	case CSTY_LOGOUT_REQUEST:
		printf("Message type: LogoutRequest\n\n");
		outputLogoutRequest(reader);
		break;
	case CSTY_NAME_IDMAPPING_REQUEST:
		printf("Message type: NameIDMappingRequest\n\n");
		break;
	case CSTY_ARTIFACT_RESOLVE:
		printf("Message type: ArtifactResolve\n\n");
		break;
	case CSTY_RESPONSE:
		printf("Message type: Response\n\n");
		outputResponse(reader);
		break;
	default:
		break;
	}

	// global security props
	if (reader.GetMessageSigned() || reader.GetPinnedAssertionSigned()) {
		printf("The processed message was signed by its author.\n");
		if (reader.GetSigningCertHandle() != 0)
			printf("Signing certificate: %s\n", reader.GetSigningCertSubjectRDN());
		else
			printf("Signing certificate was not found\n");
		printf("Signature validation result: %i\n", reader.GetMessageSignatureValidationResult());
		printf("Digest method: %s\n", reader.GetSecurityDigestMethod());
	}

	if (((reader.GetMessageContentType() == CSTY_ASSERTION) || (reader.GetMessageContentType() == CSTY_RESPONSE)) &&
		(reader.GetPinnedAssertionAssertionType() == CSAT_ENCRYPTED_ASSERTION))
	{
		printf("The assertion was encrypted by its author.\n");
		printf("Encryption method: %s\n", reader.GetSecurityEncryptionMethod());
	}

err:
	if (reader.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", reader.GetLastErrorCode(), reader.GetLastError());
	}
done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


