# 
# SecureBlackbox 2024 Python Edition - Sample Project
# 
# This sample project demonstrates the usage of SecureBlackbox in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
# 
# www.nsoftware.com/secureblackbox
# 
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
# 

import sys
import string
from secureblackbox import *

input = sys.hexversion<0x03000000 and raw_input or input

def ensureArg(args, prompt, index):
  if len(args) <= index:
    while len(args) <= index:
      args.append(None)
    args[index] = input(prompt)
  elif args[index] is None:
    args[index] = input(prompt)


def displayHelp(errMes):
	print(
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
	)
	if (errMes != ""):
		print("Error: %s\n\n"%errMes)

def fireEncrypted(e):
	print("The message is encrypted.\n")
	if (e.need_credential):
		print("The decryption certificate is not available. If you did provide a decryption certificate, make sure it matches the private key used by the message creator.\n")
	else:
		print("The decryption certificate provided matches.\n")

def fireSignatureFound(e):
	print("A signature was found in the entity being processed.\n")
	if (e.scope == 2): # sssAssertion
		print("The signature covers an assertion.\n")
	elif (e.scope == 1): # sssMessage
		print("The signature covers the whole message.\n")
	elif (e.scope == 3): # sssBinding
		print("The signature was made over the binding.\n")
	else:
		print("The signature covers something else - this doesn't look right.\n")

	if (e.cert_found):
		print("The verification certificate is available, either by being included in the signature or provided by you.\n")
		e.validate = True
	else:
		print("The verification certificate was not found - will be unable to validate the signature.\n")
		e.validate = False;
		
def fireSignatureValidated(e):
	if (value == svtValid):
		print("The signature has been validated. Validation result: VALID\n")
	elif (value == svtCorrupted):
		print("The signature has been validated. Validation result: CORRUPTED\n")
	elif (value == svtSignerNotFound):
		print("The signature has been validated. Validation result: SIGNING CERTIFICATE NOT FOUND\n")
	elif (value == svtFailure):
		print("The signature has been validated. Validation result: FAILED TO VALIDATE\n")
	elif (value == svtReferenceCorrupted):
		print("The signature has been validated. Validation result: REFERENCE CORRUPTED\n")
	else:
		print("The signature has been validated. Validation result: UNKNOWN\n")  

def outputSubjectConfirmations(reader):
	print("  Subject confirmations:\n")
	for i in range(reader.subject_confirmation_count):
		print("    Subject confirmation #%i:\n"%(i + 1))
		print("      Address: %s\n"%(reader.get_subject_confirmation_address(i)))
		print("      Data: %s\n"%(reader.get_subject_confirmation_data(i)))
		print("      Data type: %s\n"%(reader.get_subject_confirmation_data_type(i)))
		print("      ID: %s\n"%(reader.get_subject_confirmation_id(i)))
		print("      Method: %s\n"%(reader.get_subject_confirmation_method(i)))
		print("      Not before: %s\n"%(reader.get_subject_confirmation_not_before(i)))
		print("      Not after: %s\n"%(reader.get_subject_confirmation_not_on_or_after(i)))

	print("\n")

def outputConditions(reader):
	print("  Conditions:\n")
	for i in range(reader.condition_count):
		print("    Condition #%i:\n", (i + 1));

		if (reader.get_condition_condition_type(i) == 0): # csctAudienceRestrction
			print("      Condition type: Audience restriction\n")
		elif (reader.get_condition_condition_type(i) == 1): # csctOneTimeUse
			print("      Condition type: One-time use\n")
		elif (reader.get_condition_condition_type(i) == 2): # csctProxyRestrction
			print("      Condition type: Proxy restriction\n")
		elif (reader.get_condition_condition_type(i) == 3): # csctNotBefore
			print("      Condition type: Not before\n")
		elif (reader.get_condition_condition_type(i) == 4): # csctNotOnOrAfter
			print("      Condition type: Not after\n")

		print("      Condition: %s\n", reader.GetConditionCondition(i));

	print("\n")

def outputAttributes(reader):
	print("  Attributes:\n")
	for i in range(reader.attribute_count):
		print("    Attribute #%i:\n"%(i + 1))
		print("      Name: %s\n"%(reader.get_attribute_name(i)))
		print("      Name Format: %s\n"%(reader.get_attribute_name_format(i)))
		print("      Friendly Name:%s\n "%(reader.get_attribute_friendly_name(i)))
		print("      Statement Index: %i\n"%(reader.get_attribute_statement_index(i)))
		print("      Value(s): %s\n"%(reader.get_attribute_values(i)))

	print("\n")

def outputSubjectQuery(reader):
	outputSubjectConfirmations(reader)

def outputAttributeQuery(reader):
	outputAttributes(reader)
	outputSubjectConfirmations(reader)

def outputAuthnRequest(reader):
	print("  Assertion Consumer Service URL: %s\n"%(reader.get_authn_request_assertion_consumer_service_url()))
	print("  Protocol binding: %s\n"%(reader.get_authn_request_protocol_binding()))
	print("  Provider name: %s\n"%(reader.get_authn_request_provider_name()))
	print("  NameID policy format: %s\n"%(reader.get_authn_request_name_id_policy_format()))
	print("  Context class refs: %s\n"%(reader.get_authn_request_context_class_refs()))
	outputSubjectConfirmations(reader)
	outputConditions(reader)

def outputLogoutRequest(reader):
	print("  NameID: %s\n"%(reader.get_logout_request_name_id()))
	print("  Not on or after: %s\n"%(reader.get_logout_request_not_on_or_after()))
	print("  Reason: %s\n"%(reader.get_logout_request_reason()))
	print("  Session index(es): %s\n\n"%(reader.get_logout_request_session_indexes()))

def outputStatements(reader):
	printf("  Statements:\n")
	for i in range(reader.statement_count):
		print("    Statement #%i:\n"%(i + 1))
		if (reader.get_statement_statement_type(i) == 0): # csastAuthn
			print("      Type: AuthnStatement\n")
			print("      Context class ref: %s\n"%(reader.get_statement_authn_context_class_ref(i)))
			print("      Instant: %s\n"%(reader.get_statement_authn_instant(i)))
			print("      Session index: %s\n"%(reader.get_statement_authn_session_index(i)))
			print("      Not on or after: %s\n"%(reader.get_statement_authn_session_not_on_or_after(i)))
		elif (reader.get_statement_statement_type(i) == 1): # csastAttribute
			print("      Type: AttributeStatement (see attributes below)\n")
		elif (reader.get_statement_statement_type(i) == 2): # csastAuthzDecision
			print("      Type: AuthzDecisionStatement\n")
			print("      Actions: %s\n"%(reader.get_statement_authz_actions(i)))
			print("      Evidence: %s\n"%(reader.get_statement_authz_decision_evidence(i)))
			
			if (reader.get_statement_authz_decision(i) == 0): # csadnPermit
				print("      Decision: Permit\n")
			if (reader.get_statement_authz_decision(i) == 1): # csadnDeny
				print("      Decision: Deny\n")
			else:
				print("      Decision: Indeterminate\n")
		else:
			print("      Type: unknown\n")

	print("\n")

def outputAssertion(reader):
	# outputting currently selected ("pinned") assertion
	print("  Issuer: %s\n"%(reader.pinned_assertion_issuer))
	print("  Assertion ID: %s\n"%(reader.pinned_assertion_id))
	print("  Subject: %s\n\n"%(reader.pinned_assertion_subject))

	outputStatements(reader)
	outputAttributes(reader)
	outputConditions(reader)
	outputSubjectConfirmations(reader)

def outputResponse(reader):
	# outputting all assertions, one by one
	print("  Assertions found: %i\n"%(reader.assertion_count))
	for i in range(reader.assertion_count):
		print("    Assertion #%i\n"%(i))
		reader.pin_assertion(i) # selecting the assertion to process
		outputAssertion(reader)


if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    cm = CertificateManager()
    reader = SAMLReader()
    reader.on_encrypted = fireEncrypted
    reader.on_signature_found = fireSignatureFound
    reader.on_signature_validated = fireSignatureValidated
    
    inputF = ""
    vcert = ""
    vcertpass = ""
    dcert = ""
    dcertpass = ""

    try:
        for x in range(len(sys.argv)):
            if (sys.argv[x].startswith("-")):
                if (sys.argv[x].lower() == "-input"):
                    inputF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-vcert"):
                    vcert = sys.argv[x+1]
                if (sys.argv[x].lower() == "-vcertpass"):
                    vcertpass = sys.argv[x+1]
                if (sys.argv[x].lower() == "-dcert"):
                    dcert = sys.argv[x+1]
                if (sys.argv[x].lower() == "-dcertpass"):
                    dcertpass = sys.argv[x+1]
    
        if (inputF == ""):
            displayHelp("-input is required.")
            sys.exit(1)

        if (vcert != ""):
            cm.import_from_file(vcert, vcertpass)
            reader.set_cert_count(1)
            reader.set_cert_handle(0, cm.cert_handle)

        if (dcert != ""):
            cm.import_from_file(dcert, dcertpass)
            reader.decryption_certificate_handle = cm.cert_handle

        # opening the message
        print("Starting message processing.\n\n")

        reader.open_file(inputF)

        # outputting the details
        print("Message loaded successfully.\n")
        print("General details: \n")
        print("  Destination: %s\n"%(reader.message_destination))
        print("  Issue instant: %s\n"%(reader.message_issue_instant))
        print("  ID: %s\n"%(reader.message_id))
        print("  Issuer: %s\n"%(reader.message_issuer))
        print("  In response to: %s\n"%(reader.message_in_response_to))

        if (reader.message_content_type == 0): # cstyNone
            print("Message type: Unknown\n\n")
        elif (reader.message_content_type == 1): # cstyAssertionIdrequest
            print("Message type: AssertionIDRequest\n\n")
        elif (reader.message_content_type == 2): # cstySubjectQuery
            print("Message type: SubjectQuery\n\n")
            outputSubjectQuery(reader)
        elif (reader.message_content_type == 3): # cstyAuthQuery
            print("Message type: AuthnQuery\n\n")
        elif (reader.message_content_type == 4): # cstyAttributeQuery
            print("Message type: AttributeQuery\n\n")
            outputAttributeQuery(reader)
        elif (reader.message_content_type == 5): # cstyAuthDecisionQuery
            print("Message type: AuthzDecisionQuery\n\n")
        elif (reader.message_content_type == 6): # cstyAuthRequest
            print("Message type: AuthnRequest\n\n")
            outputAuthnRequest(reader)
        elif (reader.message_content_type == 7): # cstyNameIdrequest
            print("Message type: ManageNameIDRequest\n\n")
        elif (reader.message_content_type == 8): # cstyLogoutRequest
            print("Message type: LogoutRequest\n\n")
            outputLogoutRequest(reader)
        elif (reader.message_content_type == 9): # cstyIdmappingRequest
            print("Message type: NameIDMappingRequest\n\n")
        elif (reader.message_content_type == 10): # cstyArtifactResolve
            print("Message type: ArtifactResolve\n\n")
        elif (reader.message_content_type == 11): # cstyResponse
            print("Message type: Response\n\n")

        # global security props
        if (reader.message_signed or reader.pinned_assertion_signed):
            print("The processed message was signed by its author.\n")
            if (reader.signing_cert_handle != 0):
                print("Signing certificate: %s\n"%(reader.signing_cert_subject_rdn))
            else:
                print("Signing certificate was not found\n")
            print("Signature validation result: %i\n"%(reader.message_signature_validation_result))
            print("Digest method: %s\n"%(reader.security_digest_method))

        if (((reader.message_content_type == 12) or (reader.message_content_type == 11)) and (reader.pinned_assertion_assertion_type == 3)):
            print("The assertion was encrypted by its author.\n")
            print("Encryption method: %s\n"%(reader.security_encryption_method))

    except Exception as e: 
        print(e)



