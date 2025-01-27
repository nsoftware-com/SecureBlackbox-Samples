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

input = sys.hexversion < 0x03000000 and raw_input or input


def ensureArg(args, prompt, index):
    if len(args) <= index:
        while len(args) <= index:
            args.append(None)
        args[index] = input(prompt)
    elif args[index] is None:
        args[index] = input(prompt)


from datetime import datetime
from dateutil.relativedelta import relativedelta

def displayHelp(errMes):
    print(
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
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
writer = SAMLWriter()

issuer = ""
dest = ""
service = ""
scert = ""
scertpass = ""
ecert = ""
ecertpass = ""
hashalg = "SHA256"
output = ""


def currentDateTime(num):
    now = datetime.now()
    now = now + relativedelta(years=num)
    utils = Utils()
    return utils.date_to_string(now)
    
def applySecurity():
    if (scert != ""):
        scm = CertificateManager()
        scm.import_from_file(scert, scertpass)
        writer.message_signed = True
        writer.signing_cert_handle = scm.cert_handle
        writer.security_digest_method = hashalg

    if (ecert != ""):
        ecm = CertificateManager()
        scm.import_from_file(ecert, ecertpass)
        writer.encryption_cert_handle = ecm.cert_handle
        writer.security_encryption_method = "AES128"
        writer.assertion_assertion_type = 3

def createAuthnRequest():
    # creating a message of AuthnRequest type
    writer.create_new(6);

    # main message properties
    writer.message_id = "my-message-id-123"
    writer.message_issue_instant = currentDateTime(0)
    writer.message_destination = dest
    writer.message_issuer = writer.format_id(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", "")

    # adding a subject confirmation
    writer.message_subject = writer.message_issuer
    writer.add_subject_confirmation("scmethod", "http://scaddress.com", "screcipient", "", currentDateTime(-1), currentDateTime(1), "", "sctype", "scdata")

    # adding a couple of conditions
    writer.add_condition(0, "PSCU:saml20:dev")
    writer.add_condition(3, currentDateTime(0))

    # setting up authnrequest parameters
    writer.authn_request_assertion_consumer_service_index = 0
    writer.authn_request_assertion_consumer_service_url = service
    writer.authn_request_protocol_binding = "urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST"
    writer.authn_request_provider_name = "My Application"
    writer.authn_request_name_id_policy_format = "urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress"
    writer.authn_request_name_id_policy_allow_create = True
    writer.authn_request_context_comparison = 1
    writer.authn_request_context_class_refs = "urn:oasis:names:tc:SAML:2.0:ac:classes:PasswordProtectedTransport"
    writer.authn_request_context_ref_type = 1

    # applying security
    applySecurity()

    # Saving the output
    writer.save()
    writer.save_file(output)

def createLogoutRequest():
	# creating a message of LogoutRequest type
	writer.create_new(8)

	# main message properties
	writer.message_id = "my-message-id-123"
	writer.message_issue_instant = currentDateTime(0)
	writer.message_destination = dest
	writer.message_issuer = writer.format_id(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", "")

	# setting up logoutrequest parameters
	writer.logout_request_name_id = writer.format_id("id-abcdefghijkl", "", "urn:oasis:names:tc:SAML:2.0:nameid-format:transient", "", "", "")
	writer.logout_request_not_on_or_after = currentDateTime(0)
	writer.logout_request_reason = "Requested by user"
	writer.logout_request_session_indexes = "id-01234567890"

	# applying security
	applySecurity()

	# Saving the output
	writer.save_file(output)

def createAttributeQuery():
	# creating a message of AttributeQuery type
	writer.create_new(4)

	# main message properties
	writer.message_id = "my-message-id-123"
	writer.message_issue_instant = currentDateTime(0)
	writer.message_destination = dest
	writer.message_issuer = writer.format_id(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", "")

	# setting up attributequery parameters: a couple of attributes we want
	writer.add_attribute("clientID", "JohnDoe123", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic", "xs:string", 0)
	writer.add_attribute("clientCategorty", "Business", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic", "xs:string", 0)

	# ... and a subject confirmation
	writer.message_subject = writer.message_issuer
	writer.add_subject_confirmation("scmethod", "http://scaddress.com", "screcipient", "", currentDateTime(-1), currentDateTime(1), "", "sctype", "scdata")

	# applying security
	applySecurity()

	# Saving the output
	writer.save()
	writer.save_file(output)

def createSubjectQuery():
	# creating a message of SubjectQuery type
	writer.create_new(2)

	# main message properties
	writer.message_id = "my-message-id-123"
	writer.message_issue_instant = currentDateTime(0)
	writer.message_destination = dest
	writer.message_issuer = writer.format_id(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", "")

	# setting up subjectquery parameters: a couple of subject confirmations
	writer.message_subject = writer.message_issuer
	writer.add_subject_confirmation("scmethod1", "http://scaddress.com", "screcipient", "", currentDateTime(-1), currentDateTime(1), "", "sctype", "scdata")
	writer.add_subject_confirmation("scmethod2", "Sandford, Gloucestershire", "screcipient", "", currentDateTime(-1), currentDateTime(3), "", "sctype", "scdata")

	# applying security
	applySecurity()

	# Saving the output
	writer.save()
	writer.save_file(output)

def createAssertion():
	# creating a message of Assertion type
	writer.create_new(12)

	# main message properties
	writer.message_id = "my-message-id-123"
	writer.message_issue_instant = currentDateTime(0)
	writer.message_destination = dest
	writer.message_issuer = writer.format_id(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", "")

	# a message may contain multiple complex assertions, each of which
	# should be enveloped into BeginAssertion/CompleteAssertion calls.
	writer.begin_assertion()
	writer.assertion_issuer = writer.message_issuer # keeping it simple
	writer.assertion_assertion_type = 2 # CSAT_ASSERTION

	# An assertion may contain multiple attributes within multiple statements.
	# If we add an attribute without adding a statement first, a new statement
	# will be created automatically, and the attribute added to it.
	# For example, the below two attributes go to the first statement,
	# which is created automatically:
	writer.add_attribute("clientID", "JohnDoe123", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic", "xs:string", 0)
	writer.add_attribute("clientCategorty", "Business", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic", "xs:string", 0)

	# Let"s add another statement
	writer.add_attribute_statement() # returns the index, which is going to be 1

	# Adding one attribute with two values - these are going to be merged
	# because their names are identical.
	writer.add_attribute("eduPersonAffiliation", "users", "", "", 1);
	writer.add_attribute("eduPersonAffiliation", "examplerole1", "", "", 1);

	# adding an authentication statement
	writer.add_authn_statement(currentDateTime(0), "id-1234567890", currentDateTime(1), "urn:oasis:names:tc:SAML:2.0:ac:classes:Password")

	# adding an authorization statement
	writer.add_authz_decision_statement(0, "Evidence", "Resource", "namespace=ns1;value=value1")

	# adding assertion conditions (audience and time scope)
	writer.add_condition(0, "PSCU:saml20:dev")
	writer.add_condition(4, currentDateTime(1))

	# setting custom assertion ID (optional)
	writer.assertion_id = "unique-id-123456"

	# adding subject confirmations
	writer.assertion_subject = writer.format_id("Subject", "Subject", "", "", "", "")
	writer.add_subject_confirmation("scmethod1", "http://scaddress.com", "screcipient", "", currentDateTime(-1), currentDateTime(1), "", "sctype", "scdata")

	# applying security: as it applies to the assertion, it should precede
	# the CompleteAssertion() call
	applySecurity()

	# adding ("committing") the formed assertion to the SAML message
	writer.complete_assertion()

	# Saving the output
	writer.save_file(output)

def createEnvelopedAssertion():
	# creating a message of Response type
	writer.create_new(11)

	# main message properties
	writer.message_id = "my-message-id-123"
	writer.message_issue_instant = currentDateTime(0)
	writer.message_destination = dest
	writer.message_issuer = writer.format_id(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", "")

	# a message may contain multiple complex assertions, each of which
	# should be enveloped into BeginAssertion/CompleteAssertion calls.
	writer.begin_assertion()
	writer.assertion_issuer = writer.message_issuer # keeping it simple
	writer.assertion_assertion_type = 2 # CSAT_ASSERTION

	# An assertion may contain multiple attributes within multiple statements.
	# If we add an attribute without adding a statement first, a new statement
	# will be created automatically, and the attribute added to it.
	# For example, the below two attributes go to the first statement,
	# which is created automatically:
	writer.add_attribute("clientID", "JohnDoe123", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic", "xs:string", 0)
	writer.add_attribute("clientCategorty", "Business", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic", "xs:string", 0)

	# adding an authentication statement
	writer.add_authn_statement(currentDateTime(0), "id-1234567890", currentDateTime(1), "urn:oasis:names:tc:SAML:2.0:ac:classes:Password")

	# adding assertion conditions (audience and time scope)
	writer.add_condition(0, "PSCU:saml20:dev")
	writer.add_condition(4, currentDateTime(1))

	# setting custom assertion ID (optional)
	writer.assertion_id = "unique-id-123456"

	# adding subject confirmations
	writer.assertion_subject = writer.format_id("Subject", "Subject", "", "", "", "")
	writer.add_subject_confirmation("scmethod1", "http://scaddress.com", "screcipient", "", currentDateTime(-1), currentDateTime(1), "", "sctype", "scdata")

	# applying security: as it applies to the assertion, it should precede
	# the CompleteAssertion() call
	applySecurity()

	# adding ("committing") the formed assertion to the SAML message
	writer.complete_assertion()

	# Saving the output
	writer.save_file(output)

if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    try:
        mestype = -1;
        
        for x in range(len(sys.argv)):
            if (sys.argv[x].startswith("-")):
                if (sys.argv[x].lower() == "-type"):
                    mestype = int(sys.argv[x+1])       
                if (sys.argv[x].lower() == "-output"):
                    output = sys.argv[x+1]
                if (sys.argv[x].lower() == "-issuer"):
                    issuer = sys.argv[x+1]
                if (sys.argv[x].lower() == "-dest"):
                    dest = sys.argv[x+1]
                if (sys.argv[x].lower() == "-service"):
                    service = sys.argv[x+1]
                if (sys.argv[x].lower() == "-scert"):
                    scert = sys.argv[x+1]
                if (sys.argv[x].lower() == "-scertpass"):
                    scertpass = sys.argv[x+1]
                if (sys.argv[x].lower() == "-ecert"):
                    ecert = sys.argv[x+1]
                if (sys.argv[x].lower() == "-ecertpass"):
                    ecertpass = sys.argv[x+1]
                if (sys.argv[x].lower() == "-hashalg"):
                    hashalg = sys.argv[x+1]       
                
        if (mestype == -1):
            displayHelp("-type is required.")
            sys.exit(1)
        
        if (output == ""):
            displayHelp("-output is required.")
            sys.exit(1)

        if (mestype == 0):
            createAuthnRequest()
        elif (mestype == 1):
            createLogoutRequest()
        elif (mestype == 2):
            createAttributeQuery()
        elif (mestype == 3):
            createSubjectQuery()
        elif (mestype == 4):
            createAssertion()
        elif (mestype == 5):
            createEnvelopedAssertion()
        else:
            print("Invalid type value.")
            sys.exit(1)

        print("The SAML message was created successfully.\n\n")
    except Exception as e: 
        print(e)







