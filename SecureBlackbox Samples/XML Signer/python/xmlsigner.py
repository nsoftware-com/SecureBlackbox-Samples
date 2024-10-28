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
        "  xmlsigner -- SecureBlackbox XMLSigner Demo Application\n\n"
        "SYNOPSIS\n"
		"  xmlsigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password] \n"
        "          [-hashalg hash_algorithm] [-canonmethod canon_method] [-includekey] [-detached] \n\n"
        "DESCRIPTION\n"
        "  XMLSigner demonstrates the usage of XMLSigner from SecureBlackbox.\n"
        "  Used to create an XML Signature from an XML file.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the XML signature will be saved (Required).\n\n"
        "  -cert         The certificate used to sign files (Required).\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
        "  -detached     Whether the signature is detached.\n\n"
        "  -canonmethod  The canonicalization method to use. Enter the corresponding number. Valid values:\n\n"
        "                  0 - CXCM_NONE\n"
        "                  1 - CXCM_CANON\n"
        "                  2 - CXCM_CANON_COMMENT\n"
        "                  3 - CXCM_EXCL_CANON\n"
        "                  4 - CXCM_EXCL_CANON_COMMENT\n"
        "                  5 - CXCM_MIN_CANON\n"
        "                  6 - CXCM_CANON_V_1_1\n"
        "                  7 - CXCM_CANON_COMMENT_V_1_1\n\n"
		"  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224\n\n"
        "  -includekey   Whether to include the public key in the signature.\n\n"
        "EXAMPLES\n"
        "  xmlsigner -input C:\\xml\\myfile.xml -output C:\\xml\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
        "  xmlsigner -input C:\\xml\\myfile.xml -output C:\\xml\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
        "           -canonmethod 3 -hashalg SHA1 -detached\n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    signer = XMLSigner()
    cm = CertificateManager()
    
    inputF = ""
    outputF = ""
    certF = ""
    certpass = ""
    detached = False
    
    for x in range(len(sys.argv)):
        if (sys.argv[x].startswith("-")):
            if (sys.argv[x].lower() == "-input"):
                inputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-output"):
                outputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-cert"):
                certF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-certpass"):
                certpass = sys.argv[x+1]
            if (sys.argv[x].lower() == "-detached"):
                detached = True
            if (sys.argv[x].lower() == "-canonmethod"):
                signer.set_canonicalization_method(int(sys.argv[x+1]))
            if (sys.argv[x].lower() == "-hashalg"):
                signer.set_hash_algorithm(sys.argv[x+1])
            if (sys.argv[x].lower() == "-includekey"):
                signer.config("IncludeKey=true")
    
    if (inputF == ""):
        displayHelp("-input is required.")
        sys.exit(1)
    
    if (outputF == ""):
        displayHelp("-output is required.")
        sys.exit(1)
    
    if (certF == ""):
        displayHelp("-cert is required.")
        sys.exit(1)

    try:
        signer.set_output_file(outputF)
    
        if detached:
            signer.set_data_file(inputF)
            signer.set_data_type(1) # cxdtBinary
            signer.set_data_uri(os.path.basename(inputF))
            signer.set_signature_type(1) # cxstDetached
        else:
            signer.set_input_file(inputF)
            signer.set_signature_type(4) # cxstEnveloped
    
        cm.import_from_file(certF, certpass)
        signer.set_signing_cert_handle(cm.get_cert_handle())

        signer.sign()
    
        print("XML file successfully signed.")
    except Exception as e: 
        print(e)





