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
        "  soapsigner -- SecureBlackbox SOAPSigner Demo Application\n\n"
        "SYNOPSIS\n"
		"  soapsigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password]\n"
        "             [-signbody] [-hashalg hash_algorithm] [-sigtype signature_type]\n\n"
        "DESCRIPTION\n"
        "  SOAPSigner demonstrates the usage of SOAPSigner from SecureBlackbox.\n"
        "  Used to create SOAP or WSS signatures.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the signed file will be saved (Required).\n\n"
        "  -cert         The certificate used to sign files (Required).\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
        "  -sigtype      The type of signature to use. Enter the corresponding number. Valid values:\n\n"
        "                  1 - SST_WSSSIGNATURE\n"
        "                  2 - SST_SOAPSIGNATURE\n\n"
        "  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224, MD5\n\n"
        "  -signbody     Whether to sign body.\n\n"
        "EXAMPLES\n"
        "  soapsigner -input C:\\soap\\helloworld.txt -output C:\\soap\\mysoap.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
        "  soapsigner -input C:\\soap\\helloworld.txt -output C:\\soap\\mysoap.scs -cert C:\\certs\\mycert.pfx -certpass mypassword \n"
        "             -sigtype 2 -hashalg SHA256 -signbody\n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)

if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    signer = SOAPSigner()
    cm = CertificateManager()
    
    inputF = ""
    outputF = ""
    certF = ""
    certpass = ""
    
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
            if (sys.argv[x].lower() == "-sigtype"):
                signer.set_new_sig_signature_type(int(sys.argv[x+1]))
            if (sys.argv[x].lower() == "-hashalg"):
                signer.set_new_sig_hash_algorithm(sys.argv[x+1])
            if (sys.argv[x].lower() == "-signbody"):
                signer.add_body_reference("", True)

    if (inputF == ""):
        displayHelp("-input is required.")
        sys.exit(1)
    
    if (outputF == ""):
        displayHelp("-output is required.")
        sys.exit(1)
    
    if (certF == ""):
        displayHelp("-cert is required.")
        sys.exit(1)
        
    cm.import_from_file(certF, certpass)
    signer.set_signing_cert_handle(cm.get_cert_handle())
    
    try:
        signer.set_input_file(inputF)
        signer.set_output_file(outputF)

        signer.sign()
    
        print("SOAP message successfully signed.")
    except Exception as e: 
        print(e)





