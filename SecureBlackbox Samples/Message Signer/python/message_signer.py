# 
# SecureBlackbox 2022 Python Edition - Sample Project
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

def displayHelp():
    print(
        "NAME\n"
        "  messagesigner -- SecureBlackbox MessageSigner Demo Application\n\n"
        "SYNOPSIS\n"
        "  messagesigner [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password]\n"
        "             [-extended] [-level level] [-sigtype signature_type] [-tsserver timestamp_server]\n\n"
        "DESCRIPTION\n"
        "  MessageSigner demonstrates the usage of MessageSigner from SecureBlackbox.\n"
        "  Used to create PKCS#7-compliant signed files.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the signed file will be saved (Required).\n\n"
        "  -cert         The certificate used to sign files (Required).\n\n"
        "  -certpass     The password for the signing certificate (Required).\n\n"
        "  -sigtype      The type of signature to use. Enter the corresponding number. Valid values:\n\n"
        "                  1 - PKCS1 DETACHED\n"
        "                  2 - PKCS7 DETACHED (Default)\n"
        "                  3 - PKCS7 ENVELOPING\n\n"
        "  -hashalg      The Hash algorithm. Valid values: SHA1, MD5, SHA256, SHA384, SHA512, RIPEMD160\n\n"
        "EXAMPLES\n"
        "  messagesigner -input C:\\mes\\helloworld.txt -output C:\\mes\\mymes.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
        "  messagesigner -input C:\\mes\\helloworld.txt -output C:\\mes\\mymes.scs -cert C:\\certs\\mycert.pfx -certpass mypassword \\\n"
        "             -sigtype 2 -hashalg SHA256 \n\n"
    )
    
if (len(sys.argv) < 9):
    displayHelp()
    sys.exit(1)
else:
    signer = MessageSigner()
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
                signer.set_signature_type(int(sys.argv[x+1]))
            if (sys.argv[x].lower() == "-hashalg"):
                signer.set_hash_algorithm(sys.argv[x+1])
    
    if (inputF == ""):
        print("-input is required.\n")
        displayHelp()
        sys.exit(1)
    
    if (outputF == ""):
        print("-output is required.\n")
        displayHelp()
        sys.exit(1)
    
    if (certF == ""):
        print("-cert is required.\n")
        displayHelp()
        sys.exit(1)
    
    if (certpass == ""):
        print("-certpass is required.\n")
        displayHelp()
        sys.exit(1)

    try:
        signer.set_input_file(inputF)
        signer.set_output_file(outputF)
    
        cm.import_from_file(certF, certpass)
        signer.set_signing_cert_handle(cm.get_cert_handle());

        signer.sign()
    
        print("The file successfully signed.");
    except Exception as e: 
        print(e)



