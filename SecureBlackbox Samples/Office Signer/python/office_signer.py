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
        "  officesigner -- SecureBlackbox OfficeSigner Demo Application\n\n"
        "SYNOPSIS\n"
        "  officesigner [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password]\n"
        "             [-sigtype signature_type] [-hashalg hash_algorithm] [-signdoc] [-signcore] [-signorigin]\n\n"
        "DESCRIPTION\n"
        "  OfficeSigner demonstrates the usage of OfficeSigner from SecureBlackbox.\n"
        "  Used to sign office documents.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the signed file will be saved (Required).\n\n"
        "  -cert         The certificate used to sign files (Required).\n\n"
        "  -certpass     The password for the signing certificate (Required).\n\n"
        "  -sigtype      The type of signature to use. Enter the corresponding number. Valid values:\n\n"
        "                  0 - DEFAULT\n"
        "                  1 - BINARY_CRYPTO_API\n"
        "                  2 - BINARY_XML\n"
        "                  3 - OPEN_XML\n"
        "                  4 - OPEN_XPS\n"
        "                  5 - OPEN_DOCUMENT\n\n"
        "  -hashalg      The hash algorithm.\n\n"
        "  -signdoc      Whether to sign the document itself.\n\n"
        "  -signcore     Whether to sign the core properties of the document.\n\n"
        "  -signorigin   Whether to sign the signature origin.\n\n"
        "EXAMPLES\n"
        "  officesigner -input C:\\office\\helloworld.doc -output C:\\office\\myoffice.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
        "  officesigner -input C:\\office\\helloworld.doc -output C:\\office\\myoffice.scs -cert C:\\certs\\mycert.pfx -certpass mypassword \\\n"
        "             -sigtype 2 -hashalg SHA256 -signdoc -signcore -signorigin\n\n"
    )
    
if (len(sys.argv) < 9):
    displayHelp()
    sys.exit(1)
else:
    signer = OfficeSigner()
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
            if (sys.argv[x].lower() == "-signdoc"):
                signer.set_new_sig_document_signed(True)
            if (sys.argv[x].lower() == "-signcore"):
                signer.set_new_sig_core_properties_signed(True)
            if (sys.argv[x].lower() == "-signorigin"):
                signer.set_new_sig_signature_origin_signed(True)
    
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
        signer.set_signing_cert_handle(cm.get_cert_handle())

        signer.sign()
    
        print("Office file successfully signed.")
    except Exception as e: 
        print(e)



