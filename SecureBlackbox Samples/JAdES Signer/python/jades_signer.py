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
        "  jadessigner -- SecureBlackbox JAdESSigner Demo Application\n\n"
        "SYNOPSIS\n"
        "  jadessigner [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password]\n"
        "            [-level signature_level] [-autorevinfo]n\n"
        "DESCRIPTION\n"
        "  JAdESSigner demonstrates the usage of JAdESSigner from SecureBlackbox.\n"
        "  Used to create JWS/JAdES signatures.\n\n"
        "  The options are as follows:\n\n"
        "  -data         A data file (payload) to sign (Required).\n\n"
        "  -output       Where the signed file will be saved (Required).\n\n"
        "  -cert         The certificate used to sign file (Required).\n\n"
        "  -certpass     The password for the certificate (Required).\n\n"
        "  -level        The level for signatures. Enter the corresponding number.\n"
        "                Valid values:\n"
        "                  0  - JSON Web Signature\n"
        "                  1  - JAdES Baseline-B (Default)\n"
        "                  2  - JAdES Baseline-T\n"
        "                  3  - JAdES Baseline-LT\n"
        "                  4  - JAdES Baseline-LTA\n\n"
        "  -detached     Whether a detached signature should be produced.\n\n"
        "  -compact      Whether the JWS compact serialization should be used.\n\n"
        "  -flattened    Whether the flattened signature should be created.\n\n"
        "EXAMPLES\n"
        "  jadessigner -data C:\\Documents\\text.txt -output C:\\Documents\\mysignature.json -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n"
        "  jadessigner -data C:\\Documents\\data.json -output C:\\Documents\\mysignature.json -cert C:\\certs\\mycert.pfx -certpass mypassword -level 1 -detached -compact -flattened \n\n"
    )

if (len(sys.argv) < 7):
    displayHelp()
    sys.exit(1)
else:
    signer = JAdESSigner()
    cm = CertificateManager()
    
    dataF = ""
    outputF = ""
    certF = ""
    certpass = ""
    flattened = False
   
    for x in range(len(sys.argv)):
        if (sys.argv[x].startswith("-")):
            if (sys.argv[x].lower() == "-data"):
                dataF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-output"):
                outputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-cert"):
                certF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-certpass"):
                certpass = sys.argv[x+1]
            if (sys.argv[x].lower() == "-level"):
                signer.set_new_sig_level(int(sys.argv[x+1]))
            if (sys.argv[x].lower() == "-detached"):
                signer.set_detached(True)
            if (sys.argv[x].lower() == "-compact"):
                signer.set_compact_form(True)
            if (sys.argv[x].lower() == "-flattened"):
                flattened = True

    if (dataF == ""):
        print("-data is required.\n")
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
    
    signer.set_flattened_signature(flattened)

    cm.import_from_file(certF, certpass)
    signer.set_signing_cert_handle(cm.get_cert_handle())

    try:
        signer.set_data_file(dataF)
        signer.set_output_file(outputF)

        signer.sign()
    
        print("The JWS/JAdES signature created successfully.")
    except Exception as e: 
        print(e)



