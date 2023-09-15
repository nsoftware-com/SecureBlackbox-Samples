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
        "  cadessigner -- SecureBlackbox CAdESSigner Demo Application\n\n"
        "SYNOPSIS\n"
        "  cadessigner [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password]\n"
        "             [-extended] [-level level] [-sigtype signature_type] [-tsserver timestamp_server]\n\n"
        "DESCRIPTION\n"
        "  CAdESSigner demonstrates the usage of CAdESSigner from SecureBlackbox.\n"
        "  Used to create an CMS Advanced Electronic Signatures (CAdES) from file.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the CAdES will be saved (Required).\n\n"
        "  -cert         The certificate used to sign files (Required).\n\n"
        "  -certpass     The password for the signing certificate (Required).\n\n"
        "  -level        The level for CAdES signatures. Enter the corresponding number. Valid values:\n\n"
        "                  0  - UNKNOWN\n"
        "                  1  - BES\n"
        "                  2  - EPES\n"
        "                  3  - T\n"
        "                  4  - C\n"
        "                  5  - XTYPE_1\n"
        "                  6  - XTYPE_2\n"
        "                  7  - XLTYPE_1\n"
        "                  8  - XLTYPE_2\n"
        "                  9  - BASELINE_B\n"
        "                  10 - BASELINE_T\n"
        "                  11 - BASELINE_LT\n"
        "                  12 - BASELINE_LTA\n"
        "                  13 - EXTENDED_BES\n"
        "                  14 - EXTENDED_EPES\n"
        "                  15 - EXTENDED_T\n"
        "                  16 - EXTENDED_C\n"
        "                  17 - EXTENDED_XTYPE_1\n"
        "                  18 - EXTENDED_XTYPE_2\n"
        "                  19 - EXTENDED_XLTYPE_1\n"
        "                  20 - EXTENDED_XLTYPE_2\n"
        "                  21 - EXTENDED_A\n"
        "                  22 - A\n\n"
        "  -detached     Whether to store the generated signature in a separate message.\n\n"
        "  -hashalg      The Hash algorithm. Valid values: SHA1, MD5, SHA256, SHA384, SHA512, RIPEMD160\n\n"
        "EXAMPLES\n"
        "  cadessigner -input C:\\cades\\helloworld.txt -output C:\\cades\\mycades.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
        "  cadessigner -input C:\\cades\\helloworld.txt -output C:\\cades\\mycades.scs -cert C:\\certs\\mycert.pfx -certpass mypassword \\\n"
        "            -level 10 -detached -hashalg SHA256\n\n"
    )
    
if (len(sys.argv) < 9):
    displayHelp()
    sys.exit(1)
else:
    signer = CAdESSigner()
    cm = CertificateManager()
    
    inputF = ""
    outputF = ""
    certF = ""
    certpass = ""
    sigLevel = cslBES
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
            if (sys.argv[x].lower() == "-level"):
                sigLevel = int(sys.argv[x+1])
            if (sys.argv[x].lower() == "-detached"):
                detached = True
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
        signer.set_signing_cert_handle(cm.get_cert_handle())

        signer.sign(sigLevel, detached)
    
        print("The file successfully signed.")
    except Exception as e: 
        print(e)



