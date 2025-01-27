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


def displayHelp(errMes):
    print(
        "NAME\n"
        "  asicsigner -- SecureBlackbox ASiCSigner Demo Application\n\n"
        "SYNOPSIS\n"
		"  asicsigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password]\n"
        "             [-extended] [-level level] [-sigtype signature_type] [-tsserver timestamp_server]\n\n"
        "DESCRIPTION\n"
        "  ASiCSigner demonstrates the usage of ASiCSigner from SecureBlackbox.\n"
        "  Used to create an Associated Signature Container (ASiC) from one or more files.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the ASiC will be saved (Required).\n\n"
        "  -cert         The certificate used to sign files (Required).\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
        "  -sigtype      The type of signature to use. Enter the corresponding number. Valid values:\n\n"
        "                  0 - CAST_UNKNOWN\n"
        "                  1 - CAST_CAD_ES\n"
        "                  2 - CAST_XAD_ES\n\n"
        "  -level        The level for CAdES/XAdES signatures. Enter the corresponding number. Valid values:\n\n"
        "                  0  - ASL_UNKNOWN\n"
        "                  1  - ASL_GENERIC\n"
        "                  2  - ASL_BASELINE_B\n"
        "                  3  - ASL_BASELINE_T\n"
        "                  4  - ASL_BASELINE_LT\n"
        "                  5  - ASL_BASELINE_LTA\n"
        "                  6  - ASL_BES\n"
        "                  7  - ASL_EPES\n"
        "                  8  - ASL_T\n"
        "                  9  - ASL_C\n"
        "                  10 - ASL_X\n"
        "                  11 - ASL_XTYPE_1\n"
        "                  12 - ASL_XTYPE_2\n"
        "                  13 - ASL_XL\n"
        "                  14 - ASL_XLTYPE_1\n"
        "                  15 - ASL_XLTYPE_2\n"
        "                  16 - ASL_A\n"
        "                  17 - ASL_EXTENDED_BES\n"
        "                  18 - ASL_EXTENDED_EPES\n"
        "                  19 - ASL_EXTENDED_T\n"
        "                  20 - ASL_EXTENDED_C\n"
        "                  21 - ASL_EXTENDED_X\n"
        "                  22 - ASL_EXTENDED_XTYPE_1\n"
        "                  23 - ASL_EXTENDED_XTYPE_2\n"
        "                  24 - ASL_EXTENDED_XLONG\n"
        "                  25 - ASL_EXTENDED_XL\n"
        "                  26 - ASL_EXTENDED_XLTYPE_1\n"
        "                  27 - ASL_EXTENDED_XLTYPE_2\n"
        "                  28 - ASL_EXTENDED_A\n\n"
        "  -extended     Whether to use extended signatures.\n\n"
        "  -tsserver     A timestamp server to use during signing.\n\n"
        "EXAMPLES\n"
        "  asicsigner -input C:\\asic\\helloworld.txt -output C:\\asic\\myasic.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
        "  asicsigner -input C:\\asic\\helloworld.txt -output C:\\asic\\myasic.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
        "             -sigtype 2 -level 10 -extended -tsserver http://timestamp.wosign.com\n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    signer = ASiCSigner()
    signer.set_new_sig_signature_type(1);
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
            if (sys.argv[x].lower() == "-level"):
                signer.set_new_sig_level(int(sys.argv[x+1]))
            if (sys.argv[x].lower() == "-extended"):
                signer.set_extended(True)
            if (sys.argv[x].lower() == "-tsserver"):
                signer.set_timestamp_server(sys.argv[x+1])
    
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
        signer.set_source_files(inputF)
        signer.set_output_file(outputF)
    
        cm.import_from_file(certF, certpass)
        signer.set_signing_cert_handle(cm.get_cert_handle())

        signer.sign()
    
        print("ASiC created.")
    except Exception as e: 
        print(e)





