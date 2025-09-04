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
		"  cadessigner -- SecureBlackbox CAdESSigner Demo Application\n\n"
		"SYNOPSIS\n"
		"  cadessigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password]\n"
		"             [-level sig_level] [-hashalg hashalg] [-detached]\n\n"
		"DESCRIPTION\n"
		"  This sample shows how to create CAdES signatures.\n\n"
		"  The options are as follows:\n\n"
		"  -input        An input file to sign (Required).\n\n"
		"  -output       Where the signed file will be saved (Required).\n\n"
		"  -cert         The certificate used to sign files (Required).\n\n"
		"  -certpass     The password for the signing certificate.\n\n"
		"  -level        The level for CAdES signatures. Enter the corresponding number. Valid values:\n\n"
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
		"  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224, MD5\n\n"
		"  -detached     Whether to store the generated signature in a separate message.\n\n"
		"EXAMPLES\n"
		"  cadessigner -input C:\\helloworld.exe -output C:\\myexe.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
		"  cadessigner -input C:\\helloworld.dll -output C:\\mydll.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
		"             -hashalg SHA256 -level 1 -detached\n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    signer = CAdESSigner()
    cm = CertificateManager()
    
    inputF = ""
    outputF = ""
    certF = ""
    certpass = ""
    signer.set_new_sig_level(aslBES)

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
                signer.set_new_sig_level(int(sys.argv[x+1]))
            if (sys.argv[x].lower() == "-detached"):
                signer.set_detached(True)
            if (sys.argv[x].lower() == "-hashalg"):
                signer.set_new_sig_hash_algorithm(sys.argv[x+1])
    
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
        signer.set_input_file(inputF)
        signer.set_output_file(outputF)
    
        cm.import_from_file(certF, certpass)
        signer.set_signing_cert_handle(cm.get_cert_handle())

        signer.sign()
    
        print("The file successfully signed.")
    except Exception as e: 
        print(e)





