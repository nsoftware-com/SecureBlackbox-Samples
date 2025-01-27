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
		"  jadessigner -- SecureBlackbox JAdESSigner Demo Application\n\n"
		"SYNOPSIS\n"
		"  jadessigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password]\n"
		"             [-level sig_level] [-detached] [-compact] [-flattened]\n\n"
		"DESCRIPTION\n"
		"  This sample illustrates the use of JAdESSigner component for creating JWS/JAdES signature.\n\n"
		"  The options are as follows:\n\n"
		"  -input        An input file to sign (Required).\n\n"
		"  -output       Where the signed file will be saved (Required).\n\n"
		"  -cert         The certificate used to sign files (Required).\n\n"
		"  -certpass     The password for the signing certificate.\n\n"
		"  -level        The level for JAdES signatures. Enter the corresponding number. Valid values:\n\n"
		"                  1  - JASL_GENERIC\n"
		"                  2  - JASL_BASELINE_B\n"
		"                  3  - JASL_BASELINE_T\n"
		"                  4  - JASL_BASELINE_LT\n"
		"                  5  - JASL_BASELINE_LTA\n\n"
		"  -detached     Whether a detached signature should be produced.\n\n"
		"  -compact      Whether the JWS compact serialization should be used.\n\n"
		"  -flattened    Whether the flattened signature should be created.\n\n"
		"EXAMPLES\n"
		"  jadessigner -input C:\\helloworld.exe -output C:\\myexe.scs -cert C:\\certs\\mycert.pfx -certpass mypassword -flattened\n\n"
		"  jadessigner -input C:\\helloworld.dll -output C:\\mydll.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
		"             -level 1 -detached -compact\n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)

if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    signer = JAdESSigner()
    signer.set_new_sig_level(jaslBaselineB)
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
            if (sys.argv[x].lower() == "-level"):
                signer.set_new_sig_level(int(sys.argv[x+1]))
            if (sys.argv[x].lower() == "-detached"):
                signer.set_detached(True)
            if (sys.argv[x].lower() == "-compact"):
                signer.set_compact_form(True)
            if (sys.argv[x].lower() == "-flattened"):
                signer.set_flattened_signature(True)

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
        signer.set_data_file(inputF)
        signer.set_output_file(outputF)

        signer.sign()
    
        print("The JWS/JAdES signature created successfully.")
    except Exception as e: 
        print(e)





