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
        "  authenticodesigner -- SecureBlackbox AuthenticodeSigner Demo Application\n\n"
        "SYNOPSIS\n"
		"  authenticodesigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password]\n"
        "             [-sigurl sigurl] [-hashalg hashalg] [-individual] [-remove] [-tsserver timestamp_server]\n\n"
        "DESCRIPTION\n"
        "  AuthenticodeSigner demonstrates the usage of AuthenticodeSigner from SecureBlackbox.\n"
        "  Used to sign EXE and DLL files.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the signed file will be saved (Required).\n\n"
        "  -cert         The certificate used to sign files (Required).\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
        "  -sigurl       The signature URL.\n\n"
		"  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224, MD5\n\n"
        "  -individual   Whether to use individual signatures.\n\n"
        "  -tsserver     A timestamp server to use during signing.\n\n"
        "  -remove       Whether to remove existing signature.\n\n"
        "EXAMPLES\n"
        "  authenticodesigner -input C:\\helloworld.exe -output C:\\myexe.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
        "  authenticodesigner -input C:\\helloworld.dll -output C:\\mydll.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
        "             -hashalg SHA256 -individual -remove -tsserver http://timestamp.wosign.com\n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    signer = AuthenticodeSigner()
    cm = CertificateManager()
    
    inputF = ""
    outputF = ""
    certF = ""
    certpass = ""
    
    signer.set_statement_type(2) # COMMERCIAL

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
            if (sys.argv[x].lower() == "-sigurl"):
                signer.set_signature_url(sys.argv[x+1])
            if (sys.argv[x].lower() == "-hashalg"):
                signer.set_hash_algorithm(sys.argv[x+1])
            if (sys.argv[x].lower() == "-individual"):
                signer.set_statement_type(1) # INDIVIDUAL
            if (sys.argv[x].lower() == "-tsserver"):
                signer.set_timestamp_server(sys.argv[x+1])
            if (sys.argv[x].lower() == "-remove"):
                signer.set_remove_existing_signatures(True)
    
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
    
        print("The file has been successfully signed.")
    except Exception as e: 
        print(e)





