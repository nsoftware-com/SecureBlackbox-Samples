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
        "  officedecryptor -- SecureBlackbox OfficeDecryptor Demo Application\n\n"
        "SYNOPSIS\n"
		"  officedecryptor <-input input_file> <-output output_file> <-pass decryption_password> \n\n"
        "DESCRIPTION\n"
        "  OfficeDecryptor demonstrates the usage of OfficeDecryptor from SecureBlackbox.\n"
        "  Used to decrypt office documents. \n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to decrypt (Required).\n\n"
        "  -output       Where the decrypted file will be saved (Required).\n\n"
        "  -pass         Password for file decryption (Required).\n\n"
        "EXAMPLES\n"
        "  officedecryptor -input C:\\office\\helloworld.enc -output C:\\office\\helloworld.doc -pass mypassword \n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    decryptor = OfficeDecryptor()
    
    inputF = ""
    outputF = ""
    password = ""

    for x in range(len(sys.argv)):
        if (sys.argv[x].startswith("-")):
            if (sys.argv[x].lower() == "-input"):
                inputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-output"):
                outputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-pass"):
                password = sys.argv[x+1]
    
    if (inputF == ""):
        displayHelp("-input is required.")
        sys.exit(1)
    
    if (outputF == ""):
        displayHelp("-output is required.")
        sys.exit(1)
    
    if (password == ""):
        displayHelp("-pass is required.")
        sys.exit(1)

    try:
        decryptor.set_input_file(inputF)
        decryptor.set_output_file(outputF)
    
        decryptor.set_password(password)

        decryptor.decrypt()
    
        print("Office document successfully decrypted.")
    except Exception as e: 
        print(e)





