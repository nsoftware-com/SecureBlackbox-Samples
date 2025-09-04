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
        "  officeencryptor -- SecureBlackbox OfficeEncryptor Demo Application\n\n"
        "SYNOPSIS\n"
		"  officeencryptor <-input input_file> <-output output_file> <-pass encryption_password> [-enctype encryption_type] [-encalg encryption_algorithm] \n\n"
        "DESCRIPTION\n"
        "  OfficeEncryptor demonstrates the usage of OfficeEncryptor from SecureBlackbox.\n"
        "  Used to encrypt office documents. \n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to encrypt (Required).\n\n"
        "  -output       Where the encrypted file will be saved (Required).\n\n"
        "  -pass         Password for file encryption (Required).\n\n"
        "  -enctype      The type of encryption to use. Enter the corresponding number. Valid values:\n\n"
        "                  0 - OET_DEFAULT\n"
        "                  1 - OET_BINARY_RC4\n"
        "                  2 - OET_BINARY_RC4CRYPTO_API\n"
        "                  3 - OET_OPEN_XMLSTANDARD\n"
        "                  4 - OET_OPEN_XMLAGILE\n"
        "                  5 - OET_OPEN_DOCUMENT\n\n"
        "  -encalg       The encryption algorithm to use. Valid values: RC2, RC4, DES, 3DES, AES128, AES192, AES256, Blowfish \n\n"
        "EXAMPLES\n"
        "  officeencryptor -input C:\\office\\helloworld.doc -output C:\\office\\helloworld.enc -pass mypassword \n\n"
        "  officeencryptor -input C:\\office\\helloworld.doc -output C:\\office\\helloworld.enc -pass mypassword -enctype 2 -encalg AES128 \n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    encryptor = OfficeEncryptor()
    
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
            if (sys.argv[x].lower() == "-enctype"):
                encryptor.set_encryption_type(int(sys.argv[x+1]))
            if (sys.argv[x].lower() == "-encalg"):
                encryptor.set_encryption_algorithm(sys.argv[x+1])
    
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
        encryptor.set_input_file(inputF)
        encryptor.set_output_file(outputF)
    
        encryptor.set_password(password)

        encryptor.encrypt()
    
        print("Office document successfully encrypted.")
    except Exception as e: 
        print(e)





