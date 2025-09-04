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
        "  hashfunction -- SecureBlackbox HashFunction Demo Application\n\n"
        "SYNOPSIS\n"
		"  hashfunction <-input input_file> [-pass password] [-alg hash_alg] [-encoding encoding_type]\n\n"
        "DESCRIPTION\n"
        "  HashFunction demonstrates the usage of HashFunction from SecureBlackbox.\n"
        "  Used to create an hash from file.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to hash (Required).\n\n"
        "  -pass         The password for derive key.\n\n"
		"  -alg          The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224, MD5\n\n"
        "  -encoding     The encoding of hash. Valid values:\n\n"
        "                  0 - CET_DEFAULT\n"
        "                  1 - CET_BINARY\n"
        "                  2 - CET_BASE_64\n"
        "                  3 - CET_COMPACT\n"
        "                  4 - CET_JSON\n\n"
        "EXAMPLES\n"
        "  hashfunction -input C:\\hash\\helloworld.txt \n\n"
        "  hashfunction -input C:\\hash\\helloworld.txt -encoding 2 -pass mypassword \n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    hashF = HashFunction()
    hashF.set_output_encoding(cetBase64);
    cm = CryptoKeyManager()
    
    inputF = ""
    password = ""

    for x in range(len(sys.argv)):
        if (sys.argv[x].startswith("-")):
            if (sys.argv[x].lower() == "-input"):
                inputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-pass"):
                password = sys.argv[x+1]
            if (sys.argv[x].lower() == "-encoding"):
                hashF.set_output_encoding(int(sys.argv[x+1]))
    
    if (inputF == ""):
        displayHelp("-input is required.")
        sys.exit(1)
    
    try:
        if (password != ""):
            cm.derive_key(128, password, "")
            hashF.set_key_handle(cm.get_key_handle())

        output = hashF.hash_file(inputF)
        print("Calculated hash: " + output.decode('utf-8', errors='backslashreplace'))
    except Exception as e: 
        print(e)





