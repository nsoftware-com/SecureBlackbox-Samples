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
        "  messagedecompressor -- SecureBlackbox MessageDecompressor Demo Application\n\n"
        "SYNOPSIS\n"
		"  messagedecompressor <-input input_file> <-output output_file>\n\n"
        "DESCRIPTION\n"
        "  MessageDecompressor demonstrates the usage of MessageDecompressor from SecureBlackbox.\n"
        "  Used to decompressed PKCS#7 messages.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to decompress (Required).\n\n"
        "  -output       Where the original file will be saved (Required).\n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    decompressor = MessageDecompressor()
    
    inputF = ""
    outputF = ""

    for x in range(len(sys.argv)):
        if (sys.argv[x].startswith("-")):
            if (sys.argv[x].lower() == "-input"):
                inputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-output"):
                outputF = sys.argv[x+1]
    
    if (inputF == ""):
        displayHelp("-input is required.")
        sys.exit(1)
    
    if (outputF == ""):
        displayHelp("-output is required.")
        sys.exit(1)
    
    try:
        decompressor.set_input_file(inputF)
        decompressor.set_output_file(outputF)
    
        decompressor.decompress()
    
        print("The file successfully decompressed.")
    except Exception as e: 
        print(e)





