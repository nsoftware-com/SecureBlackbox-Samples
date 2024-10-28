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
        "  messagecompressor -- SecureBlackbox MessageCompressor Demo Application\n\n"
        "SYNOPSIS\n"
		"  messagecompressor <-input input_file> <-output output_file> [-level level]\n\n"
        "DESCRIPTION\n"
        "  MessageCompressor demonstrates the usage of MessageCompressor from SecureBlackbox.\n"
        "  Used to create compressed PKCS#7 messages.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to compress (Required).\n\n"
        "  -output       Where the compressed file will be saved (Required).\n\n"
        "  -level        The comression level. \n\n"
        "EXAMPLES\n"
        "  messagecompressor -input C:\\pkcs7\\helloworld.txt -output C:\\pkcs7\\compress.pkcs7 \n\n"
        "  messagecompressor -input C:\\pkcs7\\helloworld.txt -output C:\\pkcs7\\compress.pkcs7 -level 7 \n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    compressor = MessageCompressor()
    
    inputF = ""
    outputF = ""

    for x in range(len(sys.argv)):
        if (sys.argv[x].startswith("-")):
            if (sys.argv[x].lower() == "-input"):
                inputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-output"):
                outputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-level"):
                compressor.set_compression_level(int(sys.argv[x+1]))
    
    if (inputF == ""):
        displayHelp("-input is required.")
        sys.exit(1)
    
    if (outputF == ""):
        displayHelp("-output is required.")
        sys.exit(1)
    
    try:
        compressor.set_input_file(inputF)
        compressor.set_output_file(outputF)
    
        compressor.compress()
    
        print("The file successfully compressed.")
    except Exception as e: 
        print(e)





