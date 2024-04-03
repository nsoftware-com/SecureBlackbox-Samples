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

def ensureArg(args, prompt, index):
  if len(args) <= index:
    while len(args) <= index:
      args.append(None)
    args[index] = input(prompt)
  elif args[index] == None:
    args[index] = input(prompt)

def displayHelp():
    print(
        "NAME\n"
        "  asicverifier -- SecureBlackbox ASiCVerifier Demo Application\n\n"
        "SYNOPSIS\n"
        "  asicverifier [-input input_file] [-extractpath extract_path]\n\n"
        "DESCRIPTION\n"
        "  ASiCVerifier demonstrates the usage of ASiCVerifier from SecureBlackbox.\n"
        "  Used to verify the signature of and optionally extract any files in an Associated Signature Container (ASic).\n\n"
        "  The options are as follows:\n\n"
        "  -input        The ASiC to verify (Required).\n\n"
        "  -extractpath  The path to extract files to. If unspecified, files will not be extracted.\n\n"
    )
    
def translateSigType(value):    
    if (value == 1):
        return "CAdES"
    elif (value == 2):
        return "XAdES"
    elif (value == 3):
        return "Timestamp"
    else:
        return "Unknown"
    
if (len(sys.argv) < 3):
    displayHelp()
    sys.exit(1)
else:
    verifier = ASiCVerifier()
    
    inputF = ""

    try:
        for x in range(len(sys.argv)):
            if (sys.argv[x].startswith("-")):
                if (sys.argv[x].lower() == "-input"):
                    inputF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-extractpath"):
                    verifier.set_extraction_mode(1) # All
                    verifier.set_output_path(sys.argv[x+1])
                    
        if (inputF == ""):
            print("-input is required.\n")
            displayHelp()
            sys.exit(1)

        verifier.set_input_file(inputF)
        verifier.verify()
    
        print("There are %i signatures in this file."%(verifier.get_signature_count()))
        for x in range(verifier.get_signature_count()):
            print("Signature #%i\n"
                  "  SignatureType: %s\n"
                  "  File(s): %s\n\n"%
                  (x+1, translateSigType(verifier.get_signature_signature_type(x)), verifier.get_signature_signed_files(x)))                  
    except Exception as e: 
        print(e)



