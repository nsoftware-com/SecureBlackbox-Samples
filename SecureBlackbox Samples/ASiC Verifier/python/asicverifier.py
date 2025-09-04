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
		"  asicverifier -- SecureBlackbox ASiCVerifier Demo Application\n\n"
		"SYNOPSIS\n"
		"  asicverifier <-input input_file> [-extractpath extract_path]\n\n"
		"DESCRIPTION\n"
		"  ASiCVerifier demonstrates the usage of ASiCVerifier from SecureBlackbox.\n"
		"  Used to verify the signature of and optionally extract any files in an Associated Signature Container (ASiC).\n\n"
		"  The options are as follows:\n\n"
		"  -input         The ASiC to verify (Required).\n\n"
		"  -extractpath   The path to extract files to. If unspecified, files will not be extracted.\n\n"
		"EXAMPLES\n"
		"  asicverifier -input C:\\asic\\myasic.scs\n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
def translateSigType(value):    
    if (value == 1):
        return "CAdES"
    elif (value == 2):
        return "XAdES"
    elif (value == 3):
        return "Timestamp"
    else:
        return "Unknown"
    
def translateValidationResult(value):
    if (value == svtValid):
        return "The signature is valid."
    elif (value == svtCorrupted):
        return "The signature is corrupted."
    elif (value == svtSignerNotFound):
        return "Failed to acquire the signing certificate. The signature cannot be validated."
    elif (value == svtFailure):
        return "General failure."
    else:
        return "Signature validity is unknown."

if (len(sys.argv) <= 1):
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
                  "  File(s): %s\n"
                  "  Validation Result: %d, %s\n"
                  "  Chain Result: %d\n\n"%
                  (x+1, translateSigType(verifier.get_signature_signature_type(x)), verifier.get_signature_signed_files(x),
                   verifier.get_signature_signature_validation_result(x), translateValidationResult(verifier.get_signature_signature_validation_result(x)),
                   verifier.get_signature_chain_validation_result(x)))
    except Exception as e: 
        print(e)





