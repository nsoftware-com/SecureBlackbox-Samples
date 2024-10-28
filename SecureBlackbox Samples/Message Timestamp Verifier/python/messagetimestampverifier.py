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
        "  messagetimestampverifier -- SecureBlackbox MessageTimestampVerifier Demo Application\n\n"
        "SYNOPSIS\n"
		"  messagetimestampverifier <-input input_file> [-output output_file] [-data data_file] \n\n"
        "DESCRIPTION\n"
        "  MessageTimestampVerifier demonstrates the usage of MessageTimestampVerifier from SecureBlackbox.\n"
        "  Used to facilities in validating PKCS#7-compliant timestamped files. \n\n"
        "  The options are as follows:\n\n"
        "  -input        A timestamped file (Required). \n\n"
        "  -data         The original data (Required for detached signature).\n\n"
        "  -output       Where to save the verified, unpacked message (Required for non detached timestamped file).\n\n"
        "EXAMPLES\n"
        "  messagetimestampverifier -input C:\\pkcs7\\mymes.pkcs7 -output C:\\pkcs7\\helloworld.txt \n\n"
        "  messagetimestampverifier -input C:\\pkcs7\\mymes.pkcs7 -data C:\\pkcs7\\helloworld.txt \n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    verifier = MessageTimestampVerifier()
    
    inputF = ""
    dataF = ""
    outputF = ""
    
    try:
        for x in range(len(sys.argv)):
            if (sys.argv[x].startswith("-")):
                if (sys.argv[x].lower() == "-input"):
                    inputF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-data"):
                    dataF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-output"):
                    outputF = sys.argv[x+1]
                    
        if (inputF == ""):
            displayHelp("-input is required.")
            sys.exit(1)

        verifier.set_input_file(inputF)
            
        if (outputF != ""):
            verifier.set_output_file(outputF)
            verifier.verify()
        elif (dataF != ""):
            verifier.set_data_file(dataF)
            verifier.verify_detached()
        else:
            displayHelp("-output or -data is required.")
            sys.exit(1)
            
        if (verifier.get_signature_validation_result() == svtValid):
            print("The signature is valid.")
        elif (verifier.get_signature_validation_result() == svtUnknown):
            print("Signature verification failed: Unknown signature.")
        elif (verifier.get_signature_validation_result() == svtCorrupted):
            print("Signature verification failed: The signature is corrupt or invalid.")
        elif (verifier.get_signature_validation_result() == svtSignerNotFound):
            print("Signature verification failed: The signature does not contain a signer.")
        elif (verifier.get_signature_validation_result() == svtFailure):
            print("Signature verification failed.")
        else:
            print("Signature verification failed: Unknown signature.")

    except Exception as e: 
        print(e)





