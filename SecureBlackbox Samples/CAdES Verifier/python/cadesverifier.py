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
		"  cadesverifier -- SecureBlackbox CAdESVerifier Demo Application\n\n"
		"SYNOPSIS\n"
		"  cadesverifier <-input input_file> [-output output_file] [-data data_file] [-checkrev] [-ignoreerrors] [-offline]\n\n"
		"DESCRIPTION\n"
		"  This sample shows processing of CAdES signatures. \n\n"
		"  The options are as follows:\n\n"
		"  -input         An input file to verify (Required).\n\n"
		"  -output        Where to save the verified, unpacked message.\n\n"
		"  -data          The payload to be validated (for detached signatures).\n\n"
		"  -checkrev      Whether certificate revocation information should be checked.\n\n"
		"  -ignoreerrors  Whether to ignore chain validation errors.\n\n"
		"  -offline       Whether offline mode be used.\n\n"
		"EXAMPLES\n"
		"  cadesverifier -input C:\\myexe.scs -output C:\\helloworld.exe -offline\n\n"
		"  cadesverifier -input C:\\mydll.scs -data C:\\helloworld.dll -checkrev -ignoreerrors\n\n"
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
    displayHelp("")
    sys.exit(1)
else:
    verifier = CAdESVerifier()
    cm = CertificateManager()
    
    inputF = ""
    dataF = ""
    outputF = ""
    certF = ""
    certpass = ""
    checkrev = False
    ignoreerrors = False
    offline = False
    
    try:
        for x in range(len(sys.argv)):
            if (sys.argv[x].startswith("-")):
                if (sys.argv[x].lower() == "-input"):
                    inputF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-data"):
                    dataF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-output"):
                    outputF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-cert"):
                    certF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-certpass"):
                    certpass = sys.argv[x+1]
                if (sys.argv[x].lower() == "-checkrev"):
                    checkrev = True
                if (sys.argv[x].lower() == "-ignoreerrors"):
                    ignoreerrors = True
                if (sys.argv[x].lower() == "-offline"):
                    offline = True
                    
        if (inputF == ""):
            displayHelp("-input is required.")
            sys.exit(1)

        verifier.set_input_file(inputF)
        
        
        if checkrev:
            verifier.set_revocation_check(1) # Auto
        else:
            verifier.set_revocation_check(0) # None
        
        if ignoreerrors:
            verifier.set_ignore_chain_validation_errors(True) 
        else:
            verifier.set_ignore_chain_validation_errors(False)
            
        if offline:
            verifier.set_offline_mode(True) 
        else:
            verifier.set_offline_mode(False)
        
        if not (certF == ""):
            cm.import_from_file(certF, certpass)
            verifier.set_known_cert_count(1)
            verifier.set_known_cert_handle(0, cm.get_cert_handle())
            
        
        if (outputF != ""):
            verifier.set_detached(False)
            verifier.set_output_file(outputF)
        elif (dataF != ""):
            verifier.set_detached(True)
            verifier.set_data_file(value)(dataF)
        else:
            displayHelp("-output or -data is required.")
            sys.exit(1)
        
        verifier.verify()

        print("There are %i signatures in this file."%(verifier.get_signature_count()))
        for x in range(verifier.get_signature_count()):
            print("Signature #%i\n"
                  "  Validation Result: %d, %s\n"
                  "  Chain Result: %d\n\n"%
                  (x+1,
                   verifier.get_signature_signature_validation_result(x),
                   translateValidationResult(verifier.get_signature_signature_validation_result(x)),
                   verifier.get_signature_chain_validation_result(x))) 

    except Exception as e: 
        print(e)





