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
        "  authenticodeverifier -- SecureBlackbox AuthenticodeVerifier Demo Application\n\n"
        "SYNOPSIS\n"
		"  authenticodeverifier <-input input_file> [-cert certificate_file] [-certpass certificate_password]\n\n"
        "DESCRIPTION\n"
        "  AuthenticodeVerifier demonstrates the usage of AuthenticodeVerifier from SecureBlackbox.\n"
        "  Used to verify the signature.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to verify (Required).\n\n"
        "  -cert         The certificate used to sign files.\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
        "EXAMPLES\n"
        "  authenticodeverifier -input C:\\myexe.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
def translateSigValidity(value):    
    if (value == svtValid):
        return "Valid"
    elif (value == svtCorrupted):
        return "Corrupted"
    elif (value == svtSignerNotFound):
        return "Signer not found"
    elif (value == svtFailure):
        return "Failure"
    else:
        return "Unknown"
    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    verifier = AuthenticodeVerifier()
    cm = CertificateManager()
    
    inputF = ""
    certF = ""
    certpass = ""

    try:
        for x in range(len(sys.argv)):
            if (sys.argv[x].startswith("-")):
                if (sys.argv[x].lower() == "-input"):
                    inputF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-cert"):
                    certF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-certpass"):
                    certpass = sys.argv[x+1]
                    
        if (inputF == ""):
            displayHelp("-input is required.")
            sys.exit(1)

        verifier.set_input_file(inputF)
        
        if not (certF == ""):
            cm.import_from_file(certF, certpass)
            verifier.set_known_cert_count(1)
            verifier.set_known_cert_handle(0, cm.get_cert_handle())
        
        verifier.verify()

        if verifier.signed:
            print("There are %i signatures in this file."%(verifier.get_signature_count()))
            for x in range(verifier.get_signature_count()):
                print("Signature #%i\n"
                      "  Hash algorithm: %s\n"
                      "  Description: %s\n"
                      "  URL: %s\n"
                      "  Validity: %s\n\n"%
                      (x+1, verifier.get_signature_hash_algorithm(x), verifier.get_signature_description(x), verifier.get_signature_url(x), translateSigValidity(verifier.get_signature_signature_validation_result(x))))
        else:
            print("The file is not signed.\n")                 
    except Exception as e: 
        print(e)





