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
        "  messageverifier -- SecureBlackbox MessageVerifier Demo Application\n\n"
        "SYNOPSIS\n"
		"  messageverifier <-input input_file> [-output output_file] [-data data_file]\n"
        "            [-cert certificate_file] [-certpass certificate_password]\n\n"
        "DESCRIPTION\n"
        "  MessageVerifier demonstrates the usage of MessageVerifier from SecureBlackbox.\n"
        "  Used to validating PKCS#7-compliant signed files.\n\n"
        "  The options are as follows:\n\n"
        "  -input        A signature to verify (Required). If the signature is detached, this will take\n"
        "                the signature file and -data will take the original data.\n\n"
        "  -data         The original data (Required for detached signature).\n\n"
        "  -output       Where to save the verified, unpacked message (Required for non detached signature).\n\n"
        "  -cert         The certificate used to sign files.\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
        "EXAMPLES\n"
        "  messageverifier -input C:\\mes\\mymes.scs -detached -data C:\\mes\\helloworld.txt -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
        "  messageverifier -input C:\\mes\\mymes.scs -output C:\\mes\\helloworld.txt\n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    verifier = MessageVerifier()
    cm = CertificateManager()
    
    inputF = ""
    dataF = ""
    outputF = ""
    certF = ""
    certpass = ""
    
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
                    
        if (inputF == ""):
            displayHelp("-input is required.")
            sys.exit(1)

        verifier.set_input_file(inputF)
        
        if not (certF == ""):
            cm.import_from_file(certF, certpass)
            verifier.set_known_cert_count(1);
            verifier.set_known_cert_handle(0, cm.get_cert_handle());
            
        if (outputF != ""):
            verifier.set_output_file(outputF)
            verifier.verify()
        elif (dataF != ""):
            verifier.set_data_file(outputF)
            verifier.verify_detached()
        else:
            displayHelp("-output or -data is required.")
            sys.exit(1)
        
        if (verifier.get_signature_validation_result() == svtValid):
            print("The signature is valid.\n"
                  "Hash algorithm: %s"%verifier.get_hash_algorithm())
        elif (verifier.get_signature_validation_result() == svtCorrupted):
            print("Signature verification failed: The signature does not contain a signer.")
        elif (verifier.get_signature_validation_result() == svtSignerNotFound):
            print("Signature verification failed: The signature does not contain a signer.")
        elif (verifier.get_signature_validation_result() == svtFailure):
            print("Signature verification failed.")
        else:
            print("Signature verification failed: Unknown signature.")
            
    except Exception as e: 
        print(e)





