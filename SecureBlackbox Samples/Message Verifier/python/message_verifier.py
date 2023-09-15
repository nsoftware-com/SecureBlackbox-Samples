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

def displayHelp():
    print(
        "NAME\n"
        "  messageverifier -- SecureBlackbox MessageVerifier Demo Application\n\n"
        "SYNOPSIS\n"
        "  messageverifier [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password]\n"
        "             [-extended] [-level level] [-sigtype signature_type] [-tsserver timestamp_server]\n\n"
        "DESCRIPTION\n"
        "  MessageVerifier demonstrates the usage of MessageVerifier from SecureBlackbox.\n"
        "  Used to validating PKCS#7-compliant signed files.\n\n"
        "  The options are as follows:\n\n"
        "  -input        A signature to verify (Required). If the signature is detached, this will take\n"
        "                the signature file and -datafile will take the original data.\n\n"
        "  -detached     Whether the signature is detached. Use -datafile to specify the original data.\n\n"
        "  -datafile     The original data (Required for detached signature).\n\n"
        "  -output       Where to save the verified, unpacked message (Required for non detached signature).\n\n"
        "  -cert         The certificate used to sign files.\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
        "EXAMPLES\n"
        "  messageverifier -input C:\\mes\\mymes.scs -detached -datafile C:\\mes\\helloworld.txt -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
        "  messageverifier -input C:\\mes\\mymes.scs -output C:\\mes\\helloworld.txt\n\n"
    )
    
if (len(sys.argv) < 5):
    displayHelp()
    sys.exit(1)
else:
    verifier = MessageVerifier()
    cm = CertificateManager()
    
    inputF = ""
    dataF = ""
    outputF = ""
    detached = False
    certF = ""
    certpass = ""
    
    try:
        for x in range(len(sys.argv)):
            if (sys.argv[x].startswith("-")):
                if (sys.argv[x].lower() == "-input"):
                    inputF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-detached"):
                    detached = True
                if (sys.argv[x].lower() == "-datafile"):
                    dataF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-output"):
                    outputF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-cert"):
                    certF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-certpass"):
                    certpass = sys.argv[x+1]
                    
        if (inputF == ""):
            print("-input is required.\n")
            displayHelp()
            sys.exit(1)

        verifier.set_input_file(inputF)
        
        if not (certF == ""):
            cm.import_from_file(certF, certpass)
            verifier.set_known_cert_count(1);
            verifier.set_known_cert_handle(0, cm.get_cert_handle());
            
        if detached:
            if (dataF == ""):
                print("-datafile is required if -detached is used.\n")
                displayHelp()
                sys.exit(1)
            else:
                verifier.set_data_file(dataF)
                verifier.verify_detached()
        else:
            if (outputF == ""):
                print("-output is required if -detached is not used.\n")
                displayHelp()
                sys.exit(1)
            else:
                verifier.set_output_file(outputF)
                verifier.verify()
        
        if (verifier.get_signature_validation_result == svtSignerNotFound):
            print("Signer not found")
        elif (verifier.get_signature_validation_result == svtFailure):
            print("Signature verification failed")
        elif (verifier.get_signature_validation_result == svtCorrupted):
            print("Signature is invalid")
        else:
            print("Signature validated successfully.")                
    except Exception as e: 
        print(e)



