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
        "  cadesverifier -- SecureBlackbox CAdESVerifier Demo Application\n\n"
        "SYNOPSIS\n"
        "  cadesverifier [-input input_file] [-extractpath extract_path]\n\n"
        "DESCRIPTION\n"
        "  CAdESVerifier demonstrates the usage of CAdESVerifier from SecureBlackbox.\n"
        "  Used to verify the signature of and optionally extract any files in an CMS Advanced Electronic Signatures (CAdES).\n\n"
        "  The options are as follows:\n\n"
        "  -input                         A signature to verify (Required).\n\n"
        "  -detached                      Whether the signature is detached. Use -datafile to specify the original data.\n\n"
        "  -datafile                      The original data (Required for detached signature).\n\n"
        "  -output                        Where to save the verified, unpacked message (Required for non detached signature).\n\n"
        "  -cert                          The certificate used to sign files.\n\n"
        "  -certpass                      The password for the signing certificate.\n\n"
        "  -performRevocationCheck        Whether certificate revocation information should be checked.\n"
        "  -ignoreChainValidationErrors   Whether to ignore chain validation errors.\n"
        "  -forceCompleteChainValidation  Whether to check issuer (CA) certificates when the signing certificate is invalid.\n"
        "EXAMPLES\n"
        "  cadesverifier -input C:\\cades\\mycades.scs -output C:\\cades\\helloworld.txt\n\n"
        "  cadesverifier -input C:\\cades\\mycades.scs -detached -datafile C:\\cades\\helloworld.txt -cert C:\\certs\\mycert.pfx -certpass mypassword \n"
        "             -performRevocationCheck -ignoreChainValidationErrors -forceCompleteChainValidation \n\n"
    )
    
if (len(sys.argv) < 5):
    displayHelp()
    sys.exit(1)
else:
    verifier = CAdESVerifier()
    cm = CertificateManager()
    
    inputF = ""
    dataF = ""
    outputF = ""
    detached = False
    certF = ""
    certpass = ""
    performRevocationCheck = False
    ignoreChainValidationErrors = False
    forceCompleteChainValidation = False
    
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
                if (sys.argv[x].lower() == "-performRevocationCheck"):
                    performRevocationCheck = True
                if (sys.argv[x].lower() == "-ignoreChainValidationErrors"):
                    ignoreChainValidationErrors = True
                if (sys.argv[x].lower() == "-forceCompleteChainValidation"):
                    forceCompleteChainValidation = True
                    
        if (inputF == ""):
            print("-input is required.\n")
            displayHelp()
            sys.exit(1)

        verifier.set_input_file(inputF)
        
        
        if performRevocationCheck:
            verifier.set_revocation_check(1) # Auto
        else:
            verifier.set_revocation_check(0) # None
        
        if ignoreChainValidationErrors:
            verifier.set_ignore_chain_validation_errors(True) 
        else:
            verifier.set_ignore_chain_validation_errors(False)
            
        if forceCompleteChainValidation:
            verifier.config("ForceCompleteChainValidation=true")
        else:
            verifier.config("ForceCompleteChainValidation=false")
        
        if not (certF == ""):
            cm.import_from_file(certF, certpass)
            verifier.set_known_cert_count(1)
            verifier.set_known_cert_handle(0, cm.get_cert_handle())
            
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



