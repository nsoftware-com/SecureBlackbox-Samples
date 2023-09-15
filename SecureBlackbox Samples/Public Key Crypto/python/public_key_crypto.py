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
        "  publickeycrypto -- SecureBlackbox PublicKeyCrypto Demo Application\n\n"
        "SYNOPSIS\n"
        "  publickeycrypto -s/-v [-input input_file] [-output output_file] [-signature -signature_file]\n"
        "                  [-cert certificate_file] [-certpass certificate_password] [-encoding signature_encoding]\n\n"
        "DESCRIPTION\n"
        "  PublicKeyCrypto demonstrates the usage of PublicKeyCrypto from SecureBlackbox.\n"
        "  Used to sign and verify files.\n\n"
        "  The options are as follows:\n\n"
        "  -s            Sign input file and save to output \n\n" 
        "  -v            Verify signature using original file (input) \n\n" 
        "  -input        An input file to sign or verify (Required). \n\n"
        "  -output       Where the signed file will be saved (Required for signing). \n\n"
        "  -signature    An signature file to verify (Required on verifing). \n\n"
        "  -cert         The certificate used to sign files (Required).\n\n"
        "  -certpass     The password for the signing certificate (Required).\n\n"
        "  -encoding     The encoding of signature. Valid values:\n\n"
        "                  0 - DEFAULT\n"
        "                  1 - BINARY\n"
        "                  2 - BASE_64\n"
        "                  3 - COMPACT\n"
        "                  4 - JSON\n\n"
        "EXAMPLES\n"
        "  publickeycrypto -s -input C:\\cypto\\helloworld.txt -output C:\\cypto\\signature.dat -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n"
        "  publickeycrypto -v -input C:\\cypto\\helloworld.txt -signature C:\\cypto\\signature.dat -cert C:\\certs\\mycert.pfx -certpass mypassword -encoding 2 \n\n"
    )

if (len(sys.argv) < 10):
    displayHelp()
    sys.exit(1)
else:
    crypto = PublicKeyCrypto()
    cm = CertificateManager()
    ckm = CryptoKeyManager()
    
    sign = False
    verify = False
    inputF = ""
    outputF = ""
    signatureF = ""
    certF = ""
    certpass = ""
    encoding = 0
    
    for x in range(len(sys.argv)):
        if (sys.argv[x].startswith("-")):
            if (sys.argv[x].lower() == "-s"):
                sign = True
            if (sys.argv[x].lower() == "-v"):
                verify = True
            if (sys.argv[x].lower() == "-input"):
                inputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-output"):
                outputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-signature"):
                signatureF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-cert"):
                certF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-certpass"):
                certpass = sys.argv[x+1]
            if (sys.argv[x].lower() == "-encoding"):
                encoding = int(sys.argv[x+1])

    if (not (sign or verify)):
        print("-s or -v is required.\n")
        displayHelp()
        sys.exit(1)

    if (sign and verify):
        print("-Use only one -s or -v parameter.\n")
        displayHelp()
        sys.exit(1)

    if (inputF == ""):
        print("-input is required.\n")
        displayHelp()
        sys.exit(1)
    
    if (sign and (outputF == "")):
        print("-output is required.\n")
        displayHelp()
        sys.exit(1)
    
    if (verify and (signatureF == "")):
        print("-signature is required.\n")
        displayHelp()
        sys.exit(1)
    
    if (certF == ""):
        print("-cert is required.\n")
        displayHelp()
        sys.exit(1)
    
    if (certpass == ""):
        print("-certpass is required.\n")
        displayHelp()
        sys.exit(1)
        
    try:
        cm.import_from_file(certF, certpass)
        ckm.set_cert_handle(cm.get_cert_handle())
        ckm.import_from_cert()
        crypto.set_key_handle(ckm.get_key_handle())
      
        if sign:
            crypto.set_output_encoding(encoding)
            crypto.sign_file(inputF, outputF, True)
            print("The file was signed successfully.");
        else:
            crypto.set_input_encoding(encoding)
            crypto.verify_detached_file(inputF, signatureF)

            if (crypto.get_signature_validation_result == svtSignerNotFound):
                print("Signer not found")
            elif (crypto.get_signature_validation_result == svtFailure):
                print("Signature verification failed")
            elif (crypto.get_signature_validation_result == svtCorrupted):
                print("Signature is invalid")
            elif (crypto.get_signature_validation_result == svtValid):
                print("Signature validated successfully.")
            else:
                print("Verification unknown.") 
    except Exception as e: 
        print(e)



