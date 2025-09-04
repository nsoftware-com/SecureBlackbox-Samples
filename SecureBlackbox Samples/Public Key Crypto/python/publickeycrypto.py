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
        "  publickeycrypto -- SecureBlackbox PublicKeyCrypto Demo Application\n\n"
        "SYNOPSIS\n"
		"  publickeycrypto <-s/-v> <-input input_file> <-cert certificate_file> [-certpass certificate_password] \n"
        "             [-output output_file] [-sig signature_file] [-encoding encoding]\n\n"
        "DESCRIPTION\n"
        "  PublicKeyCrypto demonstrates the usage of PublicKeyCrypto from SecureBlackbox.\n"
        "  Used to sign and verify files.\n\n"
        "  The options are as follows:\n\n"
        "  -s            Sign input file and save to output \n\n"
        "  -v            Verify signature using original file (input) \n\n"
        "  -input        An input file to sign or verify (Required). \n\n"
        "  -cert         The certificate used to sign files (Required).\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
        "  -output       Where the signed file will be saved (Required for signing). \n\n"
        "  -sig          An signature file to verify (Required on verifing). \n\n"
        "  -encoding     The encoding of hash. Valid values:\n\n"
        "                  0 - CET_DEFAULT\n"
        "                  1 - CET_BINARY\n"
        "                  2 - CET_BASE_64\n"
        "                  3 - CET_COMPACT\n"
        "                  4 - CET_JSON\n\n"
        "EXAMPLES\n"
        "  publickeycrypto -s -input C:\\cypto\\helloworld.txt -output C:\\cypto\\signature.dat -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n"
        "  publickeycrypto -v -input C:\\cypto\\helloworld.txt -sig C:\\cypto\\signature.dat -cert C:\\certs\\mycert.pfx -certpass mypassword -encoding 2 \n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)

if (len(sys.argv) <= 1):
    displayHelp("")
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
    encoding = 2
    
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
            if (sys.argv[x].lower() == "-sig"):
                signatureF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-cert"):
                certF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-certpass"):
                certpass = sys.argv[x+1]
            if (sys.argv[x].lower() == "-encoding"):
                encoding = int(sys.argv[x+1])

    if (not (sign or verify)):
        displayHelp("-s or -v is required.")
        sys.exit(1)

    if (sign and verify):
        displayHelp("-Use only one -s or -v parameter.")
        sys.exit(1)

    if (inputF == ""):
        displayHelp("-input is required.")
        sys.exit(1)
    
    if (sign and (outputF == "")):
        displayHelp("-output is required.")
        sys.exit(1)
    
    if (verify and (signatureF == "")):
        displayHelp("-signature is required.")
        sys.exit(1)
    
    if (certF == ""):
        displayHelp("-cert is required.")
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

            if (crypto.get_signature_validation_result() == svtSignerNotFound):
                print("Signer not found")
            elif (crypto.get_signature_validation_result() == svtFailure):
                print("Signature verification failed")
            elif (crypto.get_signature_validation_result() == svtCorrupted):
                print("Signature is invalid")
            elif (crypto.get_signature_validation_result() == svtValid):
                print("Signature validated successfully.")
            else:
                print("Verification unknown.") 
    except Exception as e: 
        print(e)





