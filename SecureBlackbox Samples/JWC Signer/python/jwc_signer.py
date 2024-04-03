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
        "  jwsigner -- SecureBlackbox SymmetricCrypto Demo Application\n\n"
        "SYNOPSIS\n"
        "  jwsigner -s/-v [-input input_data] [-sig signature_data] [-cert certificate_file] [-certpass certificate_password] [-compact]\n\n"
        "DESCRIPTION\n"
        "  This sample illustrates how to create a detached signature over a text string.\n"
        "  Used to sign and verify data.\n\n"
        "  The options are as follows:\n\n"
        "  -s            Whether to sign input data. \n\n"
        "  -v            Whether to verify signature data. \n\n"
        "  -input        An input data to sign/verify (Required). \n\n"
        "  -sig          An signature data to verify (Required to verify). \n\n"
        "  -cert         The certificate used to encrypt file (Required). \n\n"
        "  -certpass     The password for the certificate. \n\n"
        "  -compact      Whether to use compact format \n\n"
        "EXAMPLES\n"
        "	jwsigner -s -input \"And now that you don’t have to be perfect, you can be good.\" -cert C:\\\\certs\\\\mycert.pfx -certpass mypassword \n\n"
        "	jwsigner -v -input \"And now that you don’t have to be perfect, you can be good.\" \n"
        "		-sig eyJhbGciOiJkaXIiLCJlbmMiOiJBMjU2Q0JDLUhTNTEyIn0..kuN2U -cert C:\\\\certs\\\\mycert.pfx -certpass mypassword -compact \n\n"
    )

if (len(sys.argv) < 6):
    displayHelp()
    sys.exit(1)
else:
    crypto = PublicKeyCrypto()
    cm = CertificateManager()
    ckm = CryptoKeyManager()
    
    sign = False
    verify = False
    inputD = ""
    signatureD = ""
    certF = ""
    certpass = ""
    compact = False
    
    for x in range(len(sys.argv)):
        if (sys.argv[x].startswith("-")):
            if (sys.argv[x].lower() == "-s"):
                sign = True
            if (sys.argv[x].lower() == "-v"):
                verify = True
            if (sys.argv[x].lower() == "-input"):
                inputD = sys.argv[x+1]
            if (sys.argv[x].lower() == "-sig"):
                signatureD = sys.argv[x+1]
            if (sys.argv[x].lower() == "-cert"):
                certF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-certpass"):
                certpass = sys.argv[x+1]
            if (sys.argv[x].lower() == "-compact"):
                compact = True

    if (not (sign or verify)):
        print("-s or -v is required.\n")
        displayHelp()
        sys.exit(1)

    if (sign and verify):
        print("-Use only one -s or -v parameter.\n")
        displayHelp()
        sys.exit(1)

    if (inputD == ""):
        print("-input is required.\n")
        displayHelp()
        sys.exit(1)
        
    if (verify and (signatureD == "")):
        print("-signature is required.\n")
        displayHelp()
        sys.exit(1)
    
    if (certF == ""):
        print("-cert is required.\n")
        displayHelp()
        sys.exit(1)
            
    try:
        cm.import_from_file(certF, certpass)
        ckm.set_cert_handle(cm.get_cert_handle())
        ckm.import_from_cert()
        crypto.set_key_handle(ckm.get_key_handle())
      
        inputB = inputD.encode('utf-8')
        
        if sign:
            if compact:
                crypto.set_output_encoding(3); # cetCompact
            else:
                crypto.set_output_encoding(4); # cetJSON
                
            signatureB = crypto.sign(inputB, True)         
            print("Signature: " + signatureB.decode('utf-8', errors='backslashreplace'))
        else:
            if compact:
                crypto.set_input_encoding(3); # cetCompact
            else:
                crypto.set_input_encoding(4); # cetJSON
                
            signatureB = signatureD.encode('utf-8')
            
            crypto.verify_detached(inputB, signatureB)

            if (crypto.get_signature_validation_result == svtSignerNotFound):
                print("Signer not found")
            elif (crypto.get_signature_validation_result == svtFailure):
                print("Signature verification failed")
            elif (crypto.get_signature_validation_result == svtCorrupted):
                print("Signature is invalid")
            elif (crypto.get_signature_validation_result == svtCorrupted):
                print("Signature validated successfully.")
            else:
                print("Verification unknown.") 
    except Exception as e: 
        print(e)



