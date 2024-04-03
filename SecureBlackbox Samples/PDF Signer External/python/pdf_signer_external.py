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
        "  pdfsignerexternal -- SecureBlackbox PDFSigner Demo Application\n\n"
        "SYNOPSIS\n"
        "  pdfsignerexternal [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password]\n"
        "             [-key key_file]n\n"
        "DESCRIPTION\n"
        "  PDFSigner demonstrates the usage of PDFSigner from SecureBlackbox.\n"
        "  Used to external sign pdf documents.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the signed file will be saved (Required).\n\n"
        "  -cert         The certificate used to sign file (Required).\n\n"
        "  -certpass     The password for the certificate.\n\n"
        "  -key          The key used to sign file (Required).\n\n"
        "EXAMPLES\n"
        "  pdfsigner -input C:\\pdf\\helloworld.pdf -output C:\\pdf\\mypdf.scs -cert C:\\certs\\mycert.pfx -key C:\\keys\\mykey.pem \n\n"
    )

def fireExternalSign(e):
    km = CryptoKeyManager()
    crypto = PublicKeyCrypto()
    try:
        km.import_from_file(keyF, 3, "", "", e.pars, 0)
        crypto.set_key_handle(km.get_key_handle())
        
        crypto.set_hash_algorithm(e.hash_algorithm)
        crypto.set_input_is_hash(True)
        crypto.set_scheme_params(e.pars)

        inBuf = bytes.fromhex(e.data)
        outBuf = crypto.sign(inBuf, True)

        e.signed_data = outBuf.hex().upper()
    except Exception as e: 
        print(e)    
    
if (len(sys.argv) < 9):
    displayHelp()
    sys.exit(1)
else:
    signer = PDFSigner()  
    signer.on_external_sign = fireExternalSign
    cm = CertificateManager()
    
    inputF = ""
    outputF = ""
    certF = ""
    certpass = ""
    keyF = ""
    
    for x in range(len(sys.argv)):
        if (sys.argv[x].startswith("-")):
            if (sys.argv[x].lower() == "-input"):
                inputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-output"):
                outputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-cert"):
                certF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-certpass"):
                certpass = sys.argv[x+1]
            if (sys.argv[x].lower() == "-key"):
                keyF = sys.argv[x+1]

    if (inputF == ""):
        print("-input is required.\n")
        displayHelp()
        sys.exit(1)
    
    if (outputF == ""):
        print("-output is required.\n")
        displayHelp()
        sys.exit(1)
    
    if (certF == ""):
        print("-cert is required.\n")
        displayHelp()
        sys.exit(1)
    
    if (keyF == ""):
        print("-key is required.\n")
        displayHelp()
        sys.exit(1)
        
    cm.import_from_file(certF, certpass)
    signer.set_signing_cert_handle(cm.get_cert_handle())

    if (cm.get_cert_key_algorithm() == "id-dsa"):
      signer.set_new_sig_hash_algorithm("SHA1")
    
    try:
        signer.set_input_file(inputF)
        signer.set_output_file(outputF)

        signer.set_new_sig_author_name("test demo author")
        signer.set_new_sig_reason("test demo reason")

        signer.set_ignore_chain_validation_errors(True)
        signer.set_external_crypto_mode(2)
            
        signer.sign_external()
    
        print("The document was signed successfully.")
    except Exception as e: 
        print(e)

