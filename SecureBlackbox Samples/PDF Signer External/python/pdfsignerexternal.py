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
		"  pdfsignerexternal -- SecureBlackbox PDFSigner Demo Application\n\n"
		"SYNOPSIS\n"
		"  pdfsignerexternal <-input input_file> <-output output_file> <-key key_file> <-cert certificate_file> \n"
		"                [-certpass certificate_password] [-level sig_level]\n\n"
		"DESCRIPTION\n"
		"  This sample illustrates the use of PDFSigner component for signing PDF documents.\n\n"
		"  The options are as follows:\n\n"
		"  -input        An input file to sign (Required).\n\n"
		"  -output       Where the signed file will be saved (Required).\n\n"
		"  -key          The key file to be imported (Required).\n\n"
		"  -cert         The certificate used to sign files (Required).\n\n"
		"  -certpass     The password for the signing certificate.\n\n"
		"  -level        The level for PAdES signatures. Enter the corresponding number. Valid values:\n\n"
		"                  0  - PASL_UNKNOWN\n"
		"                  1  - PASL_GENERIC\n"
		"                  2  - PASL_BASELINE_B\n"
		"                  3  - PASL_BASELINE_T\n"
		"                  4  - PASL_BASELINE_LT\n"
		"                  5  - PASL_BASELINE_LTA\n"
		"                  6  - PASL_BES\n"
		"                  7  - PASL_EPES\n"
		"                  8  - PASL_LTV\n\n"
		"EXAMPLES\n"
		"  pdfsignerexternal -input C:\\helloworld.pdf -output C:\\sign.pdf -key C:\\certs\\mykey.key -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)

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
    
if (len(sys.argv) <= 1):
    displayHelp("")
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
            if (sys.argv[x].lower() == "-level"):
                signer.set_new_sig_level(int(sys.argv[x+1]))

    if (inputF == ""):
        displayHelp("-input is required.")
        sys.exit(1)
    
    if (outputF == ""):
        displayHelp("-output is required.")
        sys.exit(1)
    
    if (certF == ""):
        displayHelp("-cert is required.")
        sys.exit(1)
    
    if (keyF == ""):
        displayHelp("-key is required.")
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



