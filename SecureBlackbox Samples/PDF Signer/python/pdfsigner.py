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
		"  pdfsigner -- SecureBlackbox PDFSigner Demo Application\n\n"
		"SYNOPSIS\n"
		"  pdfsigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password]\n"
		"             [-level sig_level] [-hashalg hashalg] [-author author] [-reason reason] [-signame signame]\n\n"
		"DESCRIPTION\n"
		"  This sample illustrates the use of PDFSigner component for signing PDF documents.\n\n"
		"  The options are as follows:\n\n"
		"  -input        An input file to sign (Required).\n\n"
		"  -output       Where the signed file will be saved (Required).\n\n"
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
		"  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224, MD5\n\n"
		"  -author       The name of the signer who produced this signature.\n\n"
		"  -reason       Specifies the reason of the signing, for example to confirm the document correctness.\n\n"
		"  -signame      Specifies the signature identifier in the PDF-file.\n\n"
		"EXAMPLES\n"
		"  pdfsigner -input C:\\helloworld.pdf -output C:\\sign.pdf -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
		"  pdfsigner -input C:\\helloworld.pdf -output C:\\sign.pdf -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
		"           -hashalg SHA256 -level 1 -author \"Test author\"\n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)

if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    signer = PDFSigner()
    cm = CertificateManager()
    
    inputF = ""
    outputF = ""
    certF = ""
    certpass = ""
    
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
            if (sys.argv[x].lower() == "-level"):
                signer.set_new_sig_level(int(sys.argv[x+1]))
            if (sys.argv[x].lower() == "-author"):
                signer.set_new_sig_author_name(sys.argv[x+1])
            if (sys.argv[x].lower() == "-reason"):
                signer.set_new_sig_reason(sys.argv[x+1])
            if (sys.argv[x].lower() == "-signame"):
                signer.set_new_sig_signature_name(sys.argv[x+1])
            if (sys.argv[x].lower() == "-hashalg"):
                signer.set_new_sig_hash_algorithm(sys.argv[x+1])

    if (inputF == ""):
        displayHelp("-input is required.")
        sys.exit(1)
    
    if (outputF == ""):
        displayHelp("-output is required.")
        sys.exit(1)
    
    if (certF == ""):
        displayHelp("-cert is required.")
        sys.exit(1)
    
    cm.import_from_file(certF, certpass)
    signer.set_signing_cert_handle(cm.get_cert_handle())

    if (cm.get_cert_key_algorithm() == "id-dsa"):
      signer.set_new_sig_hash_algorithm("SHA1")
    
    try:
        signer.set_input_file(inputF)
        signer.set_output_file(outputF)

        signer.sign()
    
        print("The document was signed successfully.")
    except Exception as e: 
        print(e)





