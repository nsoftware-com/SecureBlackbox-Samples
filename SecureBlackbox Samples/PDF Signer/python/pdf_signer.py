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
        "  pdfsigner -- SecureBlackbox PDFSigner Demo Application\n\n"
        "SYNOPSIS\n"
        "  pdfsigner [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password]\n"
        "            [-level signature_level] [-author author] [-signame sigature_name] [-autorevinfo]n\n"
        "DESCRIPTION\n"
        "  PDFSigner demonstrates the usage of PDFSigner from SecureBlackbox.\n"
        "  Used to sign pdf documents.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the signed file will be saved (Required).\n\n"
        "  -cert         The certificate used to sign file (Required).\n\n"
        "  -certpass     The password for the certificate (Required).\n\n"
        "  -level        The level for signatures. Enter the corresponding number. Valid values:\n\n"
        "                  0  - Legacy (Default)\n"
        "                  1  - BES\n"
        "                  2  - EPES\n"
        "                  3  - LTV\n"
        "                  4  - DocumentTimestamp\n"
        "  -author       The name of the signer who produced this signature.\n\n"
        "  -reason       Specifies the reason of the signing, for example to confirm the document correctness.\n\n"
        "  -signame      Specifies the signature identifier in the PDF-file.\n\n"
        "  -autorevinfo  Whether revocation info should be collected automatically.\n\n"
        "EXAMPLES\n"
        "  pdfsigner -input C:\\pdf\\helloworld.pdf -output C:\\pdf\\mypdf.scs -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n"
        "  pdfsigner -input C:\\pdf\\helloworld.pdf -output C:\\pdf\\mypdf.scs -cert C:\\certs\\mycert.pfx -certpass mypassword -level 1 -author \"Author name\" \n\n"
        "       -reason \"Test reason\" -signame \"Signature name\" -autorevinfo \n\n"
    )

if (len(sys.argv) < 9):
    displayHelp()
    sys.exit(1)
else:
    signer = PDFSigner()
    cm = CertificateManager()
    
    inputF = ""
    outputF = ""
    certF = ""
    certpass = ""
    autorevinfo = False
    
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
            if (sys.argv[x].lower() == "-autorevinfo"):
                autorevinfo = True

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
    
    if (certpass == ""):
        print("-certpass is required.\n")
        displayHelp()
        sys.exit(1)
    
    if autorevinfo:
        signer.config("AutoCollectRevocationInfo=true")
    else:
        signer.config("AutoCollectRevocationInfo=false")
    
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



