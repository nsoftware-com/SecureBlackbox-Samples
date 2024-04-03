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
        "  pdfdecryptor -- SecureBlackbox PDFDecryptor Demo Application\n\n"
        "SYNOPSIS\n"
        "  pdfdecryptor [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password] [-pass user_password] \n\n"
        "DESCRIPTION\n"
        "  PDFDecryptor demonstrates the usage of PDFDecryptor from SecureBlackbox.\n"
        "  Used to decrypt pdf documents.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to decrypt (Required).\n\n"
        "  -output       Where the decrypted file will be saved (Required).\n\n"
        "  -cert         The certificate used to decrypt file.\n\n"
        "  -certpass     The password for the certificate.\n\n"
        "  -pass         The password for decryption.\n\n"
        "EXAMPLES\n"
        "  pdfdecryptor -input C:\\pdf\\mypdf.scs -output C:\\pdf\\helloworld.pdf -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n"
        "  pdfdecryptor -input C:\\pdf\\mypdf.scs -output C:\\pdf\\helloworld.pdf -pass mypassword \n\n"
    )
    
if (len(sys.argv) < 5):
    displayHelp()
    sys.exit(1)
else:
    decryptor = PDFDecryptor()
    cm = CertificateManager()
    
    inputF = ""
    outputF = ""
    certF = ""
    certpass = ""
    password = ""

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
            if (sys.argv[x].lower() == "-pass"):
                password = sys.argv[x+1]
    
    if (inputF == ""):
        print("-input is required.\n")
        displayHelp()
        sys.exit(1)
    
    if (outputF == ""):
        print("-output is required.\n")
        displayHelp()
        sys.exit(1)
    
    if (certF != ""):
        cm.import_from_file(certF, certpass)
        decryptor.set_decryption_certificate_handle(cm.get_cert_handle())
    else:
        decryptor.set_password(password)

    try:
        decryptor.set_input_file(inputF)
        decryptor.set_output_file(outputF)

        decryptor.decrypt()
    
        print("PDF file successfully decrypted.")
    except Exception as e: 
        print(e)



