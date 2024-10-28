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
        "  simplepdfsigner -- SecureBlackbox PDFSigner Demo Application\n\n"
        "SYNOPSIS\n"
		"  simplepdfsigner <-input input_file> <-output output_file> [-cert certificate_file] [-certpass certificate_password]\n"
        "             [-pkcs11 pkcs11_file] [-pin pkcs11_pin] [-win32 win32_name]\n\n"
        "DESCRIPTION\n"
        "  PDFSigner demonstrates the usage of PDFSigner from SecureBlackbox.\n"
        "  This sample illustrates the use of PDFSigner component for signing PDF documents. \n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the signed file will be saved (Required).\n\n"
        "  -cert         The certificate used to sign files.\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
        "  -pkcs11       The pkcs11 storage used to sign file.\n\n"
        "  -pin          The PIN for pkcs11 storage\n\n"
        "  -win32        The win32 store name\n\n"
        "EXAMPLES\n"
        "  simplepdfsigner -input C:\\pdf\\myfile.pdf -output C:\\pdf\\mysignedfile.pdf -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n"
        "  simplepdfsigner -input C:\\pdf\\myfile.pdf -output C:\\pdf\\mysignedfile.pdf -pkcs11 C:\\pkcs11\\pkcs11.dll -pin mypassword \n\n"
        "  simplepdfsigner -input C:\\pdf\\myfile.pdf -output C:\\pdf\\mysignedfile.pdf -win32 My \n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)

if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    signer = PDFSigner()
    cm = CertificateManager()
    cs = CertificateStorage()
    
    inputF = ""
    outputF = ""
    certF = ""
    certpass = ""
    pkcs11File = ""
    pin = ""
    win32Store = ""
    
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
            if (sys.argv[x].lower() == "-pkcs11"):
                pkcs11File = sys.argv[x+1]
            if (sys.argv[x].lower() == "-pin"):
                pin = sys.argv[x+1]
            if (sys.argv[x].lower() == "-win32"):
                win32Store = sys.argv[x+1]

    if (inputF == ""):
        print("-input is required.")
        displayHelp()
        sys.exit(1)
    
    if (outputF == ""):
        print("-output is required.")
        displayHelp()
        sys.exit(1)
    
    if ((certF == "") and (pkcs11File == "") and (win32Store == "")):
        print("-cert or -pkcs11 or -win32 is required.")
        displayHelp()
        sys.exit(1)
    
    if ((certF != "") and (pkcs11File != "") or (certF != "") and (win32Store != "") or (pkcs11File != "") and (win32Store != "")):
        print("Use only one -cert or -pkcs11 or -win32 parameter.")
        displayHelp()
        sys.exit(1)
    
    
    if (certF != ""):   
        cm.import_from_file(certF, certpass)
        signer.set_signing_cert_handle(cm.get_cert_handle())
    else:
        if (pkcs11File != ""):
            cs.open("pkcs11://user:" + pin + "@/" + pkcs11File)
        else:
            cs.open("system://?store=" + win32Store)
        signer.set_signing_cert_handle(cs.get_cert_handle(0))
            
        
    if (cm.get_cert_key_algorithm() == "id-dsa"):
      signer.set_sig_hash_algorithm("SHA1")
    
    try:
        signer.set_input_file(inputF)
        signer.set_output_file(outputF)

        signer.set_new_sig_level(6)  #paslBES
        signer.set_widget_invisible(False)
        signer.set_ignore_chain_validation_errors(True)
        
        signer.sign()
    
        print("The document was signed successfully.")
    except Exception as e: 
        print(e)





