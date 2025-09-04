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
		"  pdfencryptor -- SecureBlackbox PDFEncryptor Demo Application\n\n"
		"SYNOPSIS\n"
		"  pdfencryptor <-input input_file> <-output output_file> [-cert certificate_file] [-certpass certificate_password]\n"
		"       [-pass password] [-encalg encryption_algorithm] \n\n"
		"DESCRIPTION\n"
		"  This sample illustrates the use of PDFEncryptor component for encrypting PDF documents.\n\n"
		"  The options are as follows:\n\n"
		"  -input        An input PDF file to encrypt (Required).\n\n"
		"  -output       Where the encrypted file will be saved (Required).\n\n"
		"  -cert         The certificate used to encrypt file.\n\n"
		"  -certpass     The password for the encryption certificate.\n\n"
		"  -pass         The password used to encrypt file.\n\n"
		"  -encalg       The encryption algorithm. Valid values: 3DES, RC4, RC2, AES128, AES192, AES256, Twofish128 \n\n"
		"EXAMPLES\n"
		"  pdfencryptor -input C:\\helloworld.pdf -output C:\\enc.pdf -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n"
		"  pdfencryptor -input C:\\helloworld.pdf -output C:\\enc.pdf -pass mypassword -alg AES128 \n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    encryptor = PDFEncryptor()
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
            if (sys.argv[x].lower() == "-encalg"):
                encryptor.set_encryption_algorithm(sys.argv[x+1])
    
    if (inputF == ""):
        displayHelp("-input is required.")
        sys.exit(1)
    
    if (outputF == ""):
        displayHelp("-output is required.")
        sys.exit(1)
    
    if (certF != ""):
        cm.import_from_file(certF, certpass)
        encryptor.set_encryption_certificate_handle(cm.get_cert_handle())
    else:
        encryptor.set_user_password(password)

    try:
        encryptor.set_input_file(inputF)
        encryptor.set_output_file(outputF)

        encryptor.encrypt()
    
        print("PDF file successfully encrypted.")
    except Exception as e: 
        print(e)





