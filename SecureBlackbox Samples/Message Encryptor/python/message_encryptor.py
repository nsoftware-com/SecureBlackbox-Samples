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
        "  messageencryptor -- SecureBlackbox MessageEncryptor Demo Application\n\n"
        "SYNOPSIS\n"
        "  messageencryptor [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password] [-encalg encryption_algorithm] \n\n"
        "DESCRIPTION\n"
        "  MessageEncryptor demonstrates the usage of MessageEncryptor from SecureBlackbox.\n"
        "  Used to create encrypted ('enveloped') PKCS#7 messages. \n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to encrypt (Required).\n\n"
        "  -output       Where the encrypted file will be saved (Required).\n\n"
        "  -cert         The certificate used to encrypt file (Required).\n\n"
        "  -certpass     The password for the encryption certificate (Required).\n\n"
        "  -encalg       The encryption algorithm. Valid values: 3DES, RC4, RC2, AES128, AES192, AES256, Twofish128 \n\n"
        "EXAMPLES\n"
        "  messageencryptor -input C:\\pkcs7\\helloworld.txt -output C:\\pkcs7\\enc.pkcs7 -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n"
        "  messageencryptor -input C:\\pkcs7\\helloworld.txt -output C:\\pkcs7\\enc.pkcs7 -cert C:\\certs\\mycert.pfx -certpass mypassword -alg AES128 \n\n"
    )
    
if (len(sys.argv) < 9):
    displayHelp()
    sys.exit(1)
else:
    encryptor = MessageEncryptor()
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
            if (sys.argv[x].lower() == "-encalg"):
                encryptor.set_encryption_algorithm(sys.argv[x+1])
    
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

    try:
        encryptor.set_input_file(inputF)
        encryptor.set_output_file(outputF)
    
        cm.import_from_file(certF, certpass)
        encryptor.set_encryption_cert_handle(cm.get_cert_handle())

        encryptor.encrypt()
    
        print("The file successfully encrypted.")
    except Exception as e: 
        print(e)



