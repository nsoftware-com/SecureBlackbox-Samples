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
        "  symmetriccrypto -- SecureBlackbox SymmetricCrypto Demo Application\n\n"
        "SYNOPSIS\n"
		"  symmetriccrypto <-e/-d> <-input input_file> <-output output_file> <-pass encryption_password> [-encoding encoding]\n\n"
        "DESCRIPTION\n"
        "  SymmetricCrypto demonstrates the usage of SymmetricCrypto from SecureBlackbox.\n"
        "  Used to sign and verify files.\n\n"
        "  The options are as follows:\n\n"
        "  -e            Encrypt input file and save to output \n\n"
        "  -d            Decrypt input file and save to output \n\n"
        "  -input        An input file to encrypt or decrypt (Required). \n\n"
        "  -output       Where the encrypted or decrypted file will be saved (Required). \n\n"
        "  -pass         The password for encryption/decryption (Required).\n\n"
        "  -encoding     The encoding of hash. Valid values:\n\n"
        "                  0 - CET_DEFAULT\n"
        "                  1 - CET_BINARY\n"
        "                  2 - CET_BASE_64\n"
        "                  3 - CET_COMPACT\n"
        "                  4 - CET_JSON\n\n"
        "EXAMPLES\n"
        "  symmetriccrypto -e -input C:\\cypto\\helloworld.txt -output C:\\cypto\\helloworld.enc -pass mypassword \n\n"
        "  symmetriccrypto -d -input C:\\cypto\\helloworld.enc -output C:\\cypto\\helloworld.txt -pass mypassword -encalg 3DES -encoding 2 \n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)

if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    crypto = SymmetricCrypto()
    ckm = CryptoKeyManager()
    
    encrypt = False
    decrypt = False
    inputF = ""
    outputF = ""
    password = ""
    encoding = 0
    
    for x in range(len(sys.argv)):
        if (sys.argv[x].startswith("-")):
            if (sys.argv[x].lower() == "-e"):
                encrypt = True
            if (sys.argv[x].lower() == "-d"):
                decrypt = True
            if (sys.argv[x].lower() == "-input"):
                inputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-output"):
                outputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-pass"):
                password = sys.argv[x+1]
            if (sys.argv[x].lower() == "-encoding"):
                encoding = int(sys.argv[x+1])

    if (not (encrypt or decrypt)):
        displayHelp("-e or -d is required.")
        sys.exit(1)

    if (encrypt and decrypt):
        displayHelp("-Use only one -e or -d parameter.")
        sys.exit(1)

    if (inputF == ""):
        displayHelp("-input is required.")
        sys.exit(1)
    
    if (outputF == ""):
        displayHelp("-output is required.")
        sys.exit(1)
        
    if (password == ""):
        displayHelp("-pass is required.")
        sys.exit(1)
        
    try:     
        crypto.set_encryption_algorithm("AES256")

        # PasswordToKey
        keybits = 256

        ckm.derive_key(keybits, password, "")
        
        iv = bytearray(16)
        ckm.set_key_iv(iv)
        
        crypto.set_key_handle(ckm.get_key_handle())

        if encrypt:
            crypto.set_output_encoding(encoding)
            crypto.encrypt_file(inputF, outputF)
            print("The file was encrypted successfully.")
        else:
            crypto.set_input_encoding(encoding)
            crypto.decrypt_file(inputF, outputF)
            print("The file was decrypted successfully.") 
    except Exception as e: 
        print(e)





