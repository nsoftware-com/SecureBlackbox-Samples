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
        "  pgpwriter -- SecureBlackbox PGPWriter Demo Application\n\n"
        "SYNOPSIS\n"
		"  pgpwriter <-input input_file> <-output output_file> <-pubkey public_key_file> <-seckey secret_key_file>\n"
		"            <-keypass keys_password> <-pass encryption_password> \n\n"
        "DESCRIPTION\n"
        "  PGPWriter demonstrates the usage of PGPWriter from SecureBlackbox.\n"
        "  Used to create encrypted and signed OpenPGP-compliant files. \n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the ASiC will be saved (Required).\n\n"
        "  -pubkey       The public key used to encrypt file (Required).\n\n"
        "  -seckey       The secret (private) key used to sign file (Required).\n\n"
        "  -keypass      The password for the keys (Required).\n\n"
        "  -pass         The password for encryption (Required).\n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)

def fireKeyPassphraseNeeded(e):
    e.passphrase = keypass
    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    writer = PGPWriter()    
    writer.on_key_passphrase_needed = fireKeyPassphraseNeeded
    keyring = PGPKeyring()
    
    inputF = ""
    outputF = ""
    pubkeyF = ""
    seckeyF = ""
    keypass = ""
    
    for x in range(len(sys.argv)):
        if (sys.argv[x].startswith("-")):
            if (sys.argv[x].lower() == "-input"):
                inputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-output"):
                outputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-pubkey"):
                pubkeyF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-seckey"):
                seckeyF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-keypass"):
                keypass = sys.argv[x+1]
            if (sys.argv[x].lower() == "-pass"):
                writer.set_passphrase(sys.argv[x+1])

    if (inputF == ""):
        displayHelp("-input is required.")
        sys.exit(1)
    
    if (outputF == ""):
        displayHelp("-output is required.")
        sys.exit(1)
    
    if (pubkeyF == ""):
        displayHelp("-pubkey is required.")
        sys.exit(1)
    
    if (seckeyF == ""):
        displayHelp("-seckey is required.")
        sys.exit(1)
        
    try:
        keyring.import_from_file(pubkeyF)
        keyring.import_from_file(seckeyF)

        writer.set_signing_key_count(1)
        for x in range(keyring.get_key_count()):
            if keyring.get_key_is_secret(x):
                writer.set_signing_key_handle(0, keyring.get_key_handle(x))
                break
            
        writer.set_encrypting_key_count(1)
        for x in range(keyring.get_key_count()):
            if keyring.get_key_is_public(x):
                writer.set_encrypting_key_handle(0, keyring.get_key_handle(x))
                break

        writer.set_input_file(inputF)
        writer.set_output_file(outputF)
        writer.encrypt_and_sign()
    
        print("The file were encrypted and signed successfully.")
    except Exception as e: 
        print(e)





