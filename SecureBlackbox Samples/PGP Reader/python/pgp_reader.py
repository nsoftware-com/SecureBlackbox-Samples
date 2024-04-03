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
        "  pgpreader -- SecureBlackbox PGPReader Demo Application\n\n"
        "SYNOPSIS\n"
        "  pgpreader [-input input_file] [-output output_file] [-pubkey public_key_file] [-seckey secret_key_file]\n"
        "            [-keypass keys_password] [-pass encryption_password] \n\n"
        "DESCRIPTION\n"
        "  PGPReader demonstrates the usage of PGPReader from SecureBlackbox.\n"
        "  Used to decrypted and verify OpenPGP-compliant files. \n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to decrypt and verify (Required).\n\n"
        "  -output       Where the decrypted file will be saved (Required).\n\n"
        "  -pubkey       The public key used to verify file (Required).\n\n"
        "  -seckey       The secret (private) key used to decrypt file (Required).\n\n"
        "  -keypass      The password for the keys (Required).\n\n"
        "  -pass         The password for decryption (Required).\n\n"
    )

def fireKeyPassphraseNeeded(e):
    e.passphrase = keypass
    
def translateSigValidity(value):
    if (value == 0): # psvValid
        return "Valid"
    elif (value == 1): # psvCorrupted
        return "Corrupted"
    elif (value == 2): # psvUnknownAlgorithm
        return "Unknown algorithm"
    elif (value == 3): # psvNoKey
        return "No key"
    else:
        return "Unknown"

if (len(sys.argv) < 9):
    displayHelp()
    sys.exit(1)
else:
    reader = PGPReader()    
    reader.on_key_passphrase_needed = fireKeyPassphraseNeeded
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
                reader.set_passphrase(sys.argv[x+1])

    if (inputF == ""):
        print("-input is required.\n")
        displayHelp()
        sys.exit(1)
    
    if (outputF == ""):
        print("-output is required.\n")
        displayHelp()
        sys.exit(1)
    
    if (pubkeyF == ""):
        print("-pubkey is required.\n")
        displayHelp()
        sys.exit(1)
    
    if (seckeyF == ""):
        print("-seckey is required.\n")
        displayHelp()
        sys.exit(1)
        
    try:
        keyring.load(pubkeyF, seckeyF)
        
        reader.set_decrypting_key_count(keyring.get_public_key_count())
        for x in range(keyring.get_public_key_count()):
            reader.set_decrypting_key_handle(x, keyring.get_public_key_handle(x))
                       
        reader.set_verifying_key_count(keyring.get_secret_key_count())
        for x in range(keyring.get_secret_key_count()):
            reader.set_verifying_key_handle(x, keyring.get_secret_key_handle(x))

        reader.set_input_file(inputF)
        reader.set_output_file(outputF)
        reader.decrypt_and_verify()
    
        print("Signatures:")
        for x in range(reader.get_signature_count()):
            username = "No name"

            for y in range(keyring.get_public_key_count()):
                if (not keyring.get_public_key_is_subkey(y) and (keyring.get_public_key_key_id(y) == reader.get_signature_signer_key_id(x))):
                    username = keyring.get_public_key_username(y)
                    break

            print(username + " - " + translateSigValidity(reader.get_signature_validity(x)))
        
        print("\nThe file were decrypted successfully.")
    except Exception as e: 
        print(e)



