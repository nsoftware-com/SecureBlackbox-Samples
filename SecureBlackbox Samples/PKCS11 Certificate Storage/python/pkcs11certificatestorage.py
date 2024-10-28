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
        "  pkcs11certificatestorage -- SecureBlackbox CertificateStorage Demo Application\n\n"
        "SYNOPSIS\n"
        "  pkcs11certificatestorage <-storage driver_path> [-pin pin] [-slot slot_num]\n\n"
        "DESCRIPTION\n"
        "  This sample illustrates the use of CertificateStorage component to access HSMs via PKCS11 interface. \n\n"
        "  The options are as follows:\n\n"
        "  -storage      A path to the pkcs11 driver file (Required).\n\n"
        "  -pin          The user PIN for the device. If no PIN is provided, the sample won't be signing in.\n\n"
        "  -slot         The slot number to use. If not specified, the first slot with a token in will be used. Pass -1 as a slot number to only list the slots.\n\n"
        "EXAMPLES\n"
        "  pkcs11certificatestorage -storage C:\\pkcs11\\pkcs11.dll -pin mypassword\n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)

if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    cs = CertificateStorage()
    cs_dop = CertificateStorage()
    
    storageFile = ""
    pin = ""
    
    for x in range(len(sys.argv)):
        if (sys.argv[x].startswith("-")):
            if (sys.argv[x].lower() == "-storage"):
                storageFile = sys.argv[x+1]
            if (sys.argv[x].lower() == "-pin"):
                pin = sys.argv[x+1]

    if (storageFile == ""):
        displayHelp("-storage is required.")
        sys.exit(1)
    
    try:
        cs.open("pkcs11:///" + storageFile + "?slot=-1")

        slotes = cs.list_stores().splitlines()
        
        for x in range(len(slotes)):
            desc = slotes[x]
            active = cs.config("PKCS11SlotTokenPresent[" + str(x) + "]")

            if not (desc == ""):
                if (active == "True"):
                    print(desc + ":")
                    cs_dop.open("pkcs11://user:" + pin + "@/" + storageFile + "?slot=" + str(x))

                    for y in range(cs_dop.get_cert_count()):
                        print("    Subject:" + cs_dop.get_cert_subject(y))
                        print("    Issuer: " + cs_dop.get_cert_issuer(y))
                        print("    ValidFrom: " + cs_dop.get_cert_valid_from(y))
                        print("    ValidTo: " + cs_dop.get_cert_valid_to(y))
                        print("    Key: " + cs_dop.get_cert_key_algorithm(y) + " (" + str(cs_dop.get_cert_key_bits(y)) + ") \n")
                    
                    print("");
                else:
                    print(desc + ": No token \n");
                    
    except Exception as e: 
        print(e)





