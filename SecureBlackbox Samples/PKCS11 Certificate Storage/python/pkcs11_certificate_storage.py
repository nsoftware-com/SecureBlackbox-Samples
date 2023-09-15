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
        "  pkcs11certificatestorage -- SecureBlackbox CertificateStorage Demo Application\n\n"
        "SYNOPSIS\n"
        "  pkcs11certificatestorage [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password]\n"
        "             [-signbody] [-hashalg hash_algorithm] [-sigtype signature_type]\n\n"
        "DESCRIPTION\n"
        "  CertificateStorage demonstrates the usage of CertificateStorage from SecureBlackbox.\n"
        "  This sample illustrates the use of CertificateStorage component for work with PKCS11 storage. \n\n"
        "  The options are as follows:\n\n"
        "  -storage      The pkcs11 storage file (Required).\n\n"
        "  -pin          The PIN for pkcs11 storage\n\n"
        "EXAMPLES\n"
        "  pkcs11certificatestorage -storage C:\\pkcs11\\pkcs11.dll -pin mypassword \n\n"
    )

if (len(sys.argv) < 3):
    displayHelp()
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
        print("-storage is required.\n")
        displayHelp()
        sys.exit(1)
    
    try:
        cs.open("pkcs11:///" + storageFile + "?slot=-1")

        slotCount = int(cs.config("PKCS11SlotCount"))

        for x in range(slotCount):
            desc = cs.config("PKCS11SlotDescription[" + str(x) + "]")
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



