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
        "  xmlverifier -- SecureBlackbox XMLVerifier Demo Application\n\n"
        "SYNOPSIS\n"
		"  xmlverifier <-input input_file> [-cert certificate_file] [-certpass certificate_password] \n"
        "            [-detached] [-data original_data] [-showrefs]\n\n"
        "DESCRIPTION\n"
        "  XMLVerifier demonstrates the usage of XMLVerifier from SecureBlackbox.\n"
        "  Used to verify an XML Signature from an XML file.\n\n"
        "  The options are as follows:\n\n"
        "  -input        A signature to verify (Required). If the signature is detached, this will take\n"
        "                the signature file and -data will take the original data.\n\n"
        "  -cert         The certificate used to verify the signature. Required if no key is included in the signature.\n\n"
        "  -certpass     The password for the certificate.\n\n"
        "  -detached     Whether the signature is detached. Use -data to specify the original data.\n\n"
        "  -data         The original data.\n\n"
        "  -showrefs     Whether to display detailed results of reference verification.\n\n"
        "EXAMPLES\n"
        "  xmlverifier -input C:\\xml\\mysigned.xml\n"
        "  xmlverifier -input C:\\xml\\mysigned.xml -detached -data C:\\xml\\my.xml\n"
        "  xmlverifier -input C:\\xml\\mysigned.xml -cert C:\\certs\\mycert.pfx -certpass test\n"
        "  xmlverifier -input C:\\xml\\mysigned.xml -showrefs\n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)

def fireReferenceValidated(e):
    if (showrefs):
        valid = "false"
        if (e.digest_valid):
            valid = "true"
        print("%.10s %.22s %.22s %s\n"%(e.id, e.uri, e.ref_type, valid))

def translateSigValidity(value):
    if (value == svtValid):
        return "Valid"
    elif (value == svtCorrupted):
        return "Corrupted"
    elif (value == svtSignerNotFound):
        return "Signer not found"
    elif (value == svtFailure):
        return "Failure"
    elif (value == svtReferenceCorrupted):
        return "Invalid references"
    else:
        return "Unknown" 
    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    verifier = XMLVerifier()
    verifier.on_reference_validated = fireReferenceValidated
    cm = CertificateManager()
    
    inputF = ""
    certF = ""
    certpass = ""
    dataF = ""
    detached = False
    showrefs = False
    
    try:
        for x in range(len(sys.argv)):
            if (sys.argv[x].startswith("-")):
                if (sys.argv[x].lower() == "-input"):
                    inputF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-data"):
                    dataF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-cert"):
                    certF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-certpass"):
                    certpass = sys.argv[x+1]
                if (sys.argv[x].lower() == "-detached"):
                    detached = True
                if (sys.argv[x].lower() == "-showrefs"):
                    showrefs = True
                    
        if (inputF == ""):
            displayHelp("-input is required.")
            sys.exit(1)
            
        if (detached and dataF == ""):
            displayHelp("-data is required if -detached is used.")
            sys.exit(1)

        verifier.set_input_file(inputF)

        if not (certF == ""):
            cm.import_from_file(certF, certpass)
            verifier.set_known_cert_count(1)
            verifier.set_known_cert_handle(0, cm.get_cert_handle())

        if showrefs:
            print("ID URI RefType DigestValid\n--------------------------")
            
        if detached:
            verifier.set_data_file(dataF)
            verifier.set_data_type(1) # cxdtBinary
            verifier.set_data_uri(os.path.basename(dataF))
            verifier.verify_detached()
        else:
            verifier.verify()
        
        for x in range(verifier.get_signature_count()):
            print("Signature " + str(x+1))
            print("  Signature Validation Result: " + translateSigValidity(verifier.get_signature_signature_validation_result(x)))
    except Exception as e: 
        print(e)





