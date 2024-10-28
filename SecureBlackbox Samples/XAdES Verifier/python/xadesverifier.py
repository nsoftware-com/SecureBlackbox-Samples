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
        "  xadesverifier -- SecureBlackbox XAdESverifier Demo Application\n\n"
        "SYNOPSIS\n"
		"  xadesverifier <-input input_file> [-data original_data] [-cert certificate_file] [-certpass certificate_password] \n"
        "                [-detached] [-showsigs] [-showrefs]\n"
        "DESCRIPTION\n"
        "  XAdESVerifier demonstrates the usage of XAdESVerifier from SecureBlackbox.\n"
        "  Used to verify an XML Extended Signature (XAdES) from an XML file.\n\n"
        "  The options are as follows:\n\n"
        "  -input        A signature to verify (Required). If the signature is detached, this will take\n"
        "                the signature file and -data will take the original data.\n\n"
        "  -cert         The certificate used to verify the signature. Required if no key is included in the signature.\n\n"
        "  -certpass     The password for the certificate.\n\n"
        "  -detached     Whether the signature is detached. Use -data to specify the original data.\n\n"
        "  -data         The original data.\n\n"
        "  -showinfo     Whether to display detailed XAdES options used with the signature.\n\n"
        "  -showrefs     Whether to display detailed results of reference verification.\n\n"
        "EXAMPLES\n"
        "  xadesverifier -input C:\\xades\\mysigned.xml\n"
        "  xadesverifier -input C:\\xades\\mysigned.xml -detached -data C:\\xades\\my.xml\n"
        "  xadesverifier -input C:\\xades\\mysigned.xml -cert C:\\certs\\mycert.pfx -certpass test\n"
        "  xadesverifier -input C:\\xades\\mysigned.xml -showsigs -showrefs\n\n"
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
    
def translateChainValidity(value):
    if (value == cvtValid):
        return "Valid"
    elif (value == cvtValidButUntrusted):
        return "Valid but untrusted"
    elif (value == cvtInvalid):
        return "Invalid"
    elif (value == cvtCantBeEstablished):
        return "Can't be established"
    else:
        return "Unknown" 
    
def translateXAdESVersion(ver):
    if (ver == 1):
        return "1.1.1"
    elif (ver == 2):
        return "1.2.2"
    elif (ver == 3):
        return "1.3.2"
    elif (ver == 4):
        return "1.4.1"
    else:
        return "Unknown"

def translateLevel(level):
    if (level == aslGeneric):
        return "Generic (XML-DSIG)"
    elif (level == aslBaselineB):
        return "XAdES Baseline B"
    elif (level == aslBaselineT):
        return "XAdES Baseline T"
    elif (level == aslBaselineLT):
        return "XAdES Baseline LT"
    elif (level == aslBaselineLTA):
        return "XAdES Baseline LTA"
    elif (level == aslBES):
        return "XAdES-BES"
    elif (level == aslEPES):
        return "XAdES-EPES"
    elif (level == aslT):
        return "XAdES-T"
    elif (level == aslC):
        return "XAdES-C"
    elif (level == aslX):
        return "XAdES-X"
    elif (level == aslXL):
        return "XAdES-X-L"
    elif (level == aslA):
        return "XAdES-A"
    elif (level == aslExtendedBES):
        return "XAdES-E-BES"
    elif (level == aslExtendedEPES):
        return "XAdES-E-EPES"
    elif (level == aslExtendedT):
        return "XAdES-E-T"
    elif (level == aslExtendedC):
        return "XAdES-E-C"
    elif (level == aslExtendedX):
        return "XAdES-E-X"
    elif (level == aslExtendedXLong):
        return "XAdES-E-X-Long"
    elif (level == aslExtendedXL):
        return "XAdES-E-X-L"
    elif (level == aslExtendedA):
        return "XAdES-E-A"
    else:
        return "Unknown"

if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    verifier = XAdESVerifier()
    verifier.on_reference_validated = fireReferenceValidated
    cm = CertificateManager()
    
    inputF = ""
    certF = ""
    certpass = ""
    dataF = ""
    detached = False
    showinfo = False
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
                if (sys.argv[x].lower() == "-showinfo"):
                    showinfo = True
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
            print("  Claimed signing time: " + verifier.get_signature_claimed_signing_time(x))
            print("  Timestamp: " + verifier.get_signature_validated_signing_time(x))
            print("  Signature Validation Result: " + translateSigValidity(verifier.get_signature_signature_validation_result(x)))
            print("  Chain Validation Result: " + translateChainValidity(verifier.get_signature_chain_validation_result(x)) + "\n")

            if showinfo and (verifier.get_signature_signature_validation_result(0) == svtValid):
                print("XAdES Detailed Information:")
                print("   XAdES Version: " + translateXAdESVersion(verifier.get_xades_version()))
                print("   Level/XAdES form: " + translateLevel(verifier.get_level()))
    except Exception as e: 
        print(e)





