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
        "  officeverifier -- SecureBlackbox OfficeVerifier Demo Application\n\n"
        "SYNOPSIS\n"
		"  officeverifier <-input input_file> [-cert certificate_file] [-certpass certificate_password] [-checkrev] [-ignoreerrors] [-offline]\n"
        "DESCRIPTION\n"
        "  OfficeVerifier demonstrates the usage of OfficeVerifier from SecureBlackbox.\n"
        "  Used to verify the signature.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to verify (Required).\n\n"
        "  -cert         The certificate used to sign files.\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
		"  -checkrev      Whether certificate revocation information should be checked.\n\n"
		"  -ignoreerrors  Whether to ignore chain validation errors.\n\n"
		"  -offline       Whether offline mode be used.\n\n"
		"EXAMPLES\n"
		"  officeverifier -input C:\\myfile.doc -output C:\\sign.doc -offline\n\n"
		"  officeverifier -input C:\\myfile.doc -output C:\\sign.doc -checkrev -ignoreerrors\n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
def translateSigType(value):
    if (value == 1): # ostBinaryCryptoAPI
        return "BinaryCryptoAPI"
    elif (value == 2): # ostBinaryXML
        return "BinaryXML"
    elif (value == 3): # ostOpenXML
        return "OpenXML"
    elif (value == 4): # ostOpenXPS
        return "OpenXPS"
    elif (value == 5): # ostOpenDocument
        return "OpenOffice"
    else:
        return "Unknown"

def translateDocSig(value):
    if (value):
        return "Document content is signed"
    else:
        return "Document content is partially signed"

def translateCore(value):
    if (value):
        return "Document properties are signed"
    else:
        return "Document properties are not signed"

def translateOrigSig(value):
    if (value):
        return "Signature origin is signed"
    else:
        return "Signature origin is not signed"

def translateSigValidity(value):
    if (value == svtValid):
        return "Valid"
    elif (value == svtCorrupted):
        return "Corrupted"
    elif (value == svtSignerNotFound):
        return "Signer not found"
    elif (value == svtFailure):
        return "Failure"
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
    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    verifier = OfficeVerifier()
    cm = CertificateManager()
    
    inputF = ""
    certF = ""
    certpass = ""
    checkrev = False
    ignoreerrors = False
    offline = False
    
    try:
        for x in range(len(sys.argv)):
            if (sys.argv[x].startswith("-")):
                if (sys.argv[x].lower() == "-input"):
                    inputF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-cert"):
                    certF = sys.argv[x+1]
                if (sys.argv[x].lower() == "-certpass"):
                    certpass = sys.argv[x+1]
                if (sys.argv[x].lower() == "-checkrev"):
                    checkrev = True
                if (sys.argv[x].lower() == "-ignoreerrors"):
                    ignoreerrors = True
                if (sys.argv[x].lower() == "-offline"):
                    offline = True
                    
        if (inputF == ""):
            print()
            displayHelp("-input is required.")
            sys.exit(1)

        verifier.set_input_file(inputF)
        
        
        if checkrev:
            verifier.set_revocation_check(1) # Auto
        else:
            verifier.set_revocation_check(0) # None
        
        if ignoreerrors:
            verifier.set_ignore_chain_validation_errors(True) 
        else:
            verifier.set_ignore_chain_validation_errors(False)
            
        if offline:
            verifier.set_offline_mode(True) 
        else:
            verifier.set_offline_mode(False)      
        
        if not (certF == ""):
            cm.import_from_file(certF, certpass)
            verifier.set_known_cert_count(1)
            verifier.set_known_cert_handle(0, cm.get_cert_handle())
            
        verifier.verify()
        
        print("There are %i signatures in this file.\n"%(verifier.get_signature_count()))
        for x in range(verifier.get_signature_count()):
            print("Signature " + str(x+1))
            print("  Signature type: " + translateSigType(verifier.get_signature_signature_type(x)))
            print("  " + translateDocSig(verifier.get_signature_document_signed(x)))
            print("  " + translateCore(verifier.get_signature_core_properties_signed(x)))
            print("  " + translateOrigSig(verifier.get_signature_signature_origin_signed(x)))
            print("  Signature Time: " + verifier.get_signature_claimed_signing_time(x))
            print("  Signature Validation Result: " + translateSigValidity(verifier.get_signature_signature_validation_result(x)))
            print("  Chain Validation Result: " + translateChainValidity(verifier.get_signature_chain_validation_result(x)) + "\n")
    except Exception as e: 
        print(e)





