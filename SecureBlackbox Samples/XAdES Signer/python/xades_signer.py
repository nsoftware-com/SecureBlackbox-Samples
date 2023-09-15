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
        "  xadessigner -- SecureBlackbox XAdESSigner Demo Application\n\n"
        "SYNOPSIS\n"
        "  xadessigner [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password] \\\n"
        "              [-version version] [-hashalg hash_algorithm] [-canonmethod canon_method] [-tsserver timestamp_server] \\\n"
        "              [-form form] [-includekey] [-detached] \n\n"
        "DESCRIPTION\n"
        "  XAdESSigner demonstrates the usage of XAdESSigner from SecureBlackbox.\n"
        "  Used to create an XML Extended Signature (XAdES) from an XML file.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the XAdES will be saved (Required).\n\n"
        "  -cert         The certificate used to sign files (Required).\n\n"
        "  -certpass     The password for the signing certificate (Required).\n\n"
        "  -version      The XAdES version to use. Enter the corresponding number. Valid values:\n\n"
        "                  1 - 1.1.1\n"
        "                  2 - 1.2.2\n"
        "                  3 - 1.3.2\n"
        "                  4 - 1.4.1\n\n"
        "  -form         The XAdES form to use. Enter the corresponding number. Valid values:\n\n"
        "                  0  - UNKNOWN \n"
        "                  1  - BASIC \n"
        "                  2  - BES \n"
        "                  3  - EPES \n"
        "                  4  - T \n"
        "                  5  - C \n"
        "                  6  - X \n"
        "                  7  - XL \n"
        "                  8  - A \n"
        "                  9  - EXTENDED_BES \n"
        "                  10 - EXTENDED_EPES \n"
        "                  11 - EXTENDED_T \n"
        "                  12 - EXTENDED_C \n"
        "                  13 - EXTENDED_X \n"
        "                  14 - EXTENDED_XLONG \n"
        "                  15 - EXTENDED_XL \n"
        "                  16 - EXTENDED_A\n\n"
        "  -detached     Whether the signature is detached.\n\n"
        "  -canonmethod  The canonicalization method to use. Enter the corresponding number. Valid values:\n\n"
        "                  0 - NONE\n"
        "                  1 - CANON\n"
        "                  2 - CANON_COMMENT\n"
        "                  3 - EXCL_CANON\n"
        "                  4 - EXCL_CANON_COMMENT\n"
        "                  5 - MIN_CANON\n"
        "                  6 - CANON_V_1_1\n"
        "                  7 - CANON_COMMENT_V_1_1\n\n"
        "  -hashalg      The hashing algorithm to use. Valid values: SHA1, MD5, SHA256, SHA384, SHA512, RIPEMD160\n\n"
        "  -tsserver     A timestamp server to use during signing.\n\n"
        "  -includekey   Whether to include the public key in the signature.\n\n"
        "EXAMPLES\n"
        "  xadessigner -input C:\\xades\\myfile.xml -output C:\\xades\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
        "  xadessigner -input C:\\xades\\myfile.xml -output C:\\xades\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword \\\n"
        "              -version 3 -form 12 -canonmethod 3 -hashalg SHA1 -detached -tsserver http://time.certum.pl\n\n"
    )
    
if (len(sys.argv) < 9):
    displayHelp()
    sys.exit(1)
else:
    signer = XAdESSigner()
    cm = CertificateManager()
    
    inputF = ""
    outputF = ""
    certF = ""
    certpass = ""
    detached = False
    
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
            if (sys.argv[x].lower() == "-version"):
                signer.set_new_sig_xades_version(int(sys.argv[x+1]))
            if (sys.argv[x].lower() == "-form"):
                signer.set_new_sig_xades_form(int(sys.argv[x+1]))
            if (sys.argv[x].lower() == "-detached"):
                detached = True
            if (sys.argv[x].lower() == "-canonmethod"):
                signer.set_new_sig_canonicalization_method(int(sys.argv[x+1]))
            if (sys.argv[x].lower() == "-hashalg"):
                signer.set_new_sig_hash_algorithm(sys.argv[x+1])
            if (sys.argv[x].lower() == "-tsserver"):
                signer.set_timestamp_server(sys.argv[x+1])
            if (sys.argv[x].lower() == "-includekey"):
                signer.config("IncludeKey=true")
    
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
        signer.set_output_file(outputF)
    
        if detached:
            signer.set_data_file(inputF)
            signer.set_data_type(1) # cxdtBinary
            signer.set_data_uri(os.path.basename(inputF))
            signer.set_new_sig_signature_type(1) # cxstDetached
        else:
            signer.set_input_file(inputF)
            signer.set_new_sig_signature_type(4) # cxstEnveloped
    
        cm.import_from_file(certF, certpass)
        signer.set_signing_cert_handle(cm.get_cert_handle())

        signer.sign()
    
        print("XAdES created.")
    except Exception as e: 
        print(e)



