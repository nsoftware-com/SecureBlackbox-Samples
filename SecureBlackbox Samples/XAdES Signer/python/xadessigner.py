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
        "  xadessigner -- SecureBlackbox XAdESSigner Demo Application\n\n"
        "SYNOPSIS\n"
		"  xadessigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password] \n"
        "              [-version version] [-hashalg hash_algorithm] [-canonmethod canon_method] [-tsserver timestamp_server] \n"
        "              [-level level] [-includekey] [-detached] \n\n"
        "DESCRIPTION\n"
        "  XAdESSigner demonstrates the usage of XAdESSigner from SecureBlackbox.\n"
        "  Used to create an XML Extended Signature (XAdES) from an XML file.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to sign (Required).\n\n"
        "  -output       Where the XAdES will be saved (Required).\n\n"
        "  -cert         The certificate used to sign files (Required).\n\n"
        "  -certpass     The password for the signing certificate.\n\n"
        "  -version      The XAdES version to use. Enter the corresponding number. Valid values:\n"
        "                  1 - XAV_111\n"
        "                  2 - XAV_122\n"
        "                  3 - XAV_132\n"
        "                  4 - XAV_141\n\n"
        "  -level         The XAdES level/form to use. Enter the corresponding number. Valid values:\n"
        "                  0  - ASL_UNKNOWN\n"
        "                  1  - ASL_GENERIC\n"
        "                  2  - ASL_BASELINE_B\n"
        "                  3  - ASL_BASELINE_T\n"
        "                  4  - ASL_BASELINE_LT\n"
        "                  5  - ASL_BASELINE_LTA\n"
        "                  6  - ASL_BES\n"
        "                  7  - ASL_EPES\n"
        "                  8  - ASL_T\n"
        "                  9  - ASL_C\n"
        "                  10 - ASL_X\n"
        "                  11 - ASL_XTYPE_1\n"
        "                  12 - ASL_XTYPE_2\n"
        "                  13 - ASL_XL\n"
        "                  14 - ASL_XLTYPE_1\n"
        "                  15 - ASL_XLTYPE_2\n"
        "                  16 - ASL_A\n"
        "                  17 - ASL_EXTENDED_BES\n"
        "                  18 - ASL_EXTENDED_EPES\n"
        "                  19 - ASL_EXTENDED_T\n"
        "                  20 - ASL_EXTENDED_C\n"
        "                  21 - ASL_EXTENDED_X\n"
        "                  22 - ASL_EXTENDED_XTYPE_1\n"
        "                  23 - ASL_EXTENDED_XTYPE_2\n"
        "                  24 - ASL_EXTENDED_XLONG\n"
        "                  25 - ASL_EXTENDED_XL\n"
        "                  26 - ASL_EXTENDED_XLTYPE_1\n"
        "                  27 - ASL_EXTENDED_XLTYPE_2\n"
        "                  28 - ASL_EXTENDED_A\n\n"
        "  -detached     Whether the signature is detached.\n\n"
        "  -canonmethod  The canonicalization method to use. Enter the corresponding number. Valid values:\n"
        "                  0 - CXCM_NONE\n"
        "                  1 - CXCM_CANON\n"
        "                  2 - CXCM_CANON_COMMENT\n"
        "                  3 - CXCM_EXCL_CANON\n"
        "                  4 - CXCM_EXCL_CANON_COMMENT\n"
        "                  5 - CXCM_MIN_CANON\n"
        "                  6 - CXCM_CANON_V_1_1\n"
        "                  7 - CXCM_CANON_COMMENT_V_1_1\n\n"
		"  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224\n\n"
        "  -tsserver     A timestamp server to use during signing.\n\n"
        "  -includekey   Whether to include the public key in the signature.\n\n"
        "EXAMPLES\n"
        "  xadessigner -input C:\\xades\\myfile.xml -output C:\\xades\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
        "  xadessigner -input C:\\xades\\myfile.xml -output C:\\xades\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
        "              -version 3 -level 3 -canonmethod 1 -hashalg SHA256 -detached -tsserver http://timestamp.wosign.com\n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
if (len(sys.argv) <= 1):
    displayHelp("")
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
            if (sys.argv[x].lower() == "-level"):
                signer.set_new_sig_level(int(sys.argv[x+1]))
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
        displayHelp("-input is required.")
        sys.exit(1)
    
    if (outputF == ""):
        displayHelp("-output is required.")
        sys.exit(1)
    
    if (certF == ""):
        displayHelp("-cert is required.")
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





