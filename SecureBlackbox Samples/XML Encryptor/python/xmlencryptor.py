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
        "  xmlencryptor -- SecureBlackbox XMLEncryptor Demo Application\n\n"
        "SYNOPSIS\n"
		"  xmlencryptor <-input input_file> <-output output_file> [-datatype encrypted_data_type] [-encmethod encryption_method]\n"
        "            [-xmlnode xml_node] [-enckey] [-enckeytype encryption_key_type] [-transport key_transport_method] \n"
        "            [-wrap key_wrap_method] [-cert certificate_file] [-certpass certificate_password] [-pass key_password] \n\n"
        "DESCRIPTION\n"
        "  XMLEncryptor demonstrates the usage of XMLEncryptor from SecureBlackbox.\n"
        "  Used to encrypt XML file.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input XML file to encrypt (Required). \n\n"
        "  -output       Where the encrypted XML file will be saved (Required). \n\n"
        "  -datatype     The encryption data type to use. Enter the corresponding number. Valid values: \n\n"
        "                  0 - Element \n"
        "                  1 - Content \n"
        "  -encmethod    The encryption method to use. Valid values: 3DES, AES128, AES192, AES256, Camellia128, Camellia192, Camellia256, DES, RC4, SEED \n\n"
        "  -xmlnode      The xml node value. \n\n"
        "  -enckey       Whether to use key encryption. \n\n"
        "  -enckeytype   The encryption key type to use. Enter the corresponding number. Valid values: \n\n"
        "                  0 - KeyTransport \n"
        "                  1 - KeyWrap \n"
        "  -transport    The key transport method to use. Enter the corresponding number. Valid values: \n\n"
        "                  0 - RSA15 \n"
        "                  1 - RSAOAEP \n"
        "  -wrap         The key wrap method to use. Valid values: 3DES, AES128, AES192, AES256, Camellia128, Camellia192, Camellia256, SEED. \n\n"
        "  -cert         The certificate used to encrypt file. \n\n"
        "  -certpass     The password for the certificate. \n\n"
        "  -pass         The password for the encrypting. \n\n"
        "EXAMPLES\n"
        "  xmlencryptor -input C:\\xml\\myfile.xml -output C:\\xml\\myencfile.xml -pass mypassword \n\n"
        "  xmlencryptor -input C:\\xml\\myfile.xml -output C:\\xml\\myencfile.xml -enckey -pass mypassword -datatype 1 -wrap AES192 \n\n"
        "  xmlencryptor -input C:\\xml\\myfile.xml -output C:\\xml\\myencfile.xml -enckey -enckeytype 0 -cert C:\\certs\\mycert.pfx -certpass mypassword \n"
        "              -datatype 2 -mimetype \"Test mime type\" -external C:\\xml\\external.xml -transport 1 \n\n"
    )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
def getKey(algorithm, password):
    reslen = 0

    if (algorithm.lower() == "AES128".lower()):
        reslen = 16
    elif (algorithm.lower() == "AES192".lower()):
        reslen = 24
    elif (algorithm.lower() == "AES256".lower()):
        reslen = 32
    elif (algorithm.lower() == "Camellia128".lower()):
        reslen = 16
    elif (algorithm.lower() == "Camellia192".lower()):
        reslen = 24
    elif (algorithm.lower() == "Camellia256".lower()):
        reslen = 32
    elif (algorithm.lower() == "DES".lower()):
        reslen = 8
    elif (algorithm.lower() == "3DES".lower()):
        reslen = 24
    elif (algorithm.lower() == "RC4".lower()):
        reslen = 16
    elif (algorithm.lower() == "SEED".lower()):
        reslen = 16

    res = password
    while (len(res) < reslen):
        res = res + "/" + password

    resb = res.encode('utf-8')
    return resb[:reslen]

    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    encryptor = XMLEncryptor()
    encryptor.set_use_gcm(False)
    cm = CertificateManager()
    
    inputF = ""
    outputF = ""
    encryptKey = False
    encryptedDataType = 0
    enckeytype = 1
    transportmethod = 0
    wrapmethod = "3DES"
    mimetype = ""
    certFile = ""
    certPass = ""
    keyPass = ""
    externalFile = ""   
    
    for x in range(len(sys.argv)):
        if (sys.argv[x].startswith("-")):
            if (sys.argv[x].lower() == "-input"):
                inputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-output"):
                outputF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-datatype"):
                encryptedDataType = int(sys.argv[x+1])
            if (sys.argv[x].lower() == "-encmethod"):
                encryptor.set_encryption_method(int(sys.argv[x+1]))
            if (sys.argv[x].lower() == "-xmlnode"):
                encryptor.set_xml_node(sys.argv[x+1])
            if (sys.argv[x].lower() == "-mimetype"):
                mimetype = sys.argv[x+1]
            if (sys.argv[x].lower() == "-enckey"):
                encryptKey = True
            if (sys.argv[x].lower() == "-enckeytype"):
                enckeytype = int(sys.argv[x+1])
            if (sys.argv[x].lower() == "-transport"):
                transportmethod = int(sys.argv[x+1])
            if (sys.argv[x].lower() == "-wrap"):
                wrapmethod = sys.argv[x+1]                
            if (sys.argv[x].lower() == "-cert"):
                certF = sys.argv[x+1]
            if (sys.argv[x].lower() == "-certpass"):
                certpass = sys.argv[x+1]
            if (sys.argv[x].lower() == "-pass"):
                keyPass = sys.argv[x+1]
            if (sys.argv[x].lower() == "-external"):
                externalFile = sys.argv[x+1]
    
    if (inputF == ""):
        displayHelp("-input is required.")
        sys.exit(1)
    
    if (outputF == ""):
        displayHelp("-output is required.")
        sys.exit(1)
        
    if (encryptKey and enckeytype == 0):
        if (certF == ""):
            displayHelp("-cert is required.")
            sys.exit(1)
    else: 
        if (keyPass == ""):
            displayHelp("-pass is required.")
            sys.exit(1)
    
    try:
        encryptor.set_input_file(inputF)
        encryptor.set_output_file(outputF)
        
        encryptor.set_encrypt_key(encryptKey)
        encryptor.set_encrypted_data_type(encryptedDataType)
        if (encryptedDataType == 2):
            if (len(mimetype) > 0):
                encryptor.config("MimeType=" + mimetype)

            in_file = open(externalFile, "rb") 
            encryptor.set_external_data(in_file.read())
            in_file.close()


        if (encryptor.get_encrypt_key()):
            encryptor.set_key_encryption_type(enckeytype)
            if (enckeytype == 0):
                encryptor.set_key_transport_method(transportmethod)
                
                cm.import_from_file(certF, certpass)
                encryptor.set_key_encryption_cert_handle(cm.get_cert_handle())
            else:
                encryptor.set_key_wrap_method(wrapmethod)
                encryptor.set_key_encryption_key(getKey(wrapmethod, keyPass))
        else:
            encryptor.set_encryption_key(getKey(encryptor.get_encryption_method(), keyPass))

        encryptor.encrypt()

        print("XML file successfully encrypted.")
    except Exception as e: 
        print(e)





