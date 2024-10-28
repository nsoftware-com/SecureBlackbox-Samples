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
        "  archivereader -- SecureBlackbox ArchiveReader Demo Application\n\n"
        "SYNOPSIS\n"
        "  archivereader <-arctype archive_type> <-input input_file> [-output output_path] [-pass decryption_password]\n\n"
        "DESCRIPTION\n"
        "  ArchiveReader demonstrates the usage of ArchiveReader from SecureBlackbox.\n"
        "  Used to extract files from archive.\n\n"
        "  The options are as follows:\n\n"
        "  -arctype      The type of archive (Required). Valid values:\n\n"
        "                  1 - AFT_ZIP\n"
        "                  2 - AFT_GZIP\n"
        "                  3 - AFT_BZIP_2\n"
        "                  4 - AFT_TAR\n"
        "                  5 - AFT_TAR_GZIP\n"
        "                  6 - AFT_TAR_BZIP_2\n\n"
        "  -input        An input archive file (Required).\n\n"
        "  -output       Where the extracted files will be saved.\n\n"
        "  -pass         The password for the encrypted archive.\n\n"
        "EXAMPLES\n"
        "  archivereader -arctype 1 -input C:\\archive\\helloworld.zip -output C:\\archive \n\n"
        "  archivereader -arctype 5 -input C:\\archive\\helloworld.tar -pass mypassword \n\n"
     )
    if (errMes != ""):
        print("Error: %s\n\n"%errMes)
    
    
if (len(sys.argv) <= 1):
    displayHelp("")
    sys.exit(1)
else:
    archive = ArchiveReader()
    
    arctype = 0
    inputArc = ""
    output = ""

    try:
        for x in range(len(sys.argv)):
            if (sys.argv[x].startswith("-")):
                if (sys.argv[x].lower() == "-arctype"):
                    arctype = int(sys.argv[x+1])
                if (sys.argv[x].lower() == "-input"):
                    inputArc = sys.argv[x+1]
                if (sys.argv[x].lower() == "-output"):
                    output = sys.argv[x+1]
                if (sys.argv[x].lower() == "-pass"):
                    archive.set_decryption_password(sys.argv[x+1])
                
        if (arctype == 0):
            displayHelp("-arctype is required.")
            sys.exit(1)
    
        if (inputArc == ""):
            displayHelp("-input is required.")
            sys.exit(1)

        archive.open(arctype, inputArc)
    
        print("List of files in the archive")
        for y in range(archive.get_file_count()):
            print("    %s"%(archive.get_file_path(y)))
        print("")
        
        if (output != ""):   
            archive.set_overwrite(True)
            archive.extract_all(output)
    
        print("All files from archive extracted.")
    except Exception as e: 
        print(e)





