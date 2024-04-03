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

from os import path

def displayHelp():
    print(
        "NAME\n"
        "  archivewriter -- SecureBlackbox ArchiveWriter Demo Application\n\n"
        "SYNOPSIS\n"
        "  archivewriter [-arctype archive_type] -input input1, input2, .... [-output output_file] [-pass encryption_password] [-enctype encryption_type]\n\n"
        "DESCRIPTION\n"
        "  ArchiveWriter demonstrates the usage of ArchiveWriter from SecureBlackbox.\n"
        "  Used to create archive.\n\n"
        "  The options are as follows:\n\n"
        "  -arctype      The type of archive (Required). Valid values:\n\n"
        "                  1 - ZIP\n"
        "                  2 - GZIP\n"
        "                  3 - BZIP_2\n"
        "                  4 - TAR\n"
        "                  5 - TAR_GZIP\n"
        "                  6 - TAR_BZIP_2\n\n"
        "  -input        List of files or directories compressing to archive (Required).\n\n"
        "  -output       Where the archive file will be saved (Required).\n\n"
        "  -pass         The password for the encryption.\n\n"
        "  -enctype      The encryption type. Valid values: \n\n"
        "                  0 - DEFAULT\n"
        "                  1 - NO_ENCRYPTION\n"
        "                  2 - GENERIC\n"
        "                  3 - WIN_ZIP\n"
        "                  4 - STRONG\n\n"
        "EXAMPLES\n"
        "  archivewriter -arctype 1 -input C:\\archive\\helloworld.txt -output C:\\archive\\myarchive.zip \n\n"
        "  archivewriter -arctype 5 -input C:\\archive\\helloworld.txt C:\\archive\\temp -output C:\\archive\\myarchive.tar -enctype 2 -pass mypassword \n\n"
    )
    
if (len(sys.argv) < 7):
    displayHelp()
    sys.exit(1)
else:
    archive = ArchiveWriter()
    
    arctype = 0
    inputIdx = -1
    output = ""
    
    try:
        for x in range(len(sys.argv)):
            if (sys.argv[x].startswith("-")):
                if (sys.argv[x].lower() == "-arctype"):
                    arctype = int(sys.argv[x+1])
                if (sys.argv[x].lower() == "-input"):
                    inputIdx = x+1         
                if (sys.argv[x].lower() == "-output"):
                    output = sys.argv[x+1]
                if (sys.argv[x].lower() == "-pass"):
                    archive.set_encryption_password(sys.argv[x+1])
                if (sys.argv[x].lower() == "-enctype"):
                    archive.set_encryption_type(int(sys.argv[x+1]))
                
        if (arctype == 0):
            print("-arctype is required.\n")
            displayHelp()
            sys.exit(1)
    
        if (inputIdx == -1):
            print("-input is required.\n")
            displayHelp()
            sys.exit(1)
    
        if (output == ""):
            print("-output is required.\n")
            displayHelp()
            sys.exit(1)

        archive.create_new(arctype)
            
        for y in range(inputIdx, len(sys.argv)):
            if (sys.argv[y].startswith("-")):
                break
            else:
                if (path.isfile(sys.argv[y])):
                    archive.add_file(path.basename(sys.argv[y]), sys.argv[y])
                elif (path.isdir(sys.argv[y])):
                    archive.add_files("", sys.argv[y], 1)
    
        print("List of files added to the archive")
        for y in range(archive.get_file_count()):
            print("    %s"%(archive.get_file_path(y)))
        print("")
                   
        archive.save(output)
    
        print("Archive created.")
    except Exception as e: 
        print(e)



