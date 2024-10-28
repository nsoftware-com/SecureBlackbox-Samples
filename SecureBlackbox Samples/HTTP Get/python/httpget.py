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


def fireCertificateValidate(e):
    e.accept = 1
    
def fireDocumentStart(e):
    print("--- Document started ---")
    
def fireDocumentFinish(e):
    print("--- Document finished ---\n")
    
def firePreparedHeaders(e):
    print("Headers sent: ")
    for x in range(http.get_req_header_count()):
        print("   " + http.get_req_header_name(x) + ": " + http.get_req_header_value(x))
    print("")
    
def fireReceivingHeaders(e):
    print("Headers received: ")
    for x in range(http.get_resp_header_count()):
        print("   " + http.get_resp_header_name(x) + ": " + http.get_resp_header_value(x))
    print("")
    
def fireRedirection(e):
    print("Request redirected to " + e.new_url + "\n")
    e.allow_redirection = 1
    
if (len(sys.argv) < 2):
    print(		
        "NAME\n"
		"  httpget -- SecureBlackbox HTTPClient Demo Application\n\n"
		"SYNOPSIS\n"
		"  httpget <url>\n\n"
		"DESCRIPTION\n"
		"  This sample illustrates the ways of making GET requests with HTTPSClient.\n\n"
		"  The options are as follows:\n\n"
		"  url       The local host of server (Required).\n\n"
		"EXAMPLES\n"
		"  httpget https://www.google.com\n\n"
    );  
else:
    http = HTTPClient()
    http.on_tls_cert_validate = fireCertificateValidate
    http.on_document_begin = fireDocumentStart
    http.on_document_end = fireDocumentFinish
    http.on_headers_prepared = firePreparedHeaders
    http.on_headers_received = fireReceivingHeaders
    http.on_redirection = fireRedirection
    
    try:
        http.get(sys.argv[1])
        print("Server Response:\n\n%s\n\n"%http.get_output_string().encode('utf8'))
    except Exception as e: 
        print(e)



