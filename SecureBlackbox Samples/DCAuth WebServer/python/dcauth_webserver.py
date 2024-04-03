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

#!C:\Users\Ludovik14\anaconda3\python.exe
import cgi, os
import cgitb
import string
import random
import tempfile
from pathlib import Path
import urllib
from html import escape
from secureblackbox import *

# The web server endpoints: the pre-signing one, the completion one, and the one that flushes the signed files.
preSigningEndpoint = "start";
completionEndpoint = "finish";
getFileEndpoint = "getfile";

# The web server endpoints: the pre-signing one, the completion one, and the one that flushes the signed files.
signingServiceURL = "http://127.0.0.1:17080/sign";

# A local (to the web app) directory to keep the pre-signed files.
preSignedDocFolder = tempfile.gettempdir() + "\presigned";
Path(preSignedDocFolder).mkdir(parents=True, exist_ok=True)

# A local (to the web app) directory to keep the signed files.
signedDocFolder = tempfile.gettempdir() + "\signed";
Path(signedDocFolder).mkdir(parents=True, exist_ok=True)

# Whether to use PKCS7 (true) or PKCS1 (false) DCAuth method.
usePKCS7Method = True

# Whether to use Javascript.
useJSMode = False

# The DCAuth authentication credentials: KeyID and KeySecret.
# These two should match the ones used by the signing service.
keyID = "mykeyid"
keySecret = "mykeysecret"

# The URI of the timestamping service.
tsaURL = ""

PKCS1Format = "?format=pkcs1"
PKCS7Format = "?format=pkcs7"

# Implements the pre-signing logic. Takes an unsigned PDF on input,
# saves the pre-signed copy in the local directory, returns the async request
# and a randomly generated DocID for the document.
def preSignDocument(docData, docID):
	# This method pre-signs an unsigned document and produces the async request.

	# Specifically, the SignAsyncBegin call performs the following steps:
	#    - calculates the document hash and prepares any necessary signature elements,
	#    - packs the hash into a DCAuth async request and signs it with KeyID and KeySecret,
	#    - generates a pre-signed version of the document, which is basically a
	#      document with a placeholder for the future signature.

	# Running the pre-signing routine.
	signer = PDFSigner()
	cm = CertificateManager()
	try:
		# Assigning the unsigned document
		signer.set_input_bytes(docData)

		# Cancelling any chain validation since we are using a test certificate
		signer.set_ignore_chain_validation_errors(True)
		signer.config("AutoCollectRevocationInfo=false")

		# For the same reason setting the signature level to BES
		# (it is unlikely that the test certificate has any proper chain)
		signer.set_sig_level(1)

		# KeyID and KeySecret are arbitrary strings (the longer, the better),
		# but they must match those used by the signing service on the other side.
		signer.set_external_crypto_key_id(keyID)
		signer.set_external_crypto_key_secret(keySecret)

		# Loading the certificate. The certificate is mandatory if PKCS#1 method
		# is used. It may only be needed with PKCS#7 method if you want to fill
		# the signature widget texts basing on the information contained in it.
		certfile = str(Path(__file__).parent) + "\cert.cer"
		if (Path(certfile).exists()):
			cm.import_from_file(certfile, "")
			signer.set_signing_cert_handle(cm.get_cert_handle())
		else:
			global usePKCS7Method
			usePKCS7Method = true

		# Specifying the signing method
		if (usePKCS7Method):
			signer.set_external_crypto_method(1)
		else:
			signer.set_external_crypto_method(0)

		if (useJSMode):
			signer.set_external_crypto_mode(4)
		else:
			signer.set_external_crypto_mode(3)

		# If the certificate was not provided (meaning PKCS#7 mode is used),
		# we need to tune-up the signature widget manually. If the certificate
		# is available, the signer information for the widget will be automatically
		# generated from the values contained in it.
		if (signer.get_signing_cert_handle() == 0):
			signer.set_sig_auto_text(False)
			signer.set_sig_algorithm_caption("RSA")
			signer.set_sig_algorithm_info("Strong Asymmetric Algorithm")
			signer.set_sig_handle("Good signature")
			signer.set_sig_signer_caption("Signed by: ")
			signer.set_sig_signer_info("Trusted Signer")

		# Including DocID in the request as 'user data': DCAuth will mirror it
		# in its async response, and we will use it on the completion stage to identify
		# the document the response corresponds to.
		signer.set_external_crypto_data(docID)

		# Setting the TSA service URL. Even though the TSA is only used on the
		# completion stage, we must assign it on the pre-signing stage too,
		# as PDFSigner uses this knowledge when calculating the size of the
		# signature window.
		signer.set_timestamp_server(tsaURL)

		# Pre-signing the document.
		res = signer.sign_async_begin();

		# Saving the pre-signed document to a local directory.
		Path(preSignedDocFolder + "\\" + docID).write_bytes(signer.get_output_bytes())

		return res;
	except Exception as e: 
		errorPage(str(e))
		return "";

# Implements the completion logic. Takes an async response on input,
# collects the pre-signed copy from a local directory, embeds the signature,
# and saves the resulting document in another local directory.
def completeSigning(asyncResponse):
	# This method completes the signing by inserting the signature into the earlier pre-signed document.

	# Specifically, the SignAsyncEnd call performs the following steps:
	#  - extracts the signature from the async response,
	#  - validates its integrity,
	#  - embeds the signature, along with any other necessary elements, to the signature,
	#  - if the TSA URL was provided, timestamps the created signature.

	signer = PDFSigner()
	try:
		if (useJSMode):
			signer.set_external_crypto_mode(4)
		else:
			signer.set_external_crypto_mode(3)

		# Extracting the DocID from the 'user data' mirrored by the DCAuth
		# service in its async response.
		docID = signer.extract_async_data(asyncResponse)

		# Checking if a pre-signed file with such DocID exists.
		if (not Path(preSignedDocFolder + "\\" + docID).exists()):
			errorPage("Pre-signed file not found")
			return ""

		# Loading the pre-signed document.
		preSignedDoc = Path(preSignedDocFolder + "\\" + docID).read_bytes()

		# Assigning the pre-signed document
		signer.set_input_bytes(preSignedDoc)

		# Cancelling any chain validation.
		signer.set_ignore_chain_validation_errors(True)

		# Assigning credentials.
		signer.set_external_crypto_key_id(keyID)
		signer.set_external_crypto_key_secret(keySecret)

		# Assigning the timestamping service URL. This time the signer object
		# makes real use of it.
		signer.set_timestamp_server(tsaURL)

		# Completing the signing.
		signer.sign_async_end(asyncResponse);

		# Saving the now fully signed document to a local directory.
		Path(signedDocFolder + "\\" + docID + ".pdf").write_bytes(signer.get_output_bytes())

		return docID
	except Exception as e:
		errorPage(str(e))
		return ""

# Outputs welcome/information page. It is only used for debug/testing purposes,
# a live signing service does not need it.
def welcomePage():
	print("Content-type: text/html")
	print("")
	print("<html>")
	print("<head><title>DCAuth demo: welcome</title></head>")
	print("<body style=\"font-family: sans-serif\">")
	print("  <h1>DCAuth demo: the web application</h1>")
	print("  <h2>Welcome to DCAuth demo</h2>")
	print("  When you click the Start button, a chosen PDF document will be uploaded")
	print("  (POST'ed) to the pre-signing page. During the generation of the pre-signing")
	print("  page the following will happen: ")
	print("  <ul>")
	print("    <li>A document hash will be calculated and incorporated into an &quot;async request&quot;,")
	print("    <li>The async request will be incorporated into the body of the produced page, ")
	print("    <li>A pre-signed copy of the document will be saved to the special directory on the web server.")
	print("  </ul>")
	print("  When your browser receives the output of the pre-signing page,")
	print("  it will run the Javascript embedded into the page to pass the async request")
	print("  to the local signing service and ask it to sign the hash.")
	print("  This web application is configured to assume that the signing");
	print("  service is accessible from the browser at the following location: " + signingServiceURL + ". ")
	print("  Please make sure the signing service is active before proceeding to the ")
	print("  pre-signing stage.")
	print("  <p>")
	print("  If you are using the second part of the DCAuth demo as your signing service,")
	print("  you can check its availability by following <a href=\"" + signingServiceURL + "\" target=\"_blank\">this link</a>")
	print("  (the service should respond with a welcome/status page)")
	print("  <form method=\"post\" enctype=\"multipart/form-data\">")
	print("    Please choose a PDF file to sign: <input type=\"file\" name=\"pdffile\" /><br />")
	print("    <input type=\"submit\" name=\"" + preSigningEndpoint + "\" value=\"Start signing\" />")
	print("  </form>")
	print("</body>")
	print("</html>")

# Pre-signs the document and outputs a web page that contains the async request
# and the Javascript that can relay it to the signing service. This is produced
# in response to a POST from the welcome page containing the unsigned document.
def preSigningPage():
	# Forming the page by inserting a variety of variable pieces to the template.
	# Note: since AsyncReq is an XML document, we need to URLEncode it to ensure
	# the page's syntactical safety. The signing service URLDecodes it.

	letters = string.ascii_uppercase + string.digits
	while True:
		docID = ''.join(random.choice(letters) for i in range(10))
		if (not Path(preSignedDocFolder + "\\" + docID).exists()):
			break

	fileitem = form['pdffile']
	buf = fileitem.file.read();

	asyncReq = preSignDocument(buf, docID);
	if (asyncReq == ""):
		return

	print("Content-type: text/html")
	print("")
	print("<html>")
	print("<head>")
	print("  <title>DCAuth demo: pre-signing completed</title>")
	print("  <script>")
	print("    // The async request itself.")
	print("    var asyncReq = \"" + urllib.parse.quote_plus(asyncReq) + "\"; ")
	print("")
	print("    // The URL where to submit the async response to.")
	print("    var completionUri = \"" + completionEndpoint + "\"; ")
	print("")
	print("    // This function relays the async request to the DCAuth signing service.")
	print("    function submitAsyncRequest() {")
	print("      var xhr = new XMLHttpRequest();")
	print("      xhr.open(\"POST\", \"" + signingServiceURL + "\", true);")
	print("      xhr.setRequestHeader(\"Content-Type\", \"application/x-www-form-urlencoded\");")
	print("      xhr.onload = function() {")
	print("        if (this.status != 200) {")
	print("          alert(\"Something went wrong: \" + this.response);")
	print("        } else {")
	print("          // Saving a copy of the response in a page element for debug purposes.")
	print("          var respElem = document.getElementById(\"asyncresptext\"); ")
	print("          respElem.innerHTML = \"<p>The below async response was received: " +
	      "          <p><textarea cols=\\\"50\\\" rows=\\\"8\\\">\" + this.response.replace(/&/g, \"&amp;\").replace(/</g, \"&lt;\").replace(/>/g, \"&gt;\").replace(/'/g, \"&#39;\").replace(/\"/g, \"&#34;\") + \"</textarea>\";")
	print("")
	print("          // Reporting the success of the operation and asking the user whether we can proceed.")
	print("          var res = confirm(\"A successful async response has been received from the signing endpoint (and a copy has been added to the page for your records). Press OK to submit this response back to the web application at \" + completionUri + \".\");")
	print("          if (res == true) {")
	print("            // Submitting the form to the web application's completion endpoint.")
	print("            document.getElementById(\"asyncresp\").value = this.response;")
	print("            document.getElementById(\"respform\").submit();")
	print("          }")
	print("        }")
	print("      }")
	print("      xhr.send(asyncReq);")
	print("    }")
	print("  </script>")
	print("</head>")
	print("<body style=\"font-family: sans-serif\">")
	print("  <h1>DCAuth demo: the web application</h1>")
	print("  <h2>Pre-signing completed. </h2>")
	print("  The PDFSigner component has successfully pre-signed the provided PDF document.")
	print("  The pre-signed document has been assigned with a DocID of " + docID)
	print("  and saved to the following location: " + preSignedDocFolder + "\\" + docID + ".")
	print("  <p>")
	print("  The async request shown below has been generated as a result of the pre-signing. ")
	print("  This has also been included in the body of this page, and will be ")
	print("  submitted to your local signing endpoint by the embedded Javascript when you click the Sign")
	print("  button below. The signing procedure is performed behind the scenes and ")
	print("  is fully transparent for you.")
	print("  <p>")
	print("  <textarea cols=\"50\" rows=\"8\">" + escape(asyncReq) + "</textarea>.")
	print("  <p>")
	print("  This page expects the signing endpoint to be accessible at " + signingServiceURL + ".")
	print("  If the endpoint cannot be reached at this address, the signing will fail.")
	print("  <p>")
	print("  If the signing completes successfully and a good async response containing")
	print("  the signature has been received from the signing service, this page will submit ")
	print("  it back to the web app for completion of the signing operation. The page ")
	print("  will notify you when this happens. ")
	print("  <p>Now click the Sign button to pass the async request to the signing service: ")
	print("  <button onclick=\"submitAsyncRequest()\">Sign</button>")
	print("  <p><div id=\"asyncresptext\"></div>")
	print("  <form method=\"post\" id=\"respform\">")
	print("    <input type=\"hidden\" value=\"\" id=\"asyncresp\" name=\"asyncresp\" />")
	print("    <input type=\"hidden\" name=\"" + completionEndpoint + "\" value=\"1\" />")
	print("  </form>")
	print("</body>")
	print("</html>")

# Pre-signs the document and outputs a web page that contains the async request in JSON format.
def preSigningPageJSMode():
	letters = string.ascii_uppercase + string.digits
	while True:
		docID = ''.join(random.choice(letters) for i in range(10))
		if (not Path(preSignedDocFolder + "\\" + docID).exists()):
			break

	fileitem = form['pdffile']
	buf = fileitem.file.read()

	asyncReq = preSignDocument(buf, docID);
	if (asyncReq == ""):
		return

	signingService = "";
	if (usePKCS7Method):
		signingService = signingServiceURL + PKCS7Format
	else:
		signingService = signingServiceURL + PKCS1Format

	print("Content-type: text/html")
	print("")
	print("<html>")
	print("<head>")
	print("  <title>DCAuth demo: pre-signing completed</title>")
	print("  <script type=\"text/javascript\" src=\"dcauth.js\" ></script>")
	print("  <script>")
	print("    // The async request itself.")
	print("    var asyncReq = " + asyncReq + "; ")
	print("")
	print("    // The URL where to submit the async response to.")
	print("    var completionUri = \"" + completionEndpoint + "\"; ")
	print("")
	print("    try")
	print("    {")
	print("      var request = new DcauthAsyncRequest(asyncReq);")
	print("    }")
	print("    catch(err) {")
	print("      alert(\"Something went wrong: \" + err.message);")
	print("    }")
	print("    var hash = request.getHash();")
	print("")
	print("    // This function relays the hash to the DCAuth signing service.")
	print("    function submitAsyncRequest() {")
	print("      var xhr = new XMLHttpRequest();")
	print("      xhr.open(\"POST\", \"" + signingService + "\", true);")
	print("      xhr.setRequestHeader(\"Content-Type\", \"application/x-www-form-urlencoded\");")
	print("      xhr.onload = function() {")
	print("        if (this.status != 200) {")
	print("          alert(\"Something went wrong: \" + this.response);")
	print("        } else {")
	print("          // Forming the response state basing on the javascript signature we received and saving it in a page element for debug purposes.")
	print("          var response = new DcauthAsyncResponse(request);")
	print("          response.setSignature(this.response);")
	print("          var respElem = document.getElementById(\"asyncresptext\"); ")
	print("          respElem.innerHTML = \"<p>The below async state was composed from the signature received from the service: " +
	      "          <p><textarea cols=\\\"50\\\" rows=\\\"8\\\">\" + response.savetostring(); + \"</textarea>\";")
	print("")
	print("          // Reporting the success of the operation and asking the user whether we can proceed.")
	print("          var res = confirm(\"A successful response has been received from the signing endpoint (and a copy has been added to the page for your records). Press OK to submit this signature back to the web application at \" + completionUri + \".\");")
	print("          if (res == true) {")
	print("            // Submitting the form to the web application''s completion endpoint.")
	print("            // TODO: check/update as needed")
	print("            document.getElementById(\"asyncresp\").value = response.savetostring();")
	print("            document.getElementById(\"respform\").submit();")
	print("          }")
	print("        }")
	print("      }")
	print("      xhr.send(hash);")
	print("    }")
	print("  </script>")
	print("</head>")
	print("<body style=\"font-family: sans-serif\" onload=''document.getElementById(\"reqhash\").innerHTML=hash;''>")
	print("  <h1>DCAuth demo: the web application</h1>")
	print("  <h2>Pre-signing completed. </h2>")
	print("  The PDFSigner component has successfully pre-signed the provided PDF document.")
	print("  The pre-signed document has been assigned with a DocID of " + docID)
	print("  and saved to the following location: " + preSignedDocFolder + "\\" + docID + ".")
	print("  <p>")
	print("  The async request shown below has been generated as a result of the pre-signing. ")
	print("  This has also been included in the body of this page as a JSON object, and")
	print("  pre-processed to extract the hash. The hash is: <div id=\"reqhash\"></div>. ")
	print("  This hash value will be submitted to your local signing endpoint by ")
	print("  the embedded Javascript when you click the Sign")
	print("  button below. The signing procedure is performed behind the scenes and ")
	print("  is fully transparent for you.")
	print("  <p>")
	print("  <textarea cols=\"50\" rows=\"8\">" + escape(asyncReq) + "</textarea>.")
	print("  <p>")
	print("  This page expects the signing endpoint to be accessible at " + signingService + ".")
	print("  If the endpoint cannot be reached at this address, the signing will fail.")
	print("  <p>")
	print("  If the signing completes successfully and a good async response containing")
	print("  the signature has been received from the signing service, this page will submit ")
	print("  it back to the web app for completion of the signing operation. The page ")
	print("  will notify you when this happens. ")
	print("  <p>Now click the Sign button to pass the hash to the signing service: ")
	print("  <button onclick=\"submitAsyncRequest()\">Sign</button>")
	print("  <p><div id=\"asyncresptext\"></div>")
	print("  <form method=\"post\" id=\"respform\">")
	print("    <input type=\"hidden\" value=\"\" id=\"asyncresp\" name=\"asyncresp\" />")
	print("    <input type=\"hidden\" name=\"" + completionEndpoint + "\" value=\"1\" />")
	print("  </form>")
	print("</body>")
	print("</html>")

# Completes the signature and saves a fully signed document. This is produced
# in response to a Javascript POST containing the async response from the pre-signing page.
def completionPage():
	docID = completeSigning(form.getvalue('asyncresp'))
	if (docID == ""):
		return

	print("Content-type: text/html")
	print("")
	print("<html>")
	print("<head><title>DCAuth demo: signing completed</title></head>")
	print("<body style=\"font-family: sans-serif\">")
	print("  <h1>DCAuth demo: the web application</h1>")
	print("  <h2>Signing completed successfully</h2>")
	print("  The remote signing procedure for your document has completed successfully. ")
	print("  Click <a href=\"?" + getFileEndpoint + "=1&id=" + docID + "\" target=\"_blank\">here</a> to download the signed document. Click <a href=\"/\">here</a> to start again.")
	print("</body>")
	print("</html>")

# Just flushes the signed PDF file.
def flushFile():
	docID = form.getvalue('id')

	if (not Path(signedDocFolder + "\\" + docID + ".pdf").exists()):
		errorPage("Signed file with this ID has not been found")
	else:
		print("Content-type: application/pdf")
		print("Content-Disposition: inline; filename=\"" + docID + ".pdf\"")
		print("Content-Transfer-Encoding: binary")
		print("Accept-Ranges: bytes")
		print("")
		sys.stdout.flush()
		sys.stdout.buffer.write(Path(signedDocFolder + "\\" + docID + ".pdf").read_bytes())

# Outputs the error page
def errorPage(errorMsg):
	print("Content-type: text/html")
	print("")
	print("<html>")
	print("<head><title>DCAuth demo: error</title></head>")
	print("<body style=\"font-family: sans-serif\">")
	print("  <h1>DCAuth demo: the web application</h1>")
	print("  <h2>Error</h2>")
	print("  The following error happened when trying to complete the operation: ")
	print("  <p><p>" + errorMsg + ".")
	print("  <p>Click <a href=\"/\">here</a> to return to the main page.")
	print("</body>")
	print("</html>")

form = cgi.FieldStorage()

if (getFileEndpoint in form):
	flushFile()
elif (preSigningEndpoint in form):
	if (useJSMode): # the pre-signing page for javascript signing
		preSigningPageJSMode()
	else: # the pre-signing page for the "default" DCAuth signing
		preSigningPage()
elif (completionEndpoint in form):
	completionPage()
else:
	welcomePage()

