<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - DCAuth WebServer</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - DCAuth WebServer">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>DCAuth WebServer</h2>
<p>A simple example of the DC technology. The sample incorporates two counterparts of DC: the application part is represented with PDFSigner control, and the private key part is represented with DCAuth control.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_httpserver.php');
require_once('../include/secureblackbox_pdfsigner.php');
require_once('../include/secureblackbox_certificatemanager.php');
require_once('../include/secureblackbox_const.php');

?>

<style>
table { width: 100% !important; }
td { white-space: nowrap; }
td input { width: 100%; }
td:last-child { width: 100%; }
</style>

<div width="90%">
  <?php
    // The web server endpoints: the pre-signing one, the completion one, and the one that flushes the signed files.
    $preSigningEndpoint = "start";
    $completionEndpoint = "finish";
    $getFileEndpoint = "getfile";

    // The web server endpoints: the pre-signing one, the completion one, and the one that flushes the signed files.
    $signingServiceURL = "http://127.0.0.1:17080/sign";

    // A local (to the web app) directory to keep the pre-signed files.
    $preSignedDocFolder = sys_get_temp_dir() . "\presigned";
    if (!file_exists($preSignedDocFolder))
    {
      mkdir($preSignedDocFolder);
    }

    // A local (to the web app) directory to keep the signed files.
    $signedDocFolder = sys_get_temp_dir() . "\signed";
    if (!file_exists($signedDocFolder))
    {
      mkdir($signedDocFolder);
    }

    // Whether to use PKCS7 (true) or PKCS1 (false) DCAuth method.
    $usePKCS7Method = true;

    // Whether to use Javascript.
    $useJSMode = false;

    if ($useJSMode)
    {
      if ($usePKCS7Method)
        $signingServiceURL = $signingServiceURL . "?format=pkcs7";
      else
        $signingServiceURL = $signingServiceURL . "?format=pkcs1";
    }

    // The DCAuth authentication credentials: KeyID and KeySecret.
    // These two should match the ones used by the signing service.
    $keyID = "mykeyid";
    $keySecret = "mykeysecret";

    // The URI of the timestamping service.
    $TSAURL = "";

    $CrLf = "\r\n";

    // Implements the pre-signing logic. Takes an unsigned PDF on input,
    // saves the pre-signed copy in the local directory, returns the async request
    // and a randomly generated DocID for the document.
    function preSignDocument($docData, $docID, $keyID, $keySecret, $usePKCS7Method, $useJSMode, $TSAURL, $preSignedDocFolder)
    {
      // This method pre-signs an unsigned document and produces the async request.

      // Specifically, the SignAsyncBegin call performs the following steps:
      //    - calculates the document hash and prepares any necessary signature elements,
      //    - packs the hash into a DCAuth async request and signs it with KeyID and KeySecret,
      //    - generates a pre-signed version of the document, which is basically a
      //      document with a placeholder for the future signature.

      // Running the pre-signing routine.
      $certmgr = new SecureBlackBox_CertificateManager();
      $signer = new SecureBlackbox_PDFSigner();
      try
      {
        // Assigning the unsigned document
        $signer->setInputBytes($docData);

        // Cancelling any chain validation since we are using a test certificate
        $signer->setIgnoreChainValidationErrors(true);
        $signer->doConfig("AutoCollectRevocationInfo=false");

        // For the same reason setting the signature level to BES
        // (it is unlikely that the test certificate has any proper chain)
        $signer->setSigLevel(1);

        // KeyID and KeySecret are arbitrary strings (the longer, the better),
        // but they must match those used by the signing service on the other side.
        $signer->setExternalCryptoKeyID($keyID);
        $signer->setExternalCryptoKeySecret($keySecret);

        // Loading the certificate. The certificate is mandatory if PKCS#1 method
        // is used. It may only be needed with PKCS#7 method if you want to fill
        // the signature widget texts basing on the information contained in it.
        $certfile = $_SERVER['DOCUMENT_ROOT'] . "\cert.cer";
        if (file_exists($certfile))
        {
          $certmgr->doImportFromFile($certfile, "");
          $signer->setSigningCertHandle($certmgr->getCertHandle());
        }
        else
        {
          // We can't use the PKCS#1 method if there is no public certificate on the
          // web application's side, so switching to PKCS#7 if the certificate is absent.
          $usePKCS7Method = true;
        }

        // Specifying the signing method
        if ($usePKCS7Method)
          $signer->setExternalCryptoMethod(1);
        else
          $signer->setExternalCryptoMethod(0);

        // Specifying the signing mode
        if ($useJSMode)
          $signer->setExternalCryptoMode(4);
        else
          $signer->setExternalCryptoMode(3);

        // If the certificate was not provided (meaning PKCS#7 mode is used),
        // we need to tune-up the signature widget manually. If the certificate
        // is available, the signer information for the widget will be automatically
        // generated from the values contained in it.
        if ($signer->getSigningCertHandle() == 0)
        {
          $signer->setSigAutoText(false);
          $signer->setSigAlgorithmCaption("RSA");
          $signer->setSigAlgorithmInfo("Strong Asymmetric Algorithm");
          $signer->setSigHeader("Good signature");
          $signer->setSigSignerCaption("Signed by: ");
          $signer->setSigSignerInfo("Trusted Signer");
        }

        // Including DocID in the request as 'user data': DCAuth will mirror it
        // in its async response, and we will use it on the completion stage to identify
        // the document the response corresponds to.
        $signer->setExternalCryptoData($docID);

        // Setting the TSA service URL. Even though the TSA is only used on the
        // completion stage, we must assign it on the pre-signing stage too,
        // as PDFSigner uses this knowledge when calculating the size of the
        // signature window.
        $signer->setTimestampServer($TSAURL);

        // Pre-signing the document.
        $res = $signer->doSignAsyncBegin();

        // Saving the pre-signed document to a local directory.
        file_put_contents($preSignedDocFolder . "\\" . $docID, $signer->getOutputBytes());

        return $res;
      }
      catch (exception $ex) 
      {
        throw $ex;
        return "";
      }
    }

    // Implements the completion logic. Takes an async response on input,
    // collects the pre-signed copy from a local directory, embeds the signature,
    // and saves the resulting document in another local directory.
    function completeSigning($asyncResponse, $preSignedDocFolder, $signedDocFolder, $keyID, $keySecret, $TSAURL, $useJSMode)
    {
      // This method completes the signing by inserting the signature into the earlier pre-signed document.

      // Specifically, the SignAsyncEnd call performs the following steps:
      //  - extracts the signature from the async response,
      //  - validates its integrity,
      //  - embeds the signature, along with any other necessary elements, to the signature,
      //  - if the TSA URL was provided, timestamps the created signature.

      $signer = new SecureBlackbox_PDFSigner();
      try
      {
        if ($useJSMode)
          $signer->setExternalCryptoMode(4);
        else
          $signer->setExternalCryptoMode(3);

        // Extracting the DocID from the 'user data' mirrored by the DCAuth
        // service in its async response.
        $docID = $signer->doExtractAsyncData($asyncResponse);

        // Checking if a pre-signed file with such DocID exists.
        if (!file_exists($preSignedDocFolder . "\\" . $docID))
          throw new Exception("Pre-signed file not found");

        // Loading the pre-signed document.
        $preSignedDoc = file_get_contents($preSignedDocFolder . "\\" . $docID);

        // Assigning the pre-signed document
        $signer->setInputBytes($preSignedDoc);

        // Cancelling any chain validation.
        $signer->setIgnoreChainValidationErrors(true);

        // Assigning credentials.
        $signer->setExternalCryptoKeyID($keyID);
        $signer->setExternalCryptoKeySecret($keySecret);

        // Assigning the timestamping service URL. This time the signer object
        // makes real use of it.
        $signer->setTimestampServer($TSAURL);

        // Completing the signing.
        $signer->doSignAsyncEnd($asyncResponse);

        // Saving the now fully signed document to a local directory.
        file_put_contents($signedDocFolder . "\\" . $docID . ".pdf", $signer->getOutputBytes());

        return $docID;
      }
      catch (exception $ex)
      {
        throw $ex;
        return "";
      }
    }

    // Outputs the error page
    function errorPage($errorMsg)
    {
      $CrLf = "\r\n";

      echo "<head><title>DCAuth demo: error</title></head>" . $CrLf;
      echo "<body style=\"font-family: sans-serif\">" . $CrLf;
      echo "  <h1>DCAuth demo: the web application</h1>" . $CrLf;
      echo "  <h2>Error</h2>" . $CrLf;
      echo "  The following error happened when trying to complete the operation: " . $CrLf;
      echo "  <p><p>" . $errorMsg . "." . $CrLf;
      echo "  <p>Click <a href=\"/\">here</a> to return to the main page." . $CrLf;
      echo "</body>";
    }

    if(isset($_GET[$getFileEndpoint])) 
    {
      // Just flushes the signed PDF file.
      
      $id = $_GET['id'];

      if (!file_exists($signedDocFolder . "\\" . $id . ".pdf"))
        throw new Exception("Signed file with this ID has not been found");

      // Clear page
      ob_end_clean();
      // Header content type
      header('Content-type: application/pdf');
      header('Content-Disposition: inline; filename="' . $id . ".pdf" . '"');
      header('Content-Transfer-Encoding: binary');
      header('Accept-Ranges: bytes');

      // Read the file
      @readfile($signedDocFolder . "\\" . $id . ".pdf");

      exit;
    } 
    elseif(isset($_POST[$preSigningEndpoint])) 
    {
      // Forming the page by inserting a variety of variable pieces to the template.
      // Note: since AsyncReq is an XML document, we need to URLEncode it to ensure
      // the page's syntactical safety. The signing service URLDecodes it.

      try
      {
        $docID = basename(tempnam($preSignedDocFolder, "temp"), '.tmp');
        $buf = file_get_contents($_FILES['pdffile']['tmp_name']);

        $asyncReq = preSignDocument($buf, $docID, $keyID, $keySecret, $usePKCS7Method, $useJSMode, $TSAURL, $preSignedDocFolder);

        echo "<head>" . $CrLf;
        echo "  <title>DCAuth demo: pre-signing completed</title>" . $CrLf;

        if ($useJSMode)
          echo "  <script type=\"text/javascript\" src=\"dcauth.js\" ></script>" . $CrLf;

        echo "  <script>" . $CrLf;
        echo "    // The async request itself." . $CrLf;

        if ($useJSMode)
          echo "    var asyncReq = " . $asyncReq . "; " . $CrLf;
        else
          echo "    var asyncReq = \"" . urlencode($asyncReq) . "\"; " . $CrLf;

        echo "" . $CrLf;
        echo "    // The URL where to submit the async response to." . $CrLf;
        echo "    var completionUri = \"" . $completionEndpoint . "\"; " . $CrLf;
        echo "" . $CrLf;

        if ($useJSMode)
        {
          echo "    try" . $CrLf;
          echo "    {" . $CrLf;
          echo "      var request = new DcauthAsyncRequest(asyncReq);" . $CrLf;
          echo "    }" . $CrLf;
          echo "    catch(err) {" . $CrLf;
          echo "      alert(\"Something went wrong: \" + err.message);" . $CrLf;
          echo "    }" . $CrLf;
          echo "    var hash = request.getHash();" . $CrLf;
          echo "" . $CrLf;
        }

        echo "    // This function relays the async request to the DCAuth signing service." . $CrLf;
        echo "    function submitAsyncRequest() {" . $CrLf;
        echo "      var xhr = new XMLHttpRequest();" . $CrLf;
        echo "      xhr.open(\"POST\", \"" . $signingServiceURL . "\", true);" . $CrLf;
        echo "      xhr.setRequestHeader(\"Content-Type\", \"application/x-www-form-urlencoded\");" . $CrLf;
        echo "      xhr.onload = function() {" . $CrLf;
        echo "        if (this.status != 200) {" . $CrLf;
        echo "          alert(\"Something went wrong: \" + this.response);" . $CrLf;
        echo "        } else {" . $CrLf;

        if ($useJSMode)
        {
          echo "          // Forming the response state basing on the javascript signature we received and saving it in a page element for debug purposes." . $CrLf;
          echo "          var response = new DcauthAsyncResponse(request);" . $CrLf;
          echo "          response.setSignature(this.response);" . $CrLf;
          echo "          var respElem = document.getElementById(\"asyncresptext\"); " . $CrLf;
          echo "          respElem.innerHTML = \"<p>The below async state was composed from the signature received from the service: ";
          echo "          <p><textarea cols=\\\"50\\\" rows=\\\"8\\\">\" + response.savetostring(); + \"</textarea>\";" . $CrLf;
        }
        else
        {
          echo "          // Saving a copy of the response in a page element for debug purposes." . $CrLf;
          echo "          var respElem = document.getElementById(\"asyncresptext\"); " . $CrLf;
          echo "          respElem.innerHTML = \"<p>The below async response was received: ";
          echo "          <p><textarea cols=\\\"50\\\" rows=\\\"8\\\">\" + this.response.replace(/&/g, \"&amp;\").replace(/</g, \"&lt;\").replace(/>/g, \"&gt;\").replace(/'/g, \"&#39;\").replace(/\"/g, \"&#34;\") + \"</textarea>\";" . $CrLf;
        }

        echo "" . $CrLf;
        echo "          // Reporting the success of the operation and asking the user whether we can proceed." . $CrLf;
        echo "          var res = confirm(\"A successful async response has been received from the signing endpoint (and a copy has been added to the page for your records). Press OK to submit this response back to the web application at \" + completionUri + \".\");" . $CrLf;
        echo "          if (res == true) {" . $CrLf;
        echo "            // Submitting the form to the web application's completion endpoint." . $CrLf;

        if ($useJSMode)
          echo "            document.getElementById(\"asyncresp\").value = response.savetostring();" . $CrLf;
        else
          echo "            document.getElementById(\"asyncresp\").value = this.response;" . $CrLf;

        echo "            document.getElementById(\"respform\").submit();" . $CrLf;
        echo "          }" . $CrLf;
        echo "        }" . $CrLf;
        echo "      }" . $CrLf;

        if ($useJSMode)
          echo "      xhr.send(hash);" . $CrLf;
        else
          echo "      xhr.send(asyncReq);" . $CrLf;

        echo "    }" . $CrLf;
        echo "  </script>" . $CrLf;
        echo "</head>" . $CrLf;

        if ($useJSMode)
          echo "<body style=\"font-family: sans-serif\" onload=''document.getElementById(\"reqhash\").innerHTML=hash;''>" . $CrLf;
        else
          echo "<body style=\"font-family: sans-serif\">" . $CrLf;

        echo "  <h1>DCAuth demo: the web application</h1>" . $CrLf;
        echo "  <h2>Pre-signing completed. </h2>" . $CrLf;
        echo "  The PDFSigner component has successfully pre-signed the provided PDF document." . $CrLf;
        echo "  The pre-signed document has been assigned with a DocID of " . $docID . $CrLf;
        echo "  and saved to the following location: " . $preSignedDocFolder . "\\" . $docID . "." . $CrLf;
        echo "  <p>" . $CrLf;
        echo "  The async request shown below has been generated as a result of the pre-signing. " . $CrLf;

        if ($useJSMode)
        {
          echo "  This has also been included in the body of this page as a JSON object, and" . $CrLf;
          echo "  pre-processed to extract the hash. The hash is: <div id=\"reqhash\"></div>. " . $CrLf;
          echo "  This hash value will be submitted to your local signing endpoint by " . $CrLf;
          echo "  the embedded Javascript when you click the Sign" . $CrLf;
        }
        else
        {
          echo "  This has also been included in the body of this page, and will be " . $CrLf;
          echo "  submitted to your local signing endpoint by the embedded Javascript when you click the Sign" . $CrLf;
        }

        echo "  button below. The signing procedure is performed behind the scenes and " . $CrLf;
        echo "  is fully transparent for you." . $CrLf;
        echo "  <p>" . $CrLf;
        echo "  <textarea cols=\"50\" rows=\"8\">" . htmlentities($asyncReq) . "</textarea>." . $CrLf;
        echo "  <p>" . $CrLf;
        echo "  This page expects the signing endpoint to be accessible at " . $signingServiceURL . "." . $CrLf;
        echo "  If the endpoint cannot be reached at this address, the signing will fail." . $CrLf;
        echo "  <p>" . $CrLf;
        echo "  If the signing completes successfully and a good async response containing" . $CrLf;
        echo "  the signature has been received from the signing service, this page will submit " . $CrLf;
        echo "  it back to the web app for completion of the signing operation. The page " . $CrLf;
        echo "  will notify you when this happens. " . $CrLf;
        echo "  <p>Now click the Sign button to pass the async request to the signing service: " . $CrLf;
        echo "  <button onclick=\"submitAsyncRequest()\">Sign</button>" . $CrLf;
        echo "  <p><div id=\"asyncresptext\"></div>" . $CrLf;
        echo "  <form method=\"post\" id=\"respform\">" . $CrLf;
        echo "    <input type=\"hidden\" value=\"\" id=\"asyncresp\" name=\"asyncresp\" />" . $CrLf;
        echo "    <input type=\"hidden\" name=\"" . $completionEndpoint . "\" value=\"1\" />" . $CrLf;
        echo "  </form>" . $CrLf;
        echo "</body>";
      }
      catch (exception $ex) 
      {
        errorPage($ex->getMessage());
      }
    } 
    elseif(isset($_POST[$completionEndpoint])) 
    {
      try
      {
        $docID = completeSigning($_POST['asyncresp'], $preSignedDocFolder, $signedDocFolder, $keyID, $keySecret, $TSAURL, $useJSMode);

        echo "<head><title>DCAuth demo: signing completed</title></head>" . $CrLf;
        echo "<body style=\"font-family: sans-serif\">" . $CrLf;
        echo "  <h1>DCAuth demo: the web application</h1>" . $CrLf;
        echo "  <h2>Signing completed successfully</h2>" . $CrLf;
        echo "  The remote signing procedure for your document has completed successfully. " . $CrLf;
        echo "  Click <a href=\"?" . $getFileEndpoint . "&id=" . $docID . "\" target=\"_blank\">here</a> to download the signed document. Click <a href=\"/\">here</a> to start again." . $CrLf;
        echo "</body>";
      }
      catch (exception $ex) 
      {
        errorPage($ex->getMessage());
      }
    }
    else 
    {
      // Outputs welcome/information page. It is only used for debug/testing purposes,
      // a live signing service does not need it.
      echo "<head><title>DCAuth demo: welcome</title></head>" . $CrLf;
      echo "<body style=\"font-family: sans-serif\">" . $CrLf;
      echo "  <h1>DCAuth demo: the web application</h1>" . $CrLf;
      echo "  <h2>Welcome to DCAuth demo</h2>" . $CrLf;
      echo "  When you click the Start button, a chosen PDF document will be uploaded" . $CrLf;
      echo "  (POST'ed) to the pre-signing page. During the generation of the pre-signing" . $CrLf;
      echo "  page the following will happen: " . $CrLf;
      echo "  <ul>" . $CrLf;
      echo "    <li>A document hash will be calculated and incorporated into an &quot;async request&quot;," . $CrLf;
      echo "    <li>The async request will be incorporated into the body of the produced page, " . $CrLf;
      echo "    <li>A pre-signed copy of the document will be saved to the special directory on the web server." . $CrLf;
      echo "  </ul>" . $CrLf;
      echo "  When your browser receives the output of the pre-signing page," . $CrLf;
      echo "  it will run the Javascript embedded into the page to pass the async request" . $CrLf;
      echo "  to the local signing service and ask it to sign the hash." . $CrLf;
      echo "  This web application is configured to assume that the signing" . $CrLf;
      echo "  service is accessible from the browser at the following location: " . $signingServiceURL . ". " . $CrLf;
      echo "  Please make sure the signing service is active before proceeding to the " . $CrLf;
      echo "  pre-signing stage." . $CrLf;
      echo "  <p>" . $CrLf;
      echo "  If you are using the second part of the DCAuth demo as your signing service," . $CrLf;
      echo "  you can check its availability by following <a href=\"" . $signingServiceURL . "\" target=\"_blank\">this link</a>" . $CrLf;
      echo "  (the service should respond with a welcome/status page)" . $CrLf;
      echo "  <form method=\"post\" enctype=\"multipart/form-data\">" . $CrLf;
      echo "    Please choose a PDF file to sign: <input type=\"file\" name=\"pdffile\" /><br />" . $CrLf;
      echo "    <input type=\"submit\" name=\"" . $preSigningEndpoint . "\" value=\"Start signing\" />" . $CrLf;
      echo "  </form>" . $CrLf;
      echo "</body>";
    }
?>
<br/>
<br/>
<br/>
<hr/>
NOTE: These pages are simple demos, and by no means complete applications.  They
are intended to illustrate the usage of the SecureBlackbox objects in a simple,
straightforward way.  What we are hoping to demonstrate is how simple it is to
program with our components.  If you want to know more about them, or if you have
questions, please visit <a href="http://www.nsoftware.com/?demopg-SBPHA" target="_blank">www.nsoftware.com</a> or
contact our technical <a href="http://www.nsoftware.com/support/">support</a>.
<br/>
<br/>
Copyright (c) 2023 /n software inc.
<br/>
<br/>
</div>

<div id="footer">
<center>
SecureBlackbox 2022 - Copyright (c) 2023 /n software inc. - For more information, please visit our website at <a href="http://www.nsoftware.com/?demopg-SBPHA" target="_blank">www.nsoftware.com</a>.
</center>
</div>

</body>
</html>

<?php if ($sendBuffer) ob_end_flush(); else ob_end_clean(); ?>
