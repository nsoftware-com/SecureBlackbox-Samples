<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - Distributed Crypto</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - Distributed Crypto">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>Distributed Crypto</h2>
<p>A simple example of the DC technology. The sample incorporates two counterparts of DC: the application part is represented with PDFSigner control, and the private key part is represented with DCAuth control.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_dcauth.php');
require_once('../include/secureblackbox_pdfsigner.php');
require_once('../include/secureblackbox_const.php');

?>

<?php $sendBuffer = TRUE; ob_start(); session_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - DC PDF Signer</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - DC PDF Signer"></head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>DC PDF Signer</h2>
<p>Use this demo to learn how to create signed PDF documents using DC.</p>
<a href="seecode.php?dcauth.php">[See The Code]</a>
<a href="default.php">[Other Demos]</a>
<a href="secureblackbox.chm">[Help]</a>
<hr/>

<?php
require_once('../include/secureblackbox_pdfsigner.php');
require_once('../include/secureblackbox_const.php');
?>

<style>
table { width: 100% !important; }
td { white-space: nowrap; }
td input { width: 100%; }
td:last-child { width: 100%; }
</style>

<div width="90%">
  <h2>DC Signing Demo - step #1</h2>
  
  <form method="POST">
    <b>Author:</b><br/>
    <input type="text" name="author" value="Author name" /><br/>
    <b>Reason:</b><br/>
    <input type="text" name="reason" value="Reason" /><br/>
    <b>File path on server:</b><br/>
    <input type="text" name="inputFile" value="c:\temp\pdf\test.pdf" /><br/>
    <input type="submit" value="Start signing" />
  </form>
</div><br/>

<?php if (isset($_SESSION['encodedRequest']) && !empty($_SESSION['encodedRequest'])) { ?>
<div width="90%">
  <h2>DC Signing Demo - step #2</h2>
  <h5>Please make sure that both DC desktop application and service are running before you press "Sign" button!</h5>
  
  <form id="myForm" method="POST">
    <input type="hidden" id="data" name="data" value="<?php echo $_SESSION['encodedRequest']; ?>" />
    <input type="submit" value="Sign" />
  </form>
  
  <div id="error"></div>
  <div id="success"></div>
</div><br/>  
<?php } ?>

<?php
  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $pdfsigner = new SecureBlackbox_PDFSigner();

    try {
      if (!isset($_SESSION['encodedRequest'])) {
        $preSignedFile = $_REQUEST['inputFile'] . ".presigned";
        $_SESSION['preSignedFile'] = $preSignedFile;
        
        $pdfsigner->setInputFile($_REQUEST['inputFile']);
        $pdfsigner->setOutputFile($preSignedFile);
        
        $pdfsigner->setSigAuthorName($_REQUEST['author']);
        $pdfsigner->setSigReason($_REQUEST['reason']);
        
        $pdfsigner->setExternalCryptoPublicKeyAlgorithm("rsaEncryption");
        $pdfsigner->setExternalCryptoHashAlgorithm("SHA256");
        
        // These values should be also set in DC signing application
        $pdfsigner->setExternalCryptoKeyID("key_id");
        $pdfsigner->setExternalCryptoKeySecret("key_secret");
        
        $pdfsigner->doConfig("TempPath=" . dirname($_REQUEST['inputFile']));

        $request = $pdfsigner->doSignAsyncBegin();

        $encodedRequest = base64_encode($request);
        $_SESSION['encodedRequest'] = $encodedRequest;
        
        header('Location: '.$_SERVER['PHP_SELF']);
        die();
      } else {
        $reply = file_get_contents('php://input');
        
        $pdfsigner->setInputFile($_SESSION['preSignedFile']);
        $pdfsigner->setOutputFile(dirname($_SESSION['preSignedFile']) . "\signed.pdf");
        
        $pdfsigner->doConfig("TempPath=" . dirname($_SESSION['preSignedFile']));
        unset($_SESSION['encodedRequest']);
        
        $pdfsigner->doSignAsyncEnd($reply);
      }
    }
    catch (exception $e) {
      unset($_SESSION['encodedRequest']);
      echo "<h2>Signing Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  } else if ($_SERVER['REQUEST_METHOD'] == "GET" && isset($_GET["reset"])) {
    unset($_SESSION['encodedRequest']);
    header('Location: '.$_SERVER['PHP_SELF']);
    die();
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
questions, please visit <a href="http://www.nsoftware.com/?demopg-SBPFA" target="_blank">www.nsoftware.com</a> or
contact our technical <a href="http://www.nsoftware.com/support/">support</a>.
<br/>
<br/>
Copyright (c) 2022 /n software inc. - All rights reserved.
<br/>
<br/></div>

<div id="footer">
<center>
SecureBlackbox 2022 - Copyright (c) 2022 /n software inc. - All rights reserved. - For more information, please visit our website at <a href="http://www.nsoftware.com/?demopg-SBPFA" target="_blank">www.nsoftware.com</a>.</center></div>

<script src="https://code.jquery.com/jquery-3.4.1.min.js"></script>
<script>
var login = null;
var pass = "";
var serverUrl = "https://localhost:8888";
var applicationUrl = "http://localhost";
var signFormId = "myForm";
var useSimplePOST = true;

function formToJSON(formId) {
    var formObj = {};
    var inputs = $('#' + formId).serializeArray();

    $.each(inputs, function (i, input) {
        formObj[input.name] = input.value;
    });

    return formObj;
}

function showError(jqXHR, exception) {
    var msg = '';
    if (jqXHR.status === 0) {
        msg = 'DC desktop application does not respond!\n Please check if its running in background.';
    } else if (jqXHR.status == 500) {
        msg = 'Internal Server Error [500]!';
    } else if (jqXHR.status == 404) {
        msg = 'Page not found [404]!';
    } else if (exception === 'timeout') {
        msg = 'Time out error!';
    } else if (exception === 'abort') {
        msg = 'Ajax request aborted!';
    } else {
        msg = 'Uncaught Error.\n' + jqXHR.responseText;
    }
    $('#error').text(msg);
    $('#error').show();
    $("#success").hide();
    
    // reset session
    $.ajax({
      url: applicationUrl + "?reset"
    });
}

function showSuccess(data) {
    $("#success").text("File signed successfully!");
    $('#success').show();
    $('#error').hide();
}

function postServerResponse(response) {
    if (useSimplePOST) {
        var toSend = atob(response.sign);

        // remove UTF-8 BOM
        if (toSend.charCodeAt(0) === 0xEF && toSend.charCodeAt(1) === 0xBB && toSend.charCodeAt(2) === 0xBF)
            toSend = toSend.substring(3);

        $.ajax({
            type: "POST",
            url: applicationUrl,
            contentType: "text/plain",
            data: toSend,
            processData: false,
            success: function (data) {
                showSuccess(data);
            },
            'error': function (jqXHR, exception) {
                showError(jqXHR, exception);
            }
        });
    } else {
        $.ajax({
            type: "POST",
            url: applicationUrl,
            contentType: "application/json",
            data: JSON.stringify(response),
            success: function (data) {
                showSuccess(data);
            },
            'error': function (jqXHR, exception) {
                showError(jqXHR, exception);
            }
        });
    }
}

$('#' + signFormId).on('submit', function (event) {
    event.preventDefault();

    if (login != null) {
        $.ajaxSetup({
            headers: {
                'Authorization': "Basic " + btoa(login + ":" + pass)
            }
        });
    }

    var json = formToJSON(signFormId);

    $.ajax({
        type: "POST",
        url: serverUrl + "/sign",
        contentType: "application/json",
        data: JSON.stringify(json),
        crossDomain: true,
        'success': function (data) {
            $('#error').text('');
            postServerResponse(data);
        },
        'error': function (jqXHR, exception) {
            showError(jqXHR, exception);
        }
    });
});

$(document).ready(function () {
    $('#error').hide();
    $('#success').hide();
});
</script>

</body></html>

<?php if ($sendBuffer) ob_end_flush(); else ob_end_clean(); ?>

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
