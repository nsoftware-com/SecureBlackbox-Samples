<?php

$site_name = $_SERVER['HTTP_HOST'];
$url_dir = "http://".$_SERVER['HTTP_HOST'].dirname($_SERVER['PHP_SELF']);
$url_this =  "http://".$_SERVER['HTTP_HOST'].$_SERVER['PHP_SELF'];

$upload_dir = "upload_files/";
$upload_url = $url_dir."/upload_files/";
$message ="";

function do_upload($upload_dir, $upload_url) {

	$temp_name = $_FILES['userfile']['tmp_name'];
	$file_name = $_FILES['userfile']['name']; 
	$file_type = $_FILES['userfile']['type']; 
	$file_size = $_FILES['userfile']['size']; 
	$result    = $_FILES['userfile']['error'];
	$file_url  = $upload_url.$file_name;
	$file_path = $upload_dir.$file_name;

	//File Name Check
    if ( $file_name =="") { 
    	$message = "Invalid File Name Specified";
    	return $message;
    }
    //File Size Check
    else if ( $file_size > 500000) {
        $message = "The file size is over 500K.";
        return $message;
    }
    //File Type Check
    else 
    if ( $file_type == "text/plain" ) {
        $message = "Sorry, You cannot upload any script file" ;
        return $message;
    }
    else
    if (!is_uploaded_file($temp_name))
    {
    	$message = "file " . $temp_name . "was not uploaded";
    	return $message;
    }

    $result  =  move_uploaded_file($temp_name, $file_path);
    if ($result)
    {
    	$message = "File uploaded successfully. The permanent URL is: <a href=$file_url>$file_url</a>";
    }
    else
    	$message = "Something is wrong with uploading a file.";

    return $message;
}

//create upload_files directory if not exist
//If it does not work, create on your own and change permission.
if (!is_dir("upload_files")) {
	die ("upload_files directory doesn't exist");
}

if ($_FILES['userfile']) {
	$message = do_upload($upload_dir, $upload_url);
	print $message;
}
else
{

?>

<form name="upload" id="upload" ENCTYPE="multipart/form-data" method="post">
  Upload Image<input type="file" id="userfile" name="userfile">
  <input type="submit" name="upload" value="Upload">
</form>  

<?php
}
?>