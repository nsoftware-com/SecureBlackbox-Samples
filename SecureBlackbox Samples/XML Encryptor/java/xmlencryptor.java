/*
 * SecureBlackbox 2022 Java Edition - Sample Project
 *
 * This sample project demonstrates the usage of SecureBlackbox in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/secureblackbox
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 */

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;

import secureblackbox.*;

public class xmlencryptor extends ConsoleDemo {

	public static Certificate loadCertificate(String file, String password)
	{
		Certificate cert = null;

		if (file.length() > 0)
		{
			try
			{
				Certificatemanager certmanager = new Certificatemanager();

				certmanager.importFromFile(file, password);

				cert = certmanager.getCertificate();
			}
			catch (Exception e)
			{
				System.out.println("Cannot load certificate!");
			}
		}

		return cert;
	}

	private static byte[] getKey(String algorithm, String pass)
	{
		int len = 0;

		if (algorithm.equalsIgnoreCase("AES128"))
			len = 16;
		else if (algorithm.equalsIgnoreCase("AES192"))
			len = 24;
		else if (algorithm.equalsIgnoreCase("AES256"))
			len = 32;
		else if (algorithm.equalsIgnoreCase("Camellia128"))
			len = 16;
		else if (algorithm.equalsIgnoreCase("Camellia192"))
			len = 24;
		else if (algorithm.equalsIgnoreCase("Camellia256"))
			len = 32;
		else if (algorithm.equalsIgnoreCase("DES"))
			len = 8;
		else if (algorithm.equalsIgnoreCase("3DES"))
			len = 24;
		else if (algorithm.equalsIgnoreCase("RC4"))
			len = 16;
		else if (algorithm.equalsIgnoreCase("SEED"))
			len = 16;

		// simple key derivation function from a Passphrase
		// TODO: replace with SHA256 hash or KDF
		String s = pass;
		while (s.length() < len)
			s = s + "/" + pass;

		return Arrays.copyOfRange(s.getBytes(StandardCharsets.UTF_8), 0, len);
	}

	public static void main(String[] args) {
		if (args.length < 6) {
			System.out.println("usage: xmlencryptor [-input input_file] [-output output_file] [-datatype encrypted_data_type] [-encmethod encryption_method]");
			System.out.println("                    [-xmlnode xml_node] [-mimetype mime_type] [-external external_file] [-enckey] [-enckeytype encryption_key_type] ");
			System.out.println("                    [-transport key_transport_method] [-wrap key_wrap_method] [-cert certificate_file] [-certpass certificate_password] [-pass key_password]");
			System.out.println("Options: ");
			System.out.println("  -input        An input XML file to encrypt (Required).");
			System.out.println("  -output       Where the encrypted XML file will be saved (Required).");
			System.out.println("  -datatype     The encryption data type to use. Enter the corresponding number. Valid values:");
			System.out.println("                  0 - cxedtElement");
			System.out.println("                  1 - cxedtContent");
			System.out.println("                  2 - cxedtExternal");
			System.out.println("  -encmethod    The encryption method to use. Valid values: 3DES, AES128, AES192, AES256, Camellia128, Camellia192, Camellia256, DES, RC4, SEED");
			System.out.println("  -xmlnode      The xml node value.");
			System.out.println("  -mimetype     The mime type.");
			System.out.println("  -external     The file where the external data will be saved.");
			System.out.println("  -enckey       Whether to use key encryption.");
			System.out.println("  -enckeytype   The encryption key type to use. Enter the corresponding number. Valid values:");
			System.out.println("                  0 - cxetKeyTransport");
			System.out.println("                  1 - cxetKeyWrap");
			System.out.println("  -transport    The key transport method to use. Enter the corresponding number. Valid values:");
			System.out.println("                  0 - cxktRSA15");
			System.out.println("                  1 - cxktRSAOAEP");
			System.out.println("  -wrap         The key wrap method to use. Valid values: 3DES, AES128, AES192, AES256, Camellia128, Camellia192, Camellia256, SEED.");
			System.out.println("  -cert         The certificate used to encrypt file.");
			System.out.println("  -certpass     The password for the certificate.");
			System.out.println("  -pass         The password for the encrypting.");
			System.out.println("\r\nExample: xmlencryptor -input C:\\xml\\myfile.xml -output C:\\xml\\myencfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword");
		} else {
			try {
				Xmlencryptor encryptor = new Xmlencryptor();
				encryptor.setUseGCM(false);
				System.out.println("***************************************************************************************************");
				System.out.println("* This sample illustrates the use of XMLEncryptor component for encrypting XML documents. *");
				System.out.println("***************************************************************************************************\n");

				boolean encryptKey = false;
				int encryptedDataType = 0;
				int enckeytype = 1;
				int transportmethod = 0;
				String wrapmethod = "3DES";
				String mimetype = "";
				String certFile = "";
				String certPass = "";
				String keyPass = "";
				String externalFile = "";

				for (int i = 0; i < args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-input"))
							encryptor.setInputFile(args[i + 1]); // args[i+1] corresponds to the value of argument [i]
						if (args[i].equals("-output"))
							encryptor.setOutputFile(args[i + 1]);
						if (args[i].equals("-datatype"))
							encryptedDataType = Integer.parseInt(args[i + 1]);
						if (args[i].equals("-encmethod"))
							encryptor.setEncryptionMethod(args[i + 1]);
						if (args[i].equals("-xmlnode"))
							encryptor.setXMLNode(args[i + 1]);
						if (args[i].equals("-mimetype"))
							mimetype = args[i + 1];
						if (args[i].equals("-enckey"))
							encryptKey = true;
						if (args[i].equals("-enckeytype"))
							enckeytype = Integer.parseInt(args[i + 1]);
						if (args[i].equals("-transport"))
							transportmethod = Integer.parseInt(args[i + 1]);
						if (args[i].equals("-wrap"))
							wrapmethod = args[i + 1];
						if (args[i].equals("-cert"))
							certFile = args[i + 1];
						if (args[i].equals("-certpass"))
							certPass = args[i + 1];
						if (args[i].equals("-pass"))
							keyPass = args[i + 1];
						if (args[i].equals("-external"))
							externalFile = args[i + 1];
					}
				}

				encryptor.setEncryptKey(encryptKey);
				switch (encryptedDataType)
				{
					case 1: encryptor.setEncryptedDataType(Xmlencryptor.cxedtContent); break;
					case 2:
						encryptor.setEncryptedDataType(Xmlencryptor.cxedtExternal);
						if (mimetype.length() > 0)
							encryptor.config("MimeType=" + mimetype);

						try
						{
							encryptor.setExternalData(Files.readAllBytes(Paths.get(externalFile)));
						}
						catch (Exception ex)
						{
							displayError(ex);
						}

						break;
					default: encryptor.setEncryptedDataType(Xmlencryptor.cxedtElement); break;
				}

				if (encryptor.isEncryptKey())
				{
					if (enckeytype == 0)
					{
						encryptor.setKeyEncryptionType(Xmlencryptor.cxetKeyTransport);

						if (transportmethod == 0)
							encryptor.setKeyTransportMethod(Xmlencryptor.cxktRSA15);
						else
							encryptor.setKeyTransportMethod(Xmlencryptor.cxktRSAOAEP);

						encryptor.setKeyEncryptionCertificate(loadCertificate(certFile, certPass));
					}
					else
					{
						encryptor.setKeyEncryptionType(Xmlencryptor.cxetKeyWrap);

						encryptor.setKeyWrapMethod(wrapmethod);
						encryptor.setKeyEncryptionKey(getKey(encryptor.getKeyWrapMethod(), keyPass));
					}
				}
				else
				{
					encryptor.setEncryptionKey(getKey(encryptor.getEncryptionMethod(), keyPass));
				}

				encryptor.encrypt();

				System.out.println("XML file successfully encrypted");

			}
			catch (Exception ex)
			{
				displayError(ex);
			}
		}
	}
}


class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }

  static String prompt(String label, String punctuation, String defaultVal)
  {
	System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
	String response = input();
	if(response.equals(""))
		return defaultVal;
	else
		return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof SecureBlackboxException) {
      System.out.print(" (" + ((SecureBlackboxException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }
}



