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
import java.util.Arrays;
import java.nio.charset.StandardCharsets;
import secureblackbox.*;

public class xmldecryptor extends ConsoleDemo {

	static String certFile = "";
	static String certPass = "";
	static String keyPass = "";
	static String externalFile = "";

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
			System.out.println("usage: xmldecryptor [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password] [-pass key_password] [-external external_file]");
			System.out.println("Options: ");
			System.out.println("  -input        An input file to decrypt (Required).");
			System.out.println("  -output       Where the decrypted XML file will be saved (Required).");
			System.out.println("  -cert         The certificate used to decrypt file.");
			System.out.println("  -certpass     The password for the certificate.");
			System.out.println("  -pass         The password for the decrypting.");
			System.out.println("  -external     The file where the external data will be saved.");
			System.out.println("\r\nExample: xmldecryptor -input C:\\xml\\myencfile.xml -output C:\\xml\\myfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword");
		} else {
			try {
				Xmldecryptor decryptor = new Xmldecryptor();
				System.out.println("***************************************************************************************************");
				System.out.println("* This sample illustrates the use of XMLDecryptor component for decrypting XML documents. *");
				System.out.println("***************************************************************************************************\n");

				for (int i = 0; i < args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-input"))
							decryptor.setInputFile(args[i + 1]); // args[i+1] corresponds to the value of argument [i]
						if (args[i].equals("-output"))
							decryptor.setOutputFile(args[i + 1]);
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

				decryptor.addXmldecryptorEventListener(new XmldecryptorEventListener() {
				@Override
					public void decryptionInfoNeeded(XmldecryptorDecryptionInfoNeededEvent e) {
						String t = "";
						if (decryptor.isUseGCM())
							t = "-GCM";
						System.out.println("Encryption method: " + decryptor.getEncryptionMethod() + t);

						if (decryptor.getEncryptedDataType() == Xmldecryptor.cxedtElement)
							t = "Element";
						else if (decryptor.getEncryptedDataType() == Xmldecryptor.cxedtContent)
							t = "Content";
						else
							t = "External";
						System.out.println("Encrypted data type: " + t);

						if (decryptor.isEncryptKey())
						{
							System.out.println("EncryptKey: true");
							if (decryptor.getKeyEncryptionType() == Xmldecryptor.cxetKeyTransport)
							{
								System.out.println("Key encryption type: transport");
								if (decryptor.getKeyTransportMethod() == Xmldecryptor.cxktRSA15)
									t = "RSA v1.5";
								else
									t = "RSA-OAEP";

								System.out.println("Key transport method: " + t);
							}
							else
							{
								System.out.println("Key encryption type: wrap");
								System.out.println("Key wrap method: " + decryptor.getKeyWrapMethod());
							}
						}
						else
							System.out.println("EncryptKey: false");

						try {
							t = decryptor.config("KeyName");
							if (!t.isEmpty())
								System.out.println("Key name: " + t);

							t = decryptor.config("MimeType");
							if (!t.isEmpty())
								System.out.println("Mime type: " + t);
						}
						catch (Exception ex)
						{
							System.out.println(ex.getMessage());
						}
						System.out.println("");

						try
						{
							if (decryptor.isEncryptKey())
							{
								if (decryptor.getKeyEncryptionType() == Xmldecryptor.cxetKeyTransport)
								{
									decryptor.setKeyDecryptionCertificate(loadCertificate(certFile, certPass));
								}
								else
								{
									decryptor.setKeyDecryptionKey(getKey(decryptor.getKeyWrapMethod(), keyPass));
								}
							}
							else
							{
								decryptor.setDecryptionKey(getKey(decryptor.getEncryptionMethod(), keyPass));
							}
						}
						catch (Exception ex)
						{
							System.out.println(ex.getMessage());
						}
					}

					@Override
					public void error(XmldecryptorErrorEvent e) {

					}

					@Override
					public void externalDecrypt(XmldecryptorExternalDecryptEvent e) {

					}

					@Override
					public void notification(XmldecryptorNotificationEvent e) {

					}

					@Override
					public void saveExternalData(XmldecryptorSaveExternalDataEvent e) {
						try
						{
							FileOutputStream fos = new FileOutputStream(externalFile);
							fos.write(e.externalData);
							fos.close();
						}
						catch (Exception ex) {}
					}

					@Override
					public void supercoreIntercept(XmldecryptorSupercoreInterceptEvent e) {

					}
				});

				decryptor.decrypt();

				System.out.println("XML file successfully decrypted");

			} catch (Exception ex) {
				System.out.println(ex.getMessage());
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



