/*
 * SecureBlackbox 2024 Java Edition - Sample Project
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
	private static String optval(String[] args, String option) {
		for (int x = 0; x < args.length - 1; x++) {
			if (args[x].equalsIgnoreCase(option)) {
				return args[x + 1];
			}
		}
		return "";
	}

	private static boolean optext(String[] args, String option) {
		for (int x = 0; x < args.length; x++) {
			if (args[x].equalsIgnoreCase(option)) {
				return true;
			}
		}
		return false;
	}

	private static void displayHelp(String errMes) {
		System.out.println(
				"NAME\n" +
						"  xmldecryptor -- SecureBlackbox XMLDecryptor Demo Application\n\n" +
						"SYNOPSIS\n" +
						"  xmldecryptor <-input input_file> <-output output_file> [-cert certificate_file] [-certpass certificate_password] [-pass key_password] \n\n" +
						"DESCRIPTION\n" +
						"  XMLDecryptor demonstrates the usage of XMLDecryptor from SecureBlackbox.\n" +
						"  Used to decrypt XML file.\n\n" +
						"  The options are as follows:\n\n" +
						"  -input        An input file to decrypt (Required). \n\n" +
						"  -output       Where the decrypted XML file will be saved (Required). \n\n" +
						"  -cert         The certificate used to decrypt file. \n\n" +
						"  -certpass     The password for the certificate. \n\n" +
						"  -pass         The password for the decrypting. \n\n" +
						"EXAMPLES\n" +
						"  xmldecryptor -input C:\\xml\\myenc.xml -output C:\\xml\\myfile.xml -pass mtpassword \n" +
						"  xmldecryptor -input C:\\xml\\myenc.xml -output C:\\xml\\myfile.xml -cert C:\\certs\\mycert.pfx -certpass test -external C:\\xml\\external.xml \n"
		);

		if (errMes.length() > 0) {
			System.out.println("Error: " + errMes);
			System.out.println();
		}

		confirmExit();
	}

	private static void confirmExit() {
		System.out.println("Press Enter to exit the demo.");
		input();
	}

	private static byte[] getKey(String algorithm, String pass) {
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

	private static XMLDecryptor decryptor;
	private static String certFile = "";
	private static String certPass = "";
	private static String keyPass = "";
	private static String externalFile = "";

	public static void main(String[] args) {
		if (args.length == 0) {
			displayHelp("");
			return;
		}

		decryptor = new XMLDecryptor();
		try {
			if (optext(args, "-input")) {
				decryptor.setInputFile(optval(args, "-input"));
			} else {
				displayHelp("-input is required.");
				return;
			}

			if (optext(args, "-output")) {
				decryptor.setOutputFile(optval(args, "-output"));
			} else {
				displayHelp("-output is required.");
				return;
			}

			if (optext(args, "-cert")) {
				certFile = optval(args, "-cert");
			}

			if (optext(args, "-certpass")) {
				certPass = optval(args, "-certpass");
			}

			if (optext(args, "-external")) {
				externalFile = optval(args, "-external");
			}

			if (optext(args, "-pass")) {
				keyPass = optval(args, "-pass");
			}

			decryptor.addXMLDecryptorEventListener(new XMLDecryptorEventListener() {
				@Override
				public void decryptionInfoNeeded(XMLDecryptorDecryptionInfoNeededEvent e) {
					String t = "";
					if (decryptor.isUseGCM())
						t = "-GCM";
					System.out.println("Encryption method: " + decryptor.getEncryptionMethod() + t);

					if (decryptor.getEncryptedDataType() == XMLDecryptor.cxedtElement)
						t = "Element";
					else if (decryptor.getEncryptedDataType() == XMLDecryptor.cxedtContent)
						t = "Content";
					else
						t = "External";
					System.out.println("Encrypted data type: " + t);

					if (decryptor.isEncryptKey()) {
						System.out.println("EncryptKey: true");
						if (decryptor.getKeyEncryptionType() == XMLDecryptor.cxetKeyTransport) {
							System.out.println("Key encryption type: transport");
							if (decryptor.getKeyTransportMethod() == XMLDecryptor.cxktRSA15)
								t = "RSA v1.5";
							else
								t = "RSA-OAEP";

							System.out.println("Key transport method: " + t);
						} else {
							System.out.println("Key encryption type: wrap");
							System.out.println("Key wrap method: " + decryptor.getKeyWrapMethod());
						}
					} else
						System.out.println("EncryptKey: false");

					try {
						t = decryptor.config("KeyName");
						if (!t.isEmpty())
							System.out.println("Key name: " + t);

						t = decryptor.config("MimeType");
						if (!t.isEmpty())
							System.out.println("Mime type: " + t);
					} catch (Exception ex) {
						System.out.println(ex.getMessage());
					}
					System.out.println("");

					try {
						if (decryptor.isEncryptKey()) {
							if (decryptor.getKeyEncryptionType() == XMLDecryptor.cxetKeyTransport) {
								CertificateManager cm = new CertificateManager();
								cm.importFromFile(certFile, certPass);
								decryptor.setKeyDecryptionCertificate(cm.getCertificate());
							} else {
								decryptor.setKeyDecryptionKey(getKey(decryptor.getKeyWrapMethod(), keyPass));
							}
						} else {
							decryptor.setDecryptionKey(getKey(decryptor.getEncryptionMethod(), keyPass));
						}
					} catch (Exception ex) {
						System.out.println(ex.getMessage());
					}
				}

				@Override
				public void error(XMLDecryptorErrorEvent e) {

				}

				@Override
				public void externalDecrypt(XMLDecryptorExternalDecryptEvent e) {

				}

				@Override
				public void notification(XMLDecryptorNotificationEvent e) {

				}

				@Override
				public void saveExternalData(XMLDecryptorSaveExternalDataEvent e) {
					try {
						FileOutputStream fos = new FileOutputStream(externalFile);
						fos.write(e.externalData);
						fos.close();
					} catch (Exception ex) {
					}
				}

				@Override
				public void supercoreIntercept(XMLDecryptorSupercoreInterceptEvent e) {

				}
			});

			decryptor.decrypt();

			System.out.println("XML file successfully decrypted.\n");

			confirmExit();
		} catch (Exception ex) {
			System.out.println(ex.getMessage());
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
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
      String response = input();
      if (response.equals(""))
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

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}



