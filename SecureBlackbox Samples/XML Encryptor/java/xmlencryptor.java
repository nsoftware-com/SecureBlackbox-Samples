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
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;

import secureblackbox.*;

public class xmlencryptor extends ConsoleDemo {
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
						"  xmlencryptor -- SecureBlackbox XMLEncryptor Demo Application\n\n" +
						"SYNOPSIS\n" +
						"  xmlencryptor <-input input_file> <-output output_file> [-datatype encrypted_data_type] [-encmethod encryption_method] \n" +
						"            [-xmlnode xml_node] [-enckey] [-enckeytype encryption_key_type] [-transport key_transport_method] \n" +
						"            [-wrap key_wrap_method] [-cert certificate_file] [-certpass certificate_password] [-pass key_password] \n\n" +
						"DESCRIPTION\n" +
						"  XMLEncryptor demonstrates the usage of XMLEncryptor from SecureBlackbox.\n" +
						"  Used to encrypt XML file.\n\n" +
						"  The options are as follows:\n\n" +
						"  -input        An input XML file to encrypt (Required). \n\n" +
						"  -output       Where the encrypted XML file will be saved (Required). \n\n" +
						"  -datatype     The encryption data type to use. Enter the corresponding number. Valid values: \n\n" +
						"                  0 - Element \n" +
						"                  1 - Content \n\n" +
						"  -encmethod    The encryption method to use. Valid values: 3DES, AES128, AES192, AES256, Camellia128, Camellia192, Camellia256, DES, RC4, SEED \n\n" +
						"  -xmlnode      The xml node value. \n\n" +
						"  -enckey       Whether to use key encryption. \n\n" +
						"  -enckeytype   The encryption key type to use. Enter the corresponding number. Valid values: \n\n" +
						"                  0 - KeyTransport \n" +
						"                  1 - KeyWrap \n\n" +
						"  -transport    The key transport method to use. Enter the corresponding number. Valid values: \n\n" +
						"                  0 - RSA15 \n" +
						"                  1 - RSAOAEP \n\n" +
						"  -wrap         The key wrap method to use. Valid values: 3DES, AES128, AES192, AES256, Camellia128, Camellia192, Camellia256, SEED. \n\n" +
						"  -cert         The certificate used to encrypt file. \n\n" +
						"  -certpass     The password for the certificate. \n\n" +
						"  -pass         The password for the encrypting. \n\n" +
						"EXAMPLES\n" +
						"  xmlencryptor -input C:\\xml\\myfile.xml -output C:\\xml\\myencfile.xml -pass mypassword \n\n" +
						"  xmlencryptor -input C:\\xml\\myfile.xml -output C:\\xml\\myencfile.xml -enckey -pass mypassword -datatype 1 -wrap AES192 \n\n" +
						"  xmlencryptor -input C:\\xml\\myfile.xml -output C:\\xml\\myencfile.xml -enckey -enckeytype 0 -cert C:\\certs\\mycert.pfx -certpass mypassword \n" +
						"              -datatype 2 -mimetype \"Test mime type\" -external C:\\xml\\external.xml -transport 1 \n"
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

	public static void main(String[] args) {
		if (args.length == 0) {
			displayHelp("");
			return;
		}

		try {
			XMLEncryptor encryptor = new XMLEncryptor();
			encryptor.setUseGCM(false);

			boolean encryptKey = false;
			int encryptedDataType = 0;
			int enckeytype = 1;
			int transportmethod = 0;
			String wrapmethod = "3DES";
			String mimetype = "";
			String keyPass = "";
			String externalFile = "";

			if (optext(args, "-input")) {
				encryptor.setInputFile(optval(args, "-input"));
			} else {
				displayHelp("-input is required.");
				return;
			}

			if (optext(args, "-output")) {
				encryptor.setOutputFile(optval(args, "-output"));
			} else {
				displayHelp("-output is required.");
				return;
			}

			if (optext(args, "-encmethod")) {
				encryptor.setEncryptionMethod(optval(args, "-encmethod"));
			}

			if (optext(args, "-xmlnode")) {
				encryptor.setXMLNode(optval(args, "-xmlnode"));
			}

			if (optext(args, "-datatype")) {
				encryptedDataType = Integer.parseInt(optval(args, "-datatype"));
			}

			if (optext(args, "-mimetype")) {
				mimetype = optval(args, "-mimetype");
			}

			if (optext(args, "-enckey")) {
				encryptKey = true;
			}

			if (optext(args, "-enckeytype")) {
				enckeytype = Integer.parseInt(optval(args, "-enckeytype"));
			}

			if (optext(args, "-transport")) {
				transportmethod = Integer.parseInt(optval(args, "-transport"));
			}

			if (optext(args, "-wrap")) {
				wrapmethod = optval(args, "-wrap");
			}

			if (optext(args, "-pass")) {
				keyPass = optval(args, "-pass");
			}

			if (optext(args, "-external")) {
				externalFile = optval(args, "-external");
			}

			encryptor.setEncryptKey(encryptKey);
			switch (encryptedDataType) {
				case 1:
					encryptor.setEncryptedDataType(XMLEncryptor.cxedtContent);
					break;
				case 2:
					encryptor.setEncryptedDataType(XMLEncryptor.cxedtExternal);
					if (mimetype.length() > 0)
						encryptor.config("MimeType=" + mimetype);

					try {
						encryptor.setExternalData(Files.readAllBytes(Paths.get(externalFile)));
					} catch (Exception ex) {
						displayError(ex);
					}

					break;
				default:
					encryptor.setEncryptedDataType(XMLEncryptor.cxedtElement);
					break;
			}

			if (encryptor.isEncryptKey()) {
				if (enckeytype == 0) {
					String certFile = "";
					String certPass = "";
					if (optext(args, "-cert")) {
						certFile = optval(args, "-cert");
					} else {
						System.out.println();
						displayHelp("-cert is required.");
						return;
					}

					if (optext(args, "-certpass")) {
						certPass = optval(args, "-certpass");
					}

					encryptor.setKeyEncryptionType(XMLEncryptor.cxetKeyTransport);

					if (transportmethod == 0)
						encryptor.setKeyTransportMethod(XMLEncryptor.cxktRSA15);
					else
						encryptor.setKeyTransportMethod(XMLEncryptor.cxktRSAOAEP);

					CertificateManager cm = new CertificateManager();
					cm.importFromFile(certFile, certPass);
					encryptor.setKeyEncryptionCertificate(cm.getCertificate());
				} else {
					encryptor.setKeyEncryptionType(XMLEncryptor.cxetKeyWrap);

					encryptor.setKeyWrapMethod(wrapmethod);
					encryptor.setKeyEncryptionKey(getKey(encryptor.getKeyWrapMethod(), keyPass));
				}
			} else {
				encryptor.setEncryptionKey(getKey(encryptor.getEncryptionMethod(), keyPass));
			}

			encryptor.encrypt();

			System.out.println("XML file successfully encrypted");

			confirmExit();
		} catch (Exception ex) {
			displayError(ex);
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



