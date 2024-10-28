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
import secureblackbox.*;
import static secureblackbox.SymmetricCrypto.*;

public class jwencryption extends ConsoleDemo {
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
						"  jwencryption -- SecureBlackbox SymmetricCrypto Demo Application\n\n" +
						"SYNOPSIS\n" +
						"  jwencryption <-e/-d> <-input input_data> <-pass encryption_password> [-compact] [-encalg encryption_algorithm]\n\n" +
						"DESCRIPTION\n" +
						"  This sample illustrates how to encrypt text to a JW token with a password.\n" +
						"  Used to encrypt and decrypt data.\n\n" +
						"  The options are as follows:\n\n" +
						"  -e            Whether to encrypt input data. \n\n" +
						"  -d            Whether to decrypt input data. \n\n" +
						"  -input        An input data to encrypt/decrypt (Required). \n\n" +
						"  -pass         Password for encryption (Required). \n\n" +
						"  -compact      Whether to use compact format \n\n" +
						"  -encalg       The encryption algorithm to use. Valid values: RC2, RC4, DES, 3DES, AES128, AES192, AES256, Blowfish \n\n" +
						"EXAMPLES\n" +
						"	jwencryption -e -input \"And now that you donÃ¢â‚¬â„¢t have to be perfect, you can be good.\" -pass mypassword -encalg AES256 \n\n" +
						"	jwencryption -d -input eyJhbGciOiJkaXIiLCJlbmMiOiJBMjU2Q0JDLUhTNTEyIn0..kuN2U -pass mypassword -compact \n"
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

	public static void main(String[] args) {
		if (args.length == 0) {
			displayHelp("");
			return;
		}

		SymmetricCrypto crypto = new SymmetricCrypto();
		try {
			boolean encrypt = false;
			boolean decrypt = false;
			byte[] inputB;
			String pass = "";
			boolean compact = false;

			if (optext(args, "-e")) {
				encrypt = true;
			}

			if (optext(args, "-d")) {
				decrypt = true;
			}

			if (!(encrypt || decrypt)) {
				displayHelp("-e or -d is required.");
				return;
			}

			if (encrypt && decrypt) {
				displayHelp("Use only one -e or -d parameter.");
				return;
			}

			if (optext(args, "-input")) {
				inputB = optval(args, "-input").getBytes();
			} else {
				displayHelp("-input is required.");
				return;
			}

			if (optext(args, "-pass")) {
				pass = optval(args, "-pass");
			} else {
				displayHelp("-pass is required.");
				return;
			}

			if (optext(args, "-encalg")) {
				crypto.setEncryptionAlgorithm(optval(args, "-encalg"));
			}

			if (optext(args, "-compact")) {
				compact = true;
			}

			// genarate key from password
			CryptoKeyManager keymanager = new CryptoKeyManager();
			keymanager.deriveKey(256, pass, "");
			keymanager.getKey().setIV(new byte[16]);
			crypto.setKey(keymanager.getKey());

			if (encrypt) {
				if (compact) {
					crypto.setOutputEncoding(cetCompact);
				} else {
					crypto.setOutputEncoding(cetJSON);
				}

				byte[] outputB = crypto.encrypt(inputB);

				System.out.println("Encrypted token: " + new String(outputB));
			} else {
				if (compact) {
					crypto.setInputEncoding(cetCompact);
				} else {
					crypto.setInputEncoding(cetJSON);
				}

				byte[] outputB = crypto.decrypt(inputB);

				System.out.println("Decrypted string: " + new String(outputB));
			}

			System.out.println();
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



