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

import static secureblackbox.PGPSignature.*;

public class pgpwriter extends ConsoleDemo {
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
				"  pgpwriter -- SecureBlackbox PGPWriter Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  pgpwriter <-input input_file> <-output output_file> <-pubkey public_key_file> <-seckey secret_key_file>\n" +
				"            <-keypass keys_password> <-pass encryption_password> \n\n" +
				"DESCRIPTION\n" +
				"  PGPWriter demonstrates the usage of PGPWriter from SecureBlackbox.\n" +
				"  Used to create encrypted and signed OpenPGP-compliant files. \n\n" +
				"  The options are as follows:\n\n" +
				"  -input        An input file to sign (Required).\n\n" +
				"  -output       Where the ASiC will be saved (Required).\n\n" +
				"  -pubkey       The public key used to encrypt file (Required).\n\n" +
				"  -seckey       The secret (private) key used to sign file (Required).\n\n" +
				"  -keypass      The password for the keys (Required).\n\n" +
				"  -pass         The password for encryption (Required).\n"
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

	private static String keypass = "";

	public static void main(String[] args) {
		if (args.length == 0) {
			displayHelp("");
			return;
		}

		PGPWriter writer = new PGPWriter();
		PGPKeyring keyring = new PGPKeyring();

		try {
			writer.addPGPWriterEventListener(new PGPWriterEventListener() {
				@Override
				public void error(PGPWriterErrorEvent e) {

				}

				@Override
				public void externalSign(PGPWriterExternalSignEvent e) {

				}

				@Override
				public void keyPassphraseNeeded(PGPWriterKeyPassphraseNeededEvent e) {
					e.passphrase = keypass;
				}

				@Override
				public void notification(PGPWriterNotificationEvent e) {

				}

				@Override
				public void progress(PGPWriterProgressEvent e) {

				}

				@Override
				public void supercoreIntercept(PGPWriterSupercoreInterceptEvent e) {

				}
			});

			if (optext(args, "-input")) {
				writer.setInputFile(optval(args, "-input"));
			} else {
				displayHelp("-input is required.");
				return;
			}

			if (optext(args, "-output")) {
				writer.setOutputFile(optval(args, "-output"));
			} else {
				displayHelp("-output is required.");
				return;
			}

			if (optext(args, "-pass")) {
				writer.setPassphrase(optval(args, "-pass"));
			} else {
				displayHelp("-pass is required.");
				return;
			}

			if (optext(args, "-keypass")) {
				keypass = optval(args, "-keypass");
			} else {
				displayHelp("-keypass is required.");
				return;
			}

			if (optext(args, "-pubkey")) {
				keyring.importFromFile(optval(args, "-pubkey"));
			} else {
				displayHelp("-pubkey is required.");
				return;
			}

			if (optext(args, "-seckey")) {
				keyring.importFromFile(optval(args, "-seckey"));
			} else {
				displayHelp("-seckey is required.");
				return;
			}

			for (int i = 0; i < keyring.getKeys().size(); i++) {
				if (keyring.getKeys().item(i).getIsSecret())
				{
					writer.getSigningKeys().add(keyring.getKeys().item(i));
					break;
				}
			}

			for (int i = 0; i < keyring.getKeys().size(); i++) {
				if (keyring.getKeys().item(i).getIsPublic())
				{
					writer.getEncryptingKeys().add(keyring.getKeys().item(i));
					break;
				}
			}

			writer.encryptAndSign();

			System.out.println("The file were encrypted and signed successfully.\n");

			confirmExit();
		} catch (Exception ex) {
			ConsoleDemo.displayError(ex);
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



