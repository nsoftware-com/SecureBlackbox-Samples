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

import static secureblackbox.PublicKeyCrypto.*;

public class publickeycrypto extends ConsoleDemo {
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
				"  publickeycrypto -- SecureBlackbox PublicKeyCrypto Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  publickeycrypto <-s/-v> <-input input_file> <-cert certificate_file> [-certpass certificate_password] \n" +
				"             [-output output_file] [-sig signature_file] [-encoding encoding]\n\n" +
				"DESCRIPTION\n" +
				"  PublicKeyCrypto demonstrates the usage of PublicKeyCrypto from SecureBlackbox.\n" +
				"  Used to sign and verify files.\n\n" +
				"  The options are as follows:\n\n" +
				"  -s            Sign input file and save to output \n\n" +
				"  -v            Verify signature using original file (input) \n\n" +
				"  -input        An input file to sign or verify (Required). \n\n" +
				"  -cert         The certificate used to sign files (Required).\n\n" +
				"  -certpass     The password for the signing certificate.\n\n" +
				"  -output       Where the signed file will be saved (Required for signing). \n\n" +
				"  -sig          An signature file to verify (Required on verifing). \n\n" +
				"  -encoding     The encoding of hash. Valid values:\n\n" +
				"                  0 - CET_DEFAULT\n" +
				"                  1 - CET_BINARY\n" +
				"                  2 - CET_BASE_64\n" +
				"                  3 - CET_COMPACT\n" +
				"                  4 - CET_JSON\n\n" +
				"EXAMPLES\n" +
				"  publickeycrypto -s -input C:\\cypto\\helloworld.txt -output C:\\cypto\\signature.dat -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n" +
				"  publickeycrypto -v -input C:\\cypto\\helloworld.txt -signature C:\\cypto\\signature.dat -cert C:\\certs\\mycert.pfx -certpass mypassword -encoding 2 \n"
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

		PublicKeyCrypto crypto = new PublicKeyCrypto();
		try {
			boolean sign = false;
			boolean verify = false;
			String input;
			int encoding;

			if (optext(args, "-s")) {
				sign = true;
			}

			if (optext(args, "-v")) {
				verify = true;
			}

			if (!(sign || verify)) {
				displayHelp("-s or -v is required.");
				return;
			}

			if (sign && verify) {
				displayHelp("Use only one -s or -v parameter.");
				return;
			}

			if (optext(args, "-input")) {
				input = optval(args, "-input");
			} else {
				displayHelp("-input is required.");
				return;
			}

			if (optext(args, "-cert")) {
				CertificateManager cm = new CertificateManager();
				CryptoKeyManager ckm = new CryptoKeyManager();

				cm.importFromFile(optval(args, "-cert"), optval(args, "-certpass"));

				ckm.setCertificate(cm.getCertificate());
				ckm.importFromCert();

				crypto.setKey(ckm.getKey());
			} else {
				displayHelp("-cert is required.");
				return;
			}

			if (optext(args, "-encoding")) {
				encoding = Integer.parseInt(optval(args, "-encoding"));
			} else {
				encoding = cetBase64;
			}

			if (sign) {
				String output;
				if (optext(args, "-output")) {
					output = optval(args, "-output");
				} else {
					displayHelp("-output is required.");
					return;
				}

				crypto.setOutputEncoding(encoding);

				crypto.signFile(input, output, true);

				System.out.println("The file was signed successfully.");
			} else {
				String signature;
				if (optext(args, "-sig")) {
					signature = optval(args, "-sig");
				} else {
					displayHelp("-sig is required.");
					return;
				}

				crypto.setInputEncoding(encoding);

				crypto.verifyDetachedFile(input, signature);

				switch (crypto.getSignatureValidationResult()) {
					case svtValid:
						System.out.println("Verification succeeded.\n");
						break;
					case svtCorrupted:
						System.out.println("Verification corrupted.\n");
						break;
					case svtSignerNotFound:
						System.out.println("Signer not found.\n");
						break;
					case svtFailure:
						System.out.println("Verification failed.\n");
						break;
					default:
						System.out.println("Verification unknown.\n");
						break;
				}
			}

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



