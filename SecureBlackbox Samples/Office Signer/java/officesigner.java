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

public class officesigner extends ConsoleDemo {
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
				"  officesigner -- SecureBlackbox OfficeSigner Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  officesigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password]\n" +
				"             [-sigtype signature_type] [-hashalg hash_algorithm] [-signdoc] [-signcore] [-signorigin]\n\n" +
				"DESCRIPTION\n" +
				"  OfficeSigner demonstrates the usage of OfficeSigner from SecureBlackbox.\n" +
				"  Used to sign office documents.\n\n" +
				"  The options are as follows:\n\n" +
				"  -input        An input file to sign (Required).\n\n" +
				"  -output       Where the signed file will be saved (Required).\n\n" +
				"  -cert         The certificate used to sign files (Required).\n\n" +
				"  -certpass     The password for the signing certificate.\n\n" +
				"  -sigtype      The type of signature to use. Enter the corresponding number. Valid values:\n\n" +
				"                  0 - OST_DEFAULT\n" +
				"                  1 - OST_BINARY_CRYPTO_API\n" +
				"                  2 - OST_BINARY_XML\n" +
				"                  3 - OST_OPEN_XML\n" +
				"                  4 - OST_OPEN_XPS\n" +
				"                  5 - OST_OPEN_DOCUMENT\n\n" +
				"  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224, MD5\n\n" +
				"  -signdoc      Whether to sign the document itself.\n\n" +
				"  -signcore     Whether to sign the core properties of the document.\n\n" +
				"  -signorigin   Whether to sign the signature origin.\n\n" +
				"EXAMPLES\n" +
				"  officesigner -input C:\\office\\helloworld.txt -output C:\\office\\myoffice.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n" +
				"  officesigner -input C:\\office\\helloworld.txt -output C:\\office\\myoffice.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n" +
				"             -sigtype 2 -hashalg SHA256 -signdoc -signcore -signorigin\n"
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

		try {
			OfficeSigner signer = new OfficeSigner();
			CertificateManager cm = new CertificateManager();

			if (optext(args, "-input")) {
				signer.setInputFile(optval(args, "-input"));
			} else {
				displayHelp("-input is required.");
				return;
			}

			if (optext(args, "-output")) {
				signer.setOutputFile(optval(args, "-output"));
			} else {
				displayHelp("-output is required.");
				return;
			}

			if (optext(args, "-cert")) {
				cm.importFromFile(optval(args, "-cert"), optval(args, "-certpass"));
				signer.setSigningCertificate(cm.getCertificate());
			} else {
				displayHelp("-cert is required.");
				return;
			}

			// Additional options
			if (optext(args, "-sigtype")) {
				signer.getNewSignature().setSignatureType(Integer.parseInt(optval(args, "-sigtype")));
			}
			if (optext(args, "-hashalg")) {
				signer.getNewSignature().setHashAlgorithm(optval(args, "-hashalg"));
			}
			signer.getNewSignature().setDocumentSigned(optext(args, "-signdoc"));
			signer.getNewSignature().setCorePropertiesSigned(optext(args, "-signcore"));
			signer.getNewSignature().setSignatureOriginSigned(optext(args, "-signorigin"));

			signer.sign();

			System.out.println("Office file successfully signed.\n");

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



