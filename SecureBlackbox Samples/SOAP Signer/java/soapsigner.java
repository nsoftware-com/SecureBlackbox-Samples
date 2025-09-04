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

public class soapsigner extends ConsoleDemo {
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
				"  soapsigner -- SecureBlackbox SOAPSigner Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  soapsigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password]\n" +
				"             [-signbody] [-hashalg hash_algorithm] [-sigtype signature_type]\n\n" +
				"DESCRIPTION\n" +
				"  SOAPSigner demonstrates the usage of SOAPSigner from SecureBlackbox.\n" +
				"  Used to create SOAP or WSS signatures.\n\n" +
				"  The options are as follows:\n\n" +
				"  -input        An input file to sign (Required).\n\n" +
				"  -output       Where the signed file will be saved (Required).\n\n" +
				"  -cert         The certificate used to sign files (Required).\n\n" +
				"  -certpass     The password for the signing certificate.\n\n" +
				"  -sigtype      The type of signature to use. Enter the corresponding number. Valid values:\n\n" +
				"                  1 - SST_WSSSIGNATURE\n" +
				"                  2 - SST_SOAPSIGNATURE\n\n" +
				"  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224, MD5\n\n" +
				"  -signbody     Whether to sign body.\n\n" +
				"EXAMPLES\n" +
				"  soapsigner -input C:\\soap\\helloworld.txt -output C:\\soap\\mysoap.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n" +
				"  soapsigner -input C:\\soap\\helloworld.txt -output C:\\soap\\mysoap.scs -cert C:\\certs\\mycert.pfx -certpass mypassword \n" +
				"             -sigtype 2 -hashalg SHA256 -signbody\n"
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

		SOAPSigner signer = new SOAPSigner();
		CertificateManager cm = new CertificateManager();

		try {
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
			if (optext(args, "-hashalg")) {
				signer.getNewSignature().setHashAlgorithm(optval(args, "-hashalg"));
			}

			if (optext(args, "-sigtype")) {
				signer.getNewSignature().setSignatureType(Integer.parseInt(optval(args, "-sigtype")));
			} else {
				signer.getNewSignature().setSignatureType(SOAPSignature.sstWSSSignature);
			}

			if (optext(args, "-signbody")) {
				signer.addBodyReference("", true);
			}

			signer.sign();

			System.out.println("SOAP message successfully signed.\n");

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



