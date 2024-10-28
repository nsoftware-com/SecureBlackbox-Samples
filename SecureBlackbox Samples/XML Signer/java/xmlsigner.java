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

public class xmlsigner extends ConsoleDemo {
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
				"  xmlsigner -- SecureBlackbox XMLSigner Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  xmlsigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password] \n" +
				"           [-detached] [-canonmethod canon_method] [-hashalg hash_algorithm] [-includekey] \n\n" +
				"DESCRIPTION\n" +
				"  XMLSigner demonstrates the usage of XMLSigner from SecureBlackbox.\n" +
				"  Used to create an XML Signature from an XML file.\n\n" +
				"  The options are as follows:\n\n" +
				"  -input        An input file to sign (Required).\n\n" +
				"  -output       Where the XML signature will be saved (Required).\n\n" +
				"  -cert         The certificate used to sign files (Required).\n\n" +
				"  -certpass     The password for the signing certificate.\n\n" +
				"  -detached     Whether the signature is detached.\n\n" +
				"  -canonmethod  The canonicalization method to use. Enter the corresponding number. Valid values:\n\n" +
				"                  0 - CXCM_NONE\n" +
				"                  1 - CXCM_CANON\n" +
				"                  2 - CXCM_CANON_COMMENT\n" +
				"                  3 - CXCM_EXCL_CANON\n" +
				"                  4 - CXCM_EXCL_CANON_COMMENT\n" +
				"                  5 - CXCM_MIN_CANON\n" +
				"                  6 - CXCM_CANON_V_1_1\n" +
				"                  7 - CXCM_CANON_COMMENT_V_1_1\n\n" +
				"  -hashalg      The hashing algorithm to use. Valid values:\n\n" +
				"                  SHA1\n" +
				"                  MD5\n" +
				"                  SHA256\n" +
				"                  SHA384\n" +
				"                  SHA512\n" +
				"                  RIPEMD160\n\n" +
				"  -includekey   Whether to include the public key in the signature.\n\n" +
				"EXAMPLES\n" +
				"  xmlsigner -input C:\\xml\\myfile.xml -output C:\\xml\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword\n" +
				"  xmlsigner -input C:\\xml\\myfile.xml -output C:\\xml\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword\n" +
				"           -canonmethod 3 -hashalg SHA1 -detached\n"
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

		XMLSigner signer = new XMLSigner();
		CertificateManager cm = new CertificateManager();
		String input;

		try {
			if (optext(args, "-input")) {
				input = optval(args, "-input");
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
			if (optext(args, "-detached")) {
				signer.setDataFile(input);
				signer.setDataType(XMLSigner.cxdtBinary);
				signer.setDataURI("filename.txt"); // use real name of the input
				signer.setSignatureType(XMLSigner.cxstDetached);
			} else {
				signer.setInputFile(input);
				signer.setSignatureType(XMLSigner.cxstEnveloped);
			}

			signer.setCanonicalizationMethod(Integer.parseInt(optval(args, "-canonmethod")));

			if (optext(args, "-hashalg")) {
				signer.setHashAlgorithm(optval(args, "-hashalg"));
			} else {
				signer.setHashAlgorithm("SHA256");
			}

			if (optext(args, "-includekey")) {
				signer.config("IncludeKey=true");
			}

			// Sign
			signer.sign();

			System.out.println("XML file successfully signed.\n");

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



