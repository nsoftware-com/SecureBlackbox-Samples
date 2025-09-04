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

public class xadessigner extends ConsoleDemo {
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
				"  xadessigner -- SecureBlackbox XAdESSigner Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  xadessigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password] \n" +
				"              [-version version] [-hashalg hash_algorithm] [-canonmethod canon_method] [-tsserver timestamp_server] \n" +
				"              [-level level] [-includekey] [-detached] \n\n" +
				"DESCRIPTION\n" +
				"  XAdESSigner demonstrates the usage of XAdESSigner from SecureBlackbox.\n" +
				"  Used to create an XML Extended Signature (XAdES) from an XML file.\n\n" +
				"  The options are as follows:\n\n" +
				"  -input        An input file to sign (Required).\n\n" +
				"  -output       Where the XAdES will be saved (Required).\n\n" +
				"  -cert         The certificate used to sign files (Required).\n\n" +
				"  -certpass     The password for the signing certificate.\n\n" +
				"  -version      The XAdES version to use. Enter the corresponding number. Valid values:\n" +
				"                  1 - XAV_111\n" +
				"                  2 - XAV_122\n" +
				"                  3 - XAV_132\n" +
				"                  4 - XAV_141\n\n" +
				"  -level        The XAdES level/form to use. Enter the corresponding number. Valid values:\n" +
		                "                  0  - ASL_UNKNOWN\n" +
                		"                  1  - ASL_GENERIC\n" +
		                "                  2  - ASL_BASELINE_B\n" +
                		"                  3  - ASL_BASELINE_T\n" +
		                "                  4  - ASL_BASELINE_LT\n" +
                		"                  5  - ASL_BASELINE_LTA\n" +
		                "                  6  - ASL_BES\n" +
                		"                  7  - ASL_EPES\n" +
		                "                  8  - ASL_T\n" +
                		"                  9  - ASL_C\n" +
		                "                  10 - ASL_X\n" +
                		"                  11 - ASL_XTYPE_1\n" +
		                "                  12 - ASL_XTYPE_2\n" +
                		"                  13 - ASL_XL\n" +
		                "                  14 - ASL_XLTYPE_1\n" +
                		"                  15 - ASL_XLTYPE_2\n" +
		                "                  16 - ASL_A\n" +
                		"                  17 - ASL_EXTENDED_BES\n" +
		                "                  18 - ASL_EXTENDED_EPES\n" +
                		"                  19 - ASL_EXTENDED_T\n" +
		                "                  20 - ASL_EXTENDED_C\n" +
                		"                  21 - ASL_EXTENDED_X\n" +
                		"                  22 - ASL_EXTENDED_XTYPE_1\n" +
		                "                  23 - ASL_EXTENDED_XTYPE_2\n" +
                		"                  24 - ASL_EXTENDED_XLONG\n" +
		                "                  25 - ASL_EXTENDED_XL\n" +
                		"                  26 - ASL_EXTENDED_XLTYPE_1\n" +
		                "                  27 - ASL_EXTENDED_XLTYPE_2\n" +
                		"                  28 - ASL_EXTENDED_A\n\n" +
				"  -detached     Whether the signature is detached.\n\n" +
				"  -canonmethod  The canonicalization method to use. Enter the corresponding number. Valid values:\n" +
				"                  0 - CXCM_NONE\n" +
				"                  1 - CXCM_CANON\n" +
				"                  2 - CXCM_CANON_COMMENT\n" +
				"                  3 - CXCM_EXCL_CANON\n" +
				"                  4 - CXCM_EXCL_CANON_COMMENT\n" +
				"                  5 - CXCM_MIN_CANON\n" +
				"                  6 - CXCM_CANON_V_1_1\n" +
				"                  7 - CXCM_CANON_COMMENT_V_1_1\n\n" +
				"  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224\n\n" +
				"  -tsserver     A timestamp server to use during signing.\n\n" +
				"  -includekey   Whether to include the public key in the signature.\n\n" +
				"EXAMPLES\n" +
				"  xadessigner -input C:\\xades\\myfile.xml -output C:\\xades\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword\n" +
				"  xadessigner -input C:\\xades\\myfile.xml -output C:\\xades\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword\n" +
				"              -version 3 -level 3 -canonmethod 1 -hashalg SHA256 -detached -tsserver http://timestamp.wosign.com\n"
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

		XAdESSigner signer = new XAdESSigner();
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

			signer.setTimestampServer(optval(args, "-tsserver"));
			if (optext(args, "-detached")) {
				signer.setDataFile(input);
				signer.setDataType(XAdESSigner.cxdtBinary);
				signer.setDataURI("filename.txt"); // use real name of the input
				signer.getNewSignature().setSignatureType(XAdESSignature.cxstDetached);
			} else {
				signer.setInputFile(input);
				signer.getNewSignature().setSignatureType(XAdESSignature.cxstEnveloped);
			}
			signer.getNewSignature().setXAdESVersion(Integer.parseInt(optval(args, "-version")));
			signer.getNewSignature().setLevel(Integer.parseInt(optval(args, "-level")));
			signer.getNewSignature().setCanonicalizationMethod(Integer.parseInt(optval(args, "-canonmethod")));
			signer.getNewSignature().setHashAlgorithm(optext(args, "-hashalg") ? optval(args, "-hashalg") : "SHA256");
			if (optext(args, "-includekey")) {
				signer.config("IncludeKey=true");
			}

			signer.sign();

			System.out.println("The file was signed successfully.\n");

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



