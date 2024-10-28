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

public class asicsigner extends ConsoleDemo {
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
                "  asicsigner -- SecureBlackbox ASiCSigner Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  asicsigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password]\n" +
                "             [-extended] [-level level] [-sigtype signature_type] [-tsserver timestamp_server]\n\n" +
                "DESCRIPTION\n" +
                "  ASiCSigner demonstrates the usage of ASiCSigner from SecureBlackbox.\n" +
                "  Used to create an Associated Signature Container (ASic) from one or more files.\n\n" +
                "  The options are as follows:\n\n" +
                "  -input        An input file to sign (Required).\n\n" +
                "  -output       Where the ASiC will be saved (Required).\n\n" +
                "  -cert         The certificate used to sign files (Required).\n\n" +
                "  -certpass     The password for the signing certificate.\n\n" +
                "  -sigtype      The type of signature to use. Enter the corresponding number. Valid values:\n\n" +
                "                  0 - CAST_UNKNOWN\n" +
                "                  1 - CAST_CADES\n" +
                "                  2 - CAST_XADES\n\n" +
                "  -level        The level for CAdES signatures. Enter the corresponding number. Valid values:\n\n" +
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
                "  -extended     Whether to use extended signatures.\n\n" +
                "  -tsserver     A timestamp server to use during signing.\n\n" +
                "EXAMPLES\n" +
                "  asicsigner -input C:\\asic\\helloworld.txt -output C:\\asic\\myasic.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n" +
                "  asicsigner -input C:\\asic\\helloworld.txt -output C:\\asic\\myasic.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n" +
                "             -sigtype 2 -level 10 -extended -tsserver http://timestamp.wosign.com\n"
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

        ASiCSigner signer = new ASiCSigner();
        CertificateManager cm = new CertificateManager();

        try {
            if (optext(args, "-input")) {
                signer.setSourceFiles(optval(args, "-input"));
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
            if (optext(args, "-extended")) {
                signer.setExtended(true);
            }
            if (optext(args, "-level")) {
                signer.getNewSignature().setLevel(Integer.parseInt(optval(args, "-level")));
            }
            if (optext(args, "-sigtype")) {
                signer.getNewSignature().setSignatureType(Integer.parseInt(optval(args, "-sigtype")));
            } else {
                signer.getNewSignature().setSignatureType(ASiCSignature.castCAdES);
            }

            // Sign
            signer.sign();

            System.out.println("ASiC created.\n");

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



