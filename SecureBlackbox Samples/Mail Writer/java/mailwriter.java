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
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;

import secureblackbox.*;

public class mailwriter extends ConsoleDemo {
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
                "  mailwriter -- SecureBlackbox MailWriter Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  mailwriter <from> <sender> <to> <cc> <bcc> <subject> <priority> <output>\n" +
                "            [-plain plain_file] [-html html_file] [-scert certificate_file] [-scertpass certificate_password]\n" +
                "            [-ecert certificate_file] [-ecertpass certificate_password] [-hashalg hashalg] [-encalg encalg]\n" +
                "            [-format signature_format] [-a attach_file]\n\n" +
                "DESCRIPTION\n" +
                "  This sample shows how to compose an e-mail message. Also, it's possible to sign and/or encrypt the message with certificates.\n\n" +
                "  The options are as follows:\n\n" +
                "  from          The sender mail address (Required).\n\n" +
                "  sender        The sender name (Required).\n\n" +
                "  to            The recipient mail (Required).\n\n" +
                "  cc            The carbon copy mail address (Required).\n\n" +
                "  bcc           The blind carbon copy mail address (Required).\n\n" +
                "  subject       The letter subject (Required).\n\n" +
                "  priority      The priority of letter. Enter the corresponding number from 0 (the lowest) to 4 (the highest) (Required).\n\n" +
                "  output        The output file (Required).\n\n" +
                "  -plain        The file with plain text message.\n\n" +
                "  -html         The file with html text message.\n\n" +
                "  -scert        The certificate used to sign files.\n\n" +
                "  -scertpass    The password for the signing certificate.\n\n" +
                "  -ecert        The certificate used to encrypt files.\n\n" +
                "  -ecertpass    The password for the encryption certificate.\n\n" +
                "  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224, MD5\n\n" +
                "  -encalg       The encryption algorithm. Enter the corresponding string. Valid values: DES, 3DES, AES128, AES192, AES256, Blowfish, Twofish, Camellia, Serpent\n\n" +
                "  -format       The signature format. Enter the corresponding number. Valid values:\n\n" +
                "                  0  - MS_MULTIPART_SIGNED\n" +
                "                  1  - MS_SIGNED_DATA\n\n" +
                "  -a            The attach file.\n\n" +
                "EXAMPLES\n" +
                "  mailwriter Sbb@mail.com SbbTeam user@mail.com \"alluser@mail.com allpeople@mail.com\" ghost@mail.com \"test example\" 2 mymail.eml\n\n" +
                "  mailwriter Sbb@mail.com SbbTeam user@mail.com \"\" \"\" \"test example\" 1 mymail.eml\n" +
                "          -plain C:\\test.txt -ecert C:\\certs\\mycert.pfx -ecertpass mypassword -hashalg SHA256 -format 0\n"
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
        if (args.length < 8) {
            displayHelp("Required parameters are not specified.");
            return;
        }

        MailMessage message = new MailMessage();
        MailWriter writer = new MailWriter();
        CertificateManager cm = new CertificateManager();

        try {
            message.setFrom(args[0]);
            message.setSender(args[1]);
            message.setSendTo(args[2]);
            message.setCc(args[3]);
            message.setBcc(args[4]);
            message.setSubject(args[5]);
            message.setPriority(Integer.parseInt(args[6]));

            if (optext(args, "-plain")) {
                message.setPlainText(new String(Files.readAllBytes(Paths.get(optval(args, "-plain"))), Charset.defaultCharset()));
            }

            if (optext(args, "-html")) {
                message.setHtmlText(new String(Files.readAllBytes(Paths.get(optval(args, "-html"))), Charset.defaultCharset()));
            }

            writer.setMessage(message);

            // Additional options
            writer.getSecuritySettings().setSignBeforeEncrypt(false);
            writer.getSecuritySettings().setSignMessageHeader(false);

            if (optext(args, "-scert")) {
                cm.importFromFile(optval(args, "-scert"), optval(args, "-scertpass"));
                writer.setSigningCertificate(cm.getCertificate());
                writer.getSecuritySettings().setSign(true);
                writer.getSecuritySettings().setHashAlgorithm("SHA256");
            }

            if (optext(args, "-ecert")) {
                cm.importFromFile(optval(args, "-ecert"), optval(args, "-ecertpass"));
                writer.getEncryptionCertificates().add(cm.getCertificate());
                writer.getSecuritySettings().setEncrypt(true);
                writer.getSecuritySettings().setEncryptionAlgorithm("AES128");
            }

            if (optext(args, "-hashalg")) {
                writer.getSecuritySettings().setHashAlgorithm(optval(args, "-hashalg"));
            }

            if (optext(args, "-encalg")) {
                writer.getSecuritySettings().setEncryptionAlgorithm(optval(args, "-encalg"));
            }

            if (optext(args, "-format")) {
                writer.getSecuritySettings().setSignatureFormat(Integer.parseInt(optval(args, "-format")));
            }

            if (optext(args, "-a")) {
                writer.attachFile(optval(args, "-a"));
            }

            // Save to file
            writer.saveToFile(args[7]);

            System.out.println("A message has been assembled and saved successfully.\n");

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



