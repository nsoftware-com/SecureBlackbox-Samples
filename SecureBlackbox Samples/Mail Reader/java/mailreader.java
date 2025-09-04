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

import static secureblackbox.MailMessage.*;
import static secureblackbox.MailSecurityInfo.*;

public class mailreader extends ConsoleDemo {
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
                "  mailreader -- SecureBlackbox MailReader Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  mailreader <-input input_file> [-cert certificate_file] [-certpass certificate_password]\n\n" +
                "DESCRIPTION\n" +
                "  This sample shows how to parse an e-mail message, including signed  and/or encrypted messages.\n\n" +
                "  The options are as follows:\n\n" +
                "  -input        An input file to verify (Required).\n\n" +
                "  -cert         The certificate used to decryption.\n\n" +
                "  -certpass     The password for the certificate.\n\n" +
                "EXAMPLES\n" +
                "  mailreader -input C:\\mymail.eml -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
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

    private static void showMailInfo(MailReader reader) {
        System.out.println();
        System.out.println("Sender = " + reader.getMessage().getSender());
        System.out.println("From = " + reader.getMessage().getFrom());
        System.out.println("SendTo = " + reader.getMessage().getSendTo());
        System.out.println("CC = " + reader.getMessage().getCc());
        System.out.println("BCC = " + reader.getMessage().getBcc());
        System.out.println("\n\t\t***Security Info***");
        System.out.println("Encrypted = " + (reader.getSecurityInfo().getEncrypted() ? "true" : "false"));
        if (reader.getSecurityInfo().getEncrypted()) {
            System.out.print("Decryption Certificate = ");
            if (reader.getDecryptionCertificate() == null) {
                System.out.println("[certificate not provided]");
            }
            else {
                System.out.println(reader.getDecryptionCertificate().getSubjectRDN());
            }
            System.out.println("Encryption algorithm = " + reader.getSecurityInfo().getEncryptionAlgorithm());
        }
        System.out.println("Signed = " + (reader.getSecurityInfo().getSigned() ? "true" : "false"));
        if (reader.getSecurityInfo().getSigned()) {
            System.out.print("Signing Certificate = ");
            if ( reader.getSigningCertificate() == null) {
                System.out.println("[certificate not found]");
            }
            else {
                System.out.println(reader.getSigningCertificate().getSubjectRDN());
            }
            System.out.print("Signature validation = ");
            switch (reader.getSecurityInfo().getSignatureValidationResult()) {
                case svtValid:
                    System.out.println("VALID");
                    break;

                case svtCorrupted:
                    System.out.println("CORRUPTED");
                    break;

                case svtSignerNotFound:
                    System.out.println("SIGNER NOT FOUND");
                    break;

                case svtFailure:
                    System.out.println("FAILURE");
                    break;

                default:
                    System.out.println("UNKNOWN");
                    break;
            }

            System.out.println("Hash algorithm = " + reader.getSecurityInfo().getHashAlgorithm());
        }
        System.out.println("\t***End Security Info Block***\n");

        System.out.println("Subject = " + reader.getMessage().getSubject());
        System.out.print("Priority = ");
        switch (reader.getMessage().getPriority()) {
            case mpLowest:
                System.out.println("LOWEST");
                break;

            case mpLow:
                System.out.println("LOW");
                break;

            case mpNormal:
                System.out.println("NORMAL");
                break;

            case mpHigh:
                System.out.println("HIGH");
                break;

            case mpHighest:
                System.out.println("HIGHEST");
                break;

            default:
                System.out.println("UNKNOWN");
                break;
        }
        System.out.println("Delivery receipt = " + (reader.getMessage().getDeliveryReceipt() ? "true" : "false"));
        System.out.println("Read receipt = " + (reader.getMessage().getReadReceipt() ? "true" : "false"));
        System.out.println("\n\t\t***Plain Text***");
        System.out.println(reader.getMessage().getPlainText());
        System.out.println("\t\t***Html Text***");
        System.out.println(reader.getMessage().getHtmlText());
        System.out.println("\nAttachments:");
        for (int i = 0; i < reader.getAttachments().size(); i++) {
            System.out.println(reader.getAttachments().item(i).getFileName() + "\t\tSize = " + reader.getAttachments().item(i).getSize());
        }
        System.out.println("\n**************************END**************************\n");
    }

    public static void main(String[] args) {
        if (args.length == 0) {
            displayHelp("");
            return;
        }

        MailReader reader = new MailReader();
        CertificateManager cm = new CertificateManager();
        final String input;

        try {
            reader.addMailReaderEventListener(new MailReaderEventListener() {
                @Override
                public void chainValidated(MailReaderChainValidatedEvent e) {

                }

                @Override
                public void decryptionInfoNeeded(MailReaderDecryptionInfoNeededEvent e) {
                    System.out.println("************************************************************************");
                    System.out.println("Decryption needed! Please try again with correct path to certificate - the 2nd argument");
                    System.out.println("************************************************************************");
                }

                @Override
                public void error(MailReaderErrorEvent e) {

                }

                @Override
                public void externalDecrypt(MailReaderExternalDecryptEvent e) {

                }

                @Override
                public void notification(MailReaderNotificationEvent e) {

                }

                @Override
                public void signatureFound(MailReaderSignatureFoundEvent e) {

                }

                @Override
                public void signatureValidated(MailReaderSignatureValidatedEvent e) {

                }

                @Override
                public void supercoreIntercept(MailReaderSupercoreInterceptEvent e) {

                }
            });

            if (optext(args, "-input")) {
                input = optval(args, "-input");
            } else {
                displayHelp("-input is required.");
                return;
            }

            if (optext(args, "-cert")) {
                cm.importFromFile(optval(args, "-cert"), optval(args, "-certpass"));
                reader.setDecryptionCertificate(cm.getCertificate());
            }

            reader.loadFromFile(input);

            showMailInfo(reader);

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



