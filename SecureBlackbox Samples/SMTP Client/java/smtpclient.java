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

public class smtpclient extends ConsoleDemo {
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
                "  smtpclient -- SecureBlackbox SMTPClient Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  smtpclient <host> <port> <from> <sender> <to> <receiver> <subject> <priority> [-username username]\n" +
                "            [-password password] [-tlsmode tlsmode] [-plain plain_file] [-html html_file]\n\n" +
                "DESCRIPTION\n" +
                "  This sample shows how to compose an e-mail message. Also, it's possible to sign and/or encrypt the message with certificates.\n\n" +
                "  The options are as follows:\n\n" +
                "  host        The name or address of a mail server (Required).\n\n" +
                "  port        The port of a mail server (Required).\n\n" +
                "  from        The sender mail address (Required).\n\n" +
                "  sender      The sender name (Required).\n\n" +
                "  to          The receiver mail address (Required).\n\n" +
                "  receiver    The receiver name (Required).\n\n" +
                "  subject     The letter subject (Required).\n\n" +
                "  priority    The priority of letter. Enter the corresponding number from 0 (the lowest) to 4 (the highest) (Required).\n\n" +
                "  -username   The user identifier for the mailbox.\n\n" +
                "  -password   The password for the mailbox user.\n\n" +
                "  -tlsmode    The TLS mode. Enter the corresponding string. Valid values: none, explicit, implicit.\n\n" +
                "  -plain      The file with plain text message.\n\n" +
                "  -html       The file with html text message.\n\n" +
                "EXAMPLES\n" +
                "  smtpclient mail.local 995 Sbb@mail.com SbbTeam user@mail.com User \"test example\" 2 -plain C:\\test.txt\n\n" +
                "  smtpclient mail.local 12345 Sbb@mail.com SbbTeam user@mail.com User \"test example\" 3 -username testuser -password pass -tlsmode implicit\n"
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

        MailWriter writer = new MailWriter();
        SMTPClient client = new SMTPClient();

        try {
            writer.getFrom().add(new MailAddress(args[3], args[2]));
            writer.getSendTo().add(new MailAddress(args[5], args[4]));

            writer.getMessage().setSubject(args[6]);

            writer.getMessage().setPriority(Integer.parseInt(args[7]));

            if (optext(args, "-plain")) {
                writer.getMessage().setPlainText(new String(Files.readAllBytes(Paths.get(optval(args, "-plain"))), Charset.defaultCharset()));
            }

            if (optext(args, "-html")) {
                writer.getMessage().setHtmlText(new String(Files.readAllBytes(Paths.get(optval(args, "-html"))), Charset.defaultCharset()));
            }

            if (optext(args, "-username")) {
                client.setUsername(optval(args, "-username"));
            }

            if (optext(args, "-password")) {
                client.setPassword(optval(args, "-password"));
            }

            if (optext(args, "-tlsmode")) {
                String tlsmode = optval(args, "-tlsmode");
                if (tlsmode.equalsIgnoreCase("none")) {
                    client.getTLSSettings().setTLSMode(TLSSettings.smNoTLS);
                } else if (tlsmode.equalsIgnoreCase("explicit")) {
                    client.getTLSSettings().setTLSMode(TLSSettings.smExplicitTLS);
                } else if (tlsmode.equalsIgnoreCase("implicit")) {
                    client.getTLSSettings().setTLSMode(TLSSettings.smImplicitTLS);
                }
            }

            client.connect(args[0], Integer.parseInt(args[1]));

            client.sendBytes(writer.saveToBytes());

            System.out.println("Message has been sent successfully.\n");

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



