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
import java.util.ArrayList;
import java.util.StringTokenizer;

import secureblackbox.*;

import static secureblackbox.MailMessage.*;

public class popclient extends ConsoleDemo {
    public static String[] translateCommandline(String toProcess) {
        if (toProcess == null || toProcess.length() == 0) {
            return new String[0];
        }

        final int normal = 0;
        final int inQuote = 1;
        final int inDoubleQuote = 2;
        int state = normal;
        final StringTokenizer tok = new StringTokenizer(toProcess, "\"\' ", true);
        final ArrayList<String> result = new ArrayList<String>();
        final StringBuilder current = new StringBuilder();
        boolean lastTokenHasBeenQuoted = false;

        while (tok.hasMoreTokens()) {
            String nextTok = tok.nextToken();
            switch (state) {
                case inQuote:
                    if ("\'".equals(nextTok)) {
                        lastTokenHasBeenQuoted = true;
                        state = normal;
                    } else {
                        current.append(nextTok);
                    }
                    break;
                case inDoubleQuote:
                    if ("\"".equals(nextTok)) {
                        lastTokenHasBeenQuoted = true;
                        state = normal;
                    } else {
                        current.append(nextTok);
                    }
                    break;
                default:
                    if ("\'".equals(nextTok)) {
                        state = inQuote;
                    } else if ("\"".equals(nextTok)) {
                        state = inDoubleQuote;
                    } else if (" ".equals(nextTok)) {
                        if (lastTokenHasBeenQuoted || current.length() != 0) {
                            result.add(current.toString());
                            current.setLength(0);
                        }
                    } else {
                        current.append(nextTok);
                    }
                    lastTokenHasBeenQuoted = false;
                    break;
            }
        }
        if (lastTokenHasBeenQuoted || current.length() != 0) {
            result.add(current.toString());
        }
        if (state == inQuote || state == inDoubleQuote) {
            throw new RuntimeException("unbalanced quotes in " + toProcess);
        }
        return result.toArray(new String[result.size()]);
    }

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
                "  popclient -- SecureBlackbox POPClient Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  popclient <host> <port> [-username username] [-password password] [-tlsmode tlsmode]\n\n" +
                "DESCRIPTION\n" +
                "  This sample how to use POP component to view messages in the mailbox.\n\n" +
                "  The options are as follows:\n\n" +
                "  host        The name or address of a mail server (Required).\n\n" +
                "  port        The port of a mail server (Required).\n\n" +
                "  -username   The user identifier for the mailbox.\n\n" +
                "  -password   The password for the mailbox user.\n\n" +
                "  -tlsmode    The TLS mode. Enter the corresponding string. Valid values: none, explicit, implicit.\n\n" +
                "EXAMPLES\n" +
                "  popclient mail.local 995\n\n" +
                "  popclient mail.local 12345 -username testuser -password pass -tlsmode implicit\n"
        );

        if (errMes.length() > 0) {
            System.out.println("Error: " + errMes);
            System.out.println();
        }

        confirmExit();
    }

    private static void displayCommands() {
        System.out.println("Mail Commands\n" +
                "l                               listing messages in mbox\n" +
                "h <message number>              print out message headers\n" +
                "f <message number>              print out full message info\n" +
                "d <message number>              delete message\n" +
                "q                               quit, saving unresolved messages in mbox");
    }

    private static void confirmExit() {
        System.out.println("Press Enter to exit the demo.");
        input();
    }

    public static void main(String[] args) {
        if (args.length < 2) {
            displayHelp("host and port is required.");
            return;
        }

        POP3Client client = new POP3Client();
        try {
            client.addPOP3ClientEventListener(new POP3ClientEventListener() {
                @Override
                public void beforeAuth(POP3ClientBeforeAuthEvent e) {

                }

                @Override
                public void command(POP3ClientCommandEvent e) {

                }

                @Override
                public void commandReply(POP3ClientCommandReplyEvent e) {

                }

                @Override
                public void commandReplyData(POP3ClientCommandReplyDataEvent e) {

                }

                @Override
                public void error(POP3ClientErrorEvent e) {

                }

                @Override
                public void externalSign(POP3ClientExternalSignEvent e) {

                }

                @Override
                public void notification(POP3ClientNotificationEvent e) {

                }

                @Override
                public void progress(POP3ClientProgressEvent e) {

                }

                @Override
                public void TLSCertNeeded(POP3ClientTLSCertNeededEvent e) {

                }

                @Override
                public void TLSCertValidate(POP3ClientTLSCertValidateEvent e) {
                    e.accept = true;
                }

                @Override
                public void TLSEstablished(POP3ClientTLSEstablishedEvent e) {

                }

                @Override
                public void TLSHandshake(POP3ClientTLSHandshakeEvent e) {

                }

                @Override
                public void TLSPSK(POP3ClientTLSPSKEvent e) {

                }

                @Override
                public void TLSShutdown(POP3ClientTLSShutdownEvent e) {

                }

                @Override
                public void supercoreIntercept(POP3ClientSupercoreInterceptEvent e) {

                }
            });

            client.config("RequestUIDs=True");
            client.getTLSSettings().setAutoValidateCertificates(false);
            client.getTLSSettings().setTLSMode(TLSSettings.smNoTLS);

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
                }
                else if (tlsmode.equalsIgnoreCase("explicit")) {
                    client.getTLSSettings().setTLSMode(TLSSettings.smExplicitTLS);
                }
                else if (tlsmode.equalsIgnoreCase("implicit")) {
                    client.getTLSSettings().setTLSMode(TLSSettings.smImplicitTLS);
                }
            }

            client.connect(args[0], Integer.parseInt(args[1]));

            System.out.println("Connected successfully!");

            // main loop to check for commands
            while (true) {
                String[] commands = translateCommandline(prompt("pop", ">"));

                if (commands.length > 0) {
                    if (commands[0].equalsIgnoreCase("l")) {
                        client.listMessages();

                        if (client.getMessages().size() > 0) {
                            for (int i = 0; i < client.getMessages().size(); i++) {
                                System.out.println("Uid: " + client.getMessages().item(i).getUID());
                                System.out.println("Size: " + client.getMessages().item(i).getSize());
                            }
                        }
                        else {
                            System.out.println("No messages on the server.");
                        }
                    } else if (commands[0].equalsIgnoreCase("d")) {
                        if (commands.length < 2) {
                            System.out.println("You need to pass <message number> parameter");
                        } else {
                            client.deleteMessage(Integer.parseInt(commands[1]));
                            System.out.println("Message deleted");
                        }
                    } else if (commands[0].equalsIgnoreCase("h")|| commands[0].equalsIgnoreCase("f")) {
                        if (commands.length < 2) {
                            System.out.println("You need to pass <message number> parameter");
                        } else {
                            client.receiveMessage(Integer.parseInt(commands[1]));

                            System.out.println("Message info:");
                            System.out.println("From: " + client.getMessage().getFrom());
                            System.out.println("To: " + client.getMessage().getSendTo());
                            System.out.println("Date: " + client.getMessage().getDate());
                            System.out.println("Subject: " + client.getMessage().getSubject());

                            switch (client.getMessage().getPriority()) {
                                case mpLowest:
                                    System.out.println("Priority: [lowest]");
                                    break;
                                case mpLow:
                                    System.out.println("Priority: [low]");
                                    break;
                                case mpNormal:
                                    System.out.println("Priority: [normal]");
                                    break;
                                case mpHigh:
                                    System.out.println("Priority: [HIGH]");
                                    break;
                                case mpHighest:
                                    System.out.println("Priority: [HIGHEST]");
                                    break;
                            }
                            if (commands[0].equalsIgnoreCase("f")) {
                                System.out.println("Plain text: " + client.getMessage().getPlainText() + "\n");
                                System.out.println("Html text: " + client.getMessage().getHtmlText() + "\n");
                            }
                        }
                    } else if (commands[0].equalsIgnoreCase("bye") || commands[0].equalsIgnoreCase("q") || commands[0].equalsIgnoreCase("quit")) {
                        if (ask("Save changes to inbox") == 'n') {
                            client.undelete();
                        }
                        client.disconnect();
                        break;
                    } else if (commands[0].equalsIgnoreCase("?") || commands[0].equalsIgnoreCase("help") || commands[0].equalsIgnoreCase("man")) {
                        displayCommands();
                    } else {
                        System.out.println("Command not recognized. Choose from these:\n");
                        displayCommands();
                    }
                } else {
                    System.out.println("Command not recognized. Choose from these:\n");
                    displayCommands();
                }
            }

            System.out.println();
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



