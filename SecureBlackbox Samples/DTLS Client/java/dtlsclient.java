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

public class dtlsclient extends ConsoleDemo {
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
            "  dtlsclient -- SecureBlackbox DTLSClient Demo Application\n\n" +
            "SYNOPSIS\n" +
            "  dtlsclient <host> <port>\n\n" +
            "DESCRIPTION\n" +
            "  This sample is a very simple program that shows how to use the DTLSClient component\n" +
            "  to send and receive messages to and from a DTLS server.\n\n" +
            "  The options are as follows:\n\n" +
            "  host       The local host of serve (Required).\n\n" +
            "  port       The port of server (Required).\n\n" +
            "EXAMPLES\n" +
            "  dtlsclient localhost 4433\n"
    );

    if (errMes.length() > 0) {
      System.out.println("Error: " + errMes);
      System.out.println();
    }

    confirmExit();
  }

  private static void displayCommands() {
    System.out.println("  send <message>        send a letter to the server and receive a response");
    System.out.println("  q                     quit");
  }

  private static void confirmExit() {
    System.out.println("Press Enter to exit the demo.");
    input();
  }

  private static DTLSClient client;

  public static void main(String[] args) {
    if (args.length < 2) {
      displayHelp("host and port is required.");
      return;
    }

    client = new DTLSClient();
    try {
      client.addDTLSClientEventListener(new DTLSClientEventListener() {
        @Override
        public void error(DTLSClientErrorEvent e) {

        }

        @Override
        public void externalSign(DTLSClientExternalSignEvent e) {

        }

        @Override
        public void notification(DTLSClientNotificationEvent e) {

        }

        @Override
        public void TLSCertNeeded(DTLSClientTLSCertNeededEvent e) {

        }

        @Override
        public void TLSCertValidate(DTLSClientTLSCertValidateEvent e) {
          if (client.getTLSServerChain().size() == 0) return;
          // do not do this in production code
          e.accept = true;
          System.out.println("Server certificate recived: " + client.getTLSServerChain().item(client.getTLSServerChain().size() - 1).getIssuer());
        }

        @Override
        public void TLSEstablished(DTLSClientTLSEstablishedEvent e) {

        }

        @Override
        public void TLSHandshake(DTLSClientTLSHandshakeEvent e) {

        }

        @Override
        public void TLSPSK(DTLSClientTLSPSKEvent e) {

        }

        @Override
        public void TLSShutdown(DTLSClientTLSShutdownEvent e) {

        }

        @Override
        public void supercoreIntercept(DTLSClientSupercoreInterceptEvent e) {

        }
      });

      client.getTLSSettings().setAutoValidateCertificates(false);

      client.connect(args[0], Integer.parseInt(args[1]));

      System.out.println("Connected successfully!");

      // main loop to check for commands
      while (true) {
        String[] commands = translateCommandline(prompt("dtls", ">"));

        if (commands.length > 0) {
          if (commands[0].equalsIgnoreCase("send")) {
            if (commands.length < 2) {
              System.out.println("You need to pass <message> parameter");
            } else {
              client.sendText(commands[1]);
              System.out.println("C->S: " + commands[1]);
              int maxPartSize = 1000;
              System.out.print("S->C: ");
              client.receiveData(maxPartSize);
              while (client.getOutputString().length() == 0) {
                client.receiveData(maxPartSize);
              }
              System.out.println(client.getOutputString());
            }
          } else if (commands[0].equalsIgnoreCase("bye") || commands[0].equalsIgnoreCase("q") || commands[0].equalsIgnoreCase("quit")) {
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



