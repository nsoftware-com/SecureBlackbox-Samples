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
import static secureblackbox.KMIPClient.*;
import static secureblackbox.KMIPObject.*;

public class kmipclient extends ConsoleDemo {
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
            "  kmipclient -- SecureBlackbox KMIPClient Demo Application\n\n" +
            "SYNOPSIS\n" +
            "  kmipclient <url> [-username username] [-password password] [-encoder encoder_type]\n\n" +
            "DESCRIPTION\n" +
            "  This sample illustrates basic FTP client operations.\n\n" +
            "  The options are as follows:\n\n" +
            "  url         The url of server (Required).\n\n" +
            "  -username   The user identifier to use for login.\n\n" +
            "  -password   The password to log in.\n\n" +
            "  -encoder    The encoder type. Enter the corresponding string. Valid values: ttlv, xml, json.\n\n" +
            "EXAMPLES\n" +
            "  kmipclient kmip://127.0.0.1:5696\n\n" +
            "  kmipclient https://127.0.0.1:5696 -username testuser -password pass -encoder xml\n"
    );

    if (errMes.length() > 0) {
      System.out.println("Error: " + errMes);
      System.out.println();
    }

    confirmExit();
  }

  private static void displayCommands() {
    System.out.println("KMIP Commands:\n" +
            "   ls                                   list objects\n" +
            "   nc <group>                           create new certificate\n" +
            "   nk <group>                           create new key\n" +
            "   ac <cert file> <cert pass> <group>   add certificate from file\n" +
            "   ak <key file> <key pass> <group>     add key from file\n" +
            "   del <object id>                      delete object\n" +
            "   e <object id> <input> <output> <IV>  encrypt input file\n" +
            "   d <object id> <input> <output> <IV>  decrypt input file\n" +
            "   s <object id> <input> <output>       sign input file\n" +
            "   v <object id> <data> <signature>     verify signature file\n" +
            "   exit, quit, q, bye                   exit\n" +
            "   ?, help, man                         help");
  }

  private static void confirmExit() {
    System.out.println("Press Enter to exit the demo.");
    input();
  }

  static Boolean verbose = false;

  public static void main(String[] args) {
    if (args.length < 1) {
      displayHelp("url is required.");
      return;
    }

    final KMIPClient client = new KMIPClient();

    try {
      client.addKMIPClientEventListener(new KMIPClientEventListener() {
        @Override
        public void error(KMIPClientErrorEvent e) {

        }

        @Override
        public void externalSign(KMIPClientExternalSignEvent e) {

        }

        @Override
        public void notification(KMIPClientNotificationEvent e) {

        }

        @Override
        public void request(KMIPClientRequestEvent e) {

        }

        @Override
        public void response(KMIPClientResponseEvent e) {

        }

        @Override
        public void TLSCertNeeded(KMIPClientTLSCertNeededEvent e) {

        }

        @Override
        public void TLSCertValidate(KMIPClientTLSCertValidateEvent e) {
          e.accept = true;
        }

        @Override
        public void TLSEstablished(KMIPClientTLSEstablishedEvent e) {

        }

        @Override
        public void TLSHandshake(KMIPClientTLSHandshakeEvent e) {

        }

        @Override
        public void TLSPSK(KMIPClientTLSPSKEvent e) {

        }

        @Override
        public void TLSShutdown(KMIPClientTLSShutdownEvent e) {

        }

        @Override
        public void supercoreIntercept(KMIPClientSupercoreInterceptEvent e) {

        }
      });

      client.setBaseURL(args[0]);
      client.getTLSSettings().setAutoValidateCertificates(false);

      if (optext(args, "-username")) {
        client.setUsername(optval(args, "-username"));
      } else {
        client.setUsername("anonymous");
      }

      if (optext(args, "-password")) {
        client.setPassword(optval(args, "-password"));
      }

      if (optext(args, "-encoder")) {
        String encoder = optval(args, "-encoder");
        if (encoder.equalsIgnoreCase("ttlv")) {
          client.setEncoding(KMIPClient.etTTLV);
        } else if (encoder.equalsIgnoreCase("xml")) {
          client.setEncoding(KMIPClient.etXML);
        } else if (encoder.equalsIgnoreCase("json")) {
          client.setEncoding(KMIPClient.etJSON);
        }
      } else {
        client.setEncoding(KMIPClient.etTTLV);
      }

      System.out.println("Connecting to " + args[0]);

      client.list(0, "", 0, 0, false);
    } catch (Exception ex) {
      displayError(ex);
      return;
    }

    // main loop to check for commands
    while (true) {
      try {
        String[] commands = translateCommandline(prompt("kmip", ">"));

        if (commands.length > 0) {
          if (commands[0].equalsIgnoreCase("ls")) {
            client.list(0, "", 0, 0, false);
            System.out.println("Objects(" + Integer.toString(client.getObjects().size()) + "):");
            for (int x = 0; x < client.getObjects().size(); x++) {

              System.out.println("  Object id: " + client.getObjects().item(x).getObjectId());
              switch (client.getObjects().item(x).getObjectType()) {
                case otCertificate:
                  System.out.println("  Object type: Certificate");
                  break;
                case otSymmetricKey:
                  System.out.println("  Object type: Symmetric Key");
                  break;
                case otPublicKey:
                  System.out.println("  Object type: Public Key");
                  break;
                case otPrivateKey:
                  System.out.println("  Object type: Private Key");
                  break;
                default:
                  System.out.println("  Object type: Unknown");
                  break;
              }
              System.out.println("  Algorithm: " + client.getObjects().item(x).getKeyAlgorithm());
              System.out.println("  Key bits: " + client.getObjects().item(x).getKeyBits());
              System.out.println("  Group: " + client.getObjects().item(x).getObjectGroup());
              System.out.println();
            }
          } else if (commands[0].equalsIgnoreCase("nc")) {
            if (commands.length < 2) {
              System.out.println("You need to pass <group> parameter");
            } else {
              String keyIds = client.generateKey("RSA", "", "", 1024, commands[1], true);

              String pubKeyId = keyIds.substring(keyIds.indexOf(client.config("ListDelimiter")) + 1);

              String certId = client.generate(pubKeyId, true);

              System.out.println("Generate certificate with Id: " + certId + " and keys: " + keyIds);
            }
          } else if (commands[0].equalsIgnoreCase("nk")) {
            if (commands.length < 2) {
              System.out.println("You need to pass <group> parameter");
            } else {
              String keyId = client.generateKey("AES", "", "", 256, commands[1], true);

              System.out.println("Generate key with Id: " + keyId);
            }
          } else if (commands[0].equalsIgnoreCase("ac")) {
            if (commands.length < 4) {
              System.out.println("You need to pass three parameters: <cert file>, <cert pass> and <group>");
            } else {
              CertificateManager cm = new CertificateManager();
              cm.importFromFile(commands[1], commands[2]);

              client.setCertificate(cm.getCertificate());
              String certId = client.add(true, commands[3], true);

              System.out.println("Added certificate with Id: " + certId);
            }
          } else if (commands[0].equalsIgnoreCase("ak")) {
            if (commands.length < 4) {
              System.out.println("You need to pass three parameters: <key file>, <key pass> and <group>");
            } else {
              CryptoKeyManager km = new CryptoKeyManager();
              km.importFromFile(commands[1], 1, "", "", "", 0, commands[2]);

              client.setKey(km.getKey());
              String keyId = client.addKey(commands[3], true);

              System.out.println("Added key with Id: " + keyId);
            }
          } else if (commands[0].equalsIgnoreCase("del")) {
            if (commands.length < 2) {
              System.out.println("You need to pass <object id> parameter");
            } else {
              client.remove(commands[1]);
              System.out.println("Delete object with Id: " + commands[1]);
            }
          } else if (commands[0].equalsIgnoreCase("e")) {
            if (commands.length < 5) {
              System.out.println("You need to pass four parameters: <object id>, <input>, <output> and <IV>");
            } else {
              client.setInputFile(commands[2]);
              client.setOutputFile(commands[3]);

              Utils utils = new Utils();
              byte[] IV = utils.hexDecode(commands[4]);

              client.encrypt(commands[1], "", IV, "", "", 0);

              if (client.getAuxResult().length() > 0)
                System.out.println("The file successfully encrypted. IV: " + client.getAuxResult());
              else
                System.out.println("The file successfully encrypted.");
            }
          } else if (commands[0].equalsIgnoreCase("d")) {
            if (commands.length < 5) {
              System.out.println("You need to pass four parameters: <object id>, <input>, <output> and <IV>");
            } else {
              client.setInputFile(commands[2]);
              client.setOutputFile(commands[3]);

              Utils utils = new Utils();
              byte[] IV = utils.hexDecode(commands[4]);

              client.decrypt(commands[1], "", IV, "", "", 0);

              System.out.println("The file successfully decrypted.");
            }
          } else if (commands[0].equalsIgnoreCase("s")) {
            if (commands.length < 4) {
              System.out.println("You need to pass three parameters: <object id>, <input> and <output>");
            } else {
              client.setInputFile(commands[2]);
              client.setOutputFile(commands[3]);

              client.sign(commands[1], "", "", "SHA256", false);

              System.out.println("The file successfully signed.");
            }
          } else if (commands[0].equalsIgnoreCase("v")) {
            if (commands.length < 4) {
              System.out.println("You need to pass three parameters: <object id>, <data> and <signature>");
            } else {
              client.setDataFile(commands[2]);
              client.setInputFile(commands[3]);

              client.verify(commands[1], "", "", "SHA256", false);

              switch (client.getSignatureValidationResult()) {
                case svtValid:
                  System.out.println("Verification succeeded.");
                  break;
                case svtCorrupted:
                  System.out.println("Verification corrupted.");
                  break;
                case svtFailure:
                  System.out.println("Verification failed.");
                  break;
                default:
                  System.out.println("Verification unknown.");
                  break;
              }
            }
          } else if (commands[0].equalsIgnoreCase("bye") || commands[0].equalsIgnoreCase("exit") || commands[0].equalsIgnoreCase("quit") || commands[0].equalsIgnoreCase("q")) {
            System.out.println();
            break;
          } else if (commands[0].equalsIgnoreCase("?") || commands[0].equalsIgnoreCase("help") || commands[0].equalsIgnoreCase("man")) {
            displayCommands();
          } else {
            System.out.println("Command not recognized.");
            displayCommands();
          }
        } else {
          System.out.println("Command not recognized.");
          displayCommands();
        }
      } catch (Exception ex) {
        System.out.println("Error: " + ex.getMessage());
      }
    }

    confirmExit();
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



