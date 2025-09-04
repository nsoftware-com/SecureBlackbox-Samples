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
import java.util.Calendar;
import java.util.Date;

import secureblackbox.*;
import neo.SAMLContentType;
import neo.SAMLDecisions;

public class samlwriter extends ConsoleDemo {
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
                "  samlwriter -- SecureBlackbox SAMLWriter Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  samlwriter <-type message_type> <output output_file> [-issuer issuer_url] [-dest destination_url] [-service service_url]\n" +
                "             [-scert sign_certificate_file] [-scertpass sign_certificate_password] [-hashalg hashalg]\n" +
                "             [-ecert encrypt_certificate_file] [-ecertpass encrypt_certificate_password]\n\n" +
                "DESCRIPTION\n" +
                "  This sample illustrates how to use SAMLWriter to create SAML messages.\n\n" +
                "  The options are as follows:\n\n" +
                "  -type        The type of SAML message (Required). Enter the corresponding number. Valid values:\n\n" +
                "                  0  - AuthnRequest\n" +
                "                  1  - LogoutRequest\n" +
                "                  2  - AttributeQuery\n" +
                "                  3  - SubjectQuery\n" +
                "                  4  - Assertion\n" +
                "                  5  - Enveloped Assertion\n\n" +
                "  -output      Where the SAML message file will be saved (Required).\n\n" +
                "  -issuer      The issuer URL.\n\n" +
                "  -dest        The destination URL.\n\n" +
                "  -service     The service URL.\n\n" +
                "  -scert       The certificate used to sign files.\n\n" +
                "  -scertpass   The password for the signing certificate.\n\n" +
                "  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224, MD5\n\n" +
                "  -ecert       The certificate used to encrypt files.\n\n" +
                "  -ecertpass   The password for the encrypting certificate.\n\n" +
                "EXAMPLES\n" +
                "  samlwriter -type 0 -output C:\\mess.saml -ecert C:\\certs\\mycert.pfx -ecertpass mypassword\n\n" +
                "  samlwriter -type 4 -output C:\\mess.saml -scert C:\\certs\\mycert.pfx -scertpass mypassword -hashalg SHA256 \n" +
                "           -issuer http://saml.localservice.com/metadata/ -dest http://saml.remoteservice.com/sso -service http://saml.localservice.com/acs\n\n"
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

    private static SAMLWriter writer;
    private static String issuer = "";
    private static String dest = "";
    private static String service = "";
    private static String scert = "";
    private static String scertpass = "";
    private static String ecert = "";
    private static String ecertpass = "";
    private static String hashalg = "SHA256";
    private static String output = "";

    private static String currentDateTime(int num) {
        Calendar c = Calendar.getInstance();
        c.setTime(new Date());
        c.add(Calendar.YEAR, num);

        Utils utils = new Utils();
        try {
            return utils.dateToString(c.getTime());
        } catch (Exception ex) {
            displayError(ex);
            return "";
        }
    }

    private static boolean applySecurity() {
        if (scert.length() > 0) {
            CertificateManager scm = new CertificateManager();
            try {
                scm.importFromFile(scert, scertpass);

                writer.setSigningCertificate(scm.getCertificate());
                writer.getMessage().setSigned(true);
                writer.getSecurity().setDigestMethod(hashalg);
            } catch (Exception ex) {
                displayError(ex);
                return false;
            }
        }

        if (ecert.length() > 0) {
            CertificateManager ecm = new CertificateManager();
            try {
                ecm.importFromFile(ecert, ecertpass);

                writer.setEncryptionCertificate(ecm.getCertificate());
                writer.getSecurity().setEncryptionMethod("AES128");
                writer.getAssertion().setAssertionType(SAMLAssertion.csatAssertion);
            } catch (Exception ex) {
                displayError(ex);
                return false;
            }
        }

        return true;
    }

    private static boolean createAuthnRequest() {
        boolean res = false;

        try {
            // creating a message of AuthnRequest type
            writer.createNew(SAMLContentType.cstyAuthnRequest);

            // main message properties
            writer.getMessage().setID("my-message-id-123");
            writer.getMessage().setIssueInstant(currentDateTime(0));
            writer.getMessage().setDestination(dest);
            writer.getMessage().setIssuer(writer.formatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

            // adding a subject confirmation
            writer.getMessage().setSubject(writer.getMessage().getIssuer());
            writer.addSubjectConfirmation("scmethod", "http://scaddress.com", "screcipient", "",
                    currentDateTime(-1), currentDateTime(1), "", "sctype", "scdata");

            // adding a couple of conditions
            writer.addCondition(SAMLCondition.csctAudienceRestriction, "PSCU:saml20:dev");
            writer.addCondition(SAMLCondition.csctNotBefore, currentDateTime(0));

            // setting up authnrequest parameters
            writer.getAuthnRequest().setAssertionConsumerServiceIndex(0);
            writer.getAuthnRequest().setAssertionConsumerServiceURL(service);
            writer.getAuthnRequest().setProtocolBinding("urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST");
            writer.getAuthnRequest().setProviderName("My Application");
            writer.getAuthnRequest().setNameIDPolicyFormat("urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress");
            writer.getAuthnRequest().setNameIDPolicyAllowCreate(true);
            writer.getAuthnRequest().setContextComparison(SAMLAuthnRequest.cacctExact);
            writer.getAuthnRequest().setContextClassRefs("urn:oasis:names:tc:SAML:2.0:ac:classes:PasswordProtectedTransport");
            writer.getAuthnRequest().setContextRefType(SAMLAuthnRequest.cacrtClass);

            // applying security
            if (applySecurity()) {
                // Saving the output
                writer.saveFile(output);

                res = true;
            }
        } catch (Exception ex) {
            displayError(ex);
        }

        return res;
    }

    private static boolean createLogoutRequest() {
        boolean res = false;

        try {
            // creating a message of LogoutRequest type
            writer.createNew(SAMLContentType.cstyLogoutRequest);

            // main message properties
            writer.getMessage().setID("my-message-id-123");
            writer.getMessage().setIssueInstant(currentDateTime(0));
            writer.getMessage().setDestination(dest);
            writer.getMessage().setIssuer(writer.formatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

            // setting up logoutrequest parameters
            writer.getLogoutRequest().setNameID(writer.formatID("id-abcdefghijkl", "", "urn:oasis:names:tc:SAML:2.0:nameid-format:transient", "", "", ""));
            writer.getLogoutRequest().setNotOnOrAfter(currentDateTime(0));
            writer.getLogoutRequest().setReason("Requested by user");
            writer.getLogoutRequest().setSessionIndexes("id-01234567890");

            // applying security
            if (applySecurity()) {
                // Saving the output
                writer.saveFile(output);

                res = true;
            }
        } catch (Exception ex) {
            displayError(ex);
        }

        return res;
    }

    private static boolean createAttributeQuery() {
        boolean res = false;

        try {
            // creating a message of AttributeQuery type
            writer.createNew(SAMLContentType.cstyAttributeQuery);

            // main message properties
            writer.getMessage().setID("my-message-id-123");
            writer.getMessage().setIssueInstant(currentDateTime(0));
            writer.getMessage().setDestination(dest);
            writer.getMessage().setIssuer(writer.formatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

            // setting up attributequery parameters: a couple of attributes we want
            writer.addAttribute("clientID", "JohnDoe123", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
                    "xs:string", 0);
            writer.addAttribute("clientCategorty", "Business", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
                    "xs:string", 0);

            // ... and a subject confirmation
            writer.getMessage().setSubject(writer.getMessage().getIssuer());
            writer.addSubjectConfirmation("scmethod", "http://scaddress.com", "screcipient", "",
                    currentDateTime(-1), currentDateTime(1), "", "sctype", "scdata");

            // applying security
            if (applySecurity()) {
                // Saving the output
                writer.saveFile(output);

                res = true;
            }
        } catch (Exception ex) {
            displayError(ex);
        }

        return res;
    }

    private static boolean createSubjectQuery() {
        boolean res = false;

        try {
            // creating a message of SubjectQuery type
            writer.createNew(SAMLContentType.cstySubjectQuery);

            // main message properties
            writer.getMessage().setID("my-message-id-123");
            writer.getMessage().setIssueInstant(currentDateTime(0));
            writer.getMessage().setDestination(dest);
            writer.getMessage().setIssuer(writer.formatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

            // setting up subjectquery parameters: a couple of subject confirmations
            writer.getMessage().setSubject(writer.getMessage().getIssuer());
            writer.addSubjectConfirmation("scmethod1", "http://scaddress.com", "screcipient", "",
                    currentDateTime(-1), currentDateTime(1), "", "sctype", "scdata");
            writer.addSubjectConfirmation("scmethod2", "Sandford, Gloucestershire", "screcipient", "",
                    currentDateTime(-1), currentDateTime(3), "", "sctype", "scdata");

            // applying security
            if (applySecurity()) {
                // Saving the output
                writer.saveFile(output);

                res = true;
            }
        } catch (Exception ex) {
            displayError(ex);
        }

        return res;
    }

    private static boolean createAssertion() {
        boolean res = false;

        try {
            // creating a message of Assertion type
            writer.createNew(SAMLContentType.cstyAssertion);

            // main message properties
            writer.getMessage().setID("my-message-id-123");
            writer.getMessage().setIssueInstant(currentDateTime(0));
            writer.getMessage().setDestination(dest);
            writer.getMessage().setIssuer(writer.formatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

            // a message may contain multiple complex assertions, each of which
            // should be enveloped into BeginAssertion/CompleteAssertion calls.
            writer.beginAssertion();
            writer.getAssertion().setIssuer(writer.getMessage().getIssuer()); // keeping it simple
            writer.getAssertion().setAssertionType(SAMLAssertion.csatAssertion);

            // An assertion may contain multiple attributes within multiple statements.
            // If we add an attribute without adding a statement first, a new statement
            // will be created automatically, and the attribute added to it.
            // For example, the below two attributes go to the first statement,
            // which is created automatically:
            writer.addAttribute("clientID", "JohnDoe123", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
                    "xs:string", 0);
            writer.addAttribute("clientCategorty", "Business", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
                    "xs:string", 0);

            // Let"s add another statement
            writer.addAttributeStatement(); // returns the index, which is going to be 1

            // Adding one attribute with two values - these are going to be merged
            // because their names are identical.
            writer.addAttribute("eduPersonAffiliation", "users", "", "", 1);
            writer.addAttribute("eduPersonAffiliation", "examplerole1", "", "", 1);

            // adding an authentication statement
            writer.addAuthnStatement(currentDateTime(0), "id-1234567890",
                    currentDateTime(1), "urn:oasis:names:tc:SAML:2.0:ac:classes:Password");

            // adding an authorization statement
            writer.addAuthzDecisionStatement(SAMLDecisions.csadnPermit, "Evidence", "Resource", "namespace=ns1;value=value1");

            // adding assertion conditions (audience and time scope)
            writer.addCondition(SAMLCondition.csctAudienceRestriction, "PSCU:saml20:dev");
            writer.addCondition(SAMLCondition.csctNotOnOrAfter, currentDateTime(1));

            // setting custom assertion ID (optional)
            writer.getAssertion().setID("unique-id-123456");

            // adding subject confirmations
            writer.getAssertion().setSubject(writer.formatID("Subject", "Subject", "", "", "", ""));
            writer.addSubjectConfirmation("scmethod1", "http://scaddress.com", "screcipient", "",
                    currentDateTime(-1), currentDateTime(1), "", "sctype", "scdata");

            // applying security: as it applies to the assertion, it should precede
            // the CompleteAssertion() call
            if (applySecurity()) {
                // adding ("committing") the formed assertion to the SAML message
                writer.completeAssertion();

                // Saving the output
                writer.saveFile(output);

                res = true;
            }
        } catch (Exception ex) {
            displayError(ex);
        }

        return res;
    }

    private static boolean createEnvelopedAssertion() {
        boolean res = false;

        try {
            // creating a message of Response type
            writer.createNew(SAMLContentType.cstyResponse);

            // main message properties
            writer.getMessage().setID("my-message-id-123");
            writer.getMessage().setIssueInstant(currentDateTime(0));
            writer.getMessage().setDestination(dest);
            writer.getMessage().setIssuer(writer.formatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

            // a message may contain multiple complex assertions, each of which
            // should be enveloped into BeginAssertion/CompleteAssertion calls.
            writer.beginAssertion();
            writer.getAssertion().setIssuer(writer.getMessage().getIssuer()); // keeping it simple
            writer.getAssertion().setAssertionType(SAMLAssertion.csatAssertion);

            // An assertion may contain multiple attributes within multiple statements.
            // If we add an attribute without adding a statement first, a new statement
            // will be created automatically, and the attribute added to it.
            // For example, the below two attributes go to the first statement,
            // which is created automatically:
            writer.addAttribute("clientID", "JohnDoe123", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
                    "xs:string", 0);
            writer.addAttribute("clientCategorty", "Business", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
                    "xs:string", 0);

            // adding an authentication statement
            writer.addAuthnStatement(currentDateTime(0), "id-1234567890",
                    currentDateTime(1), "urn:oasis:names:tc:SAML:2.0:ac:classes:Password");

            // adding assertion conditions (audience and time scope)
            writer.addCondition(SAMLCondition.csctAudienceRestriction, "PSCU:saml20:dev");
            writer.addCondition(SAMLCondition.csctNotOnOrAfter, currentDateTime(1));

            // setting custom assertion ID (optional)
            writer.getAssertion().setID("unique-id-123456");

            // adding subject confirmations
            writer.getAssertion().setSubject(writer.formatID("Subject", "Subject", "", "", "", ""));
            writer.addSubjectConfirmation("scmethod1", "http://scaddress.com", "screcipient", "",
                    currentDateTime(-1), currentDateTime(1), "", "sctype", "scdata");

            // applying security: as it applies to the assertion, it should precede
            // the CompleteAssertion() call
            if (applySecurity()) {
                // adding ("committing") the formed assertion to the SAML message
                writer.completeAssertion();

                // Saving the output
                writer.saveFile(output);

                res = true;
            }
        } catch (Exception ex) {
            displayError(ex);
        }

        return res;
    }

    public static void main(String[] args) {
        if (args.length == 0) {
            displayHelp("");
            return;
        }

        writer = new SAMLWriter();
        int type = -1;
        if (optext(args, "-type")) {
            type = Integer.parseInt(optval(args, "-type"));
        }
        else {
            displayHelp("-type is required.");
            return;
        }

        if (optext(args, "-output")) {
            output = optval(args, "-output");
        }
        else {
            displayHelp("-output is required.");
            return;
        }

        issuer = optval(args, "-issuer");

        dest = optval(args, "-dest");

        service = optval(args, "-service");

        scert = optval(args, "-scert");
        scertpass = optval(args, "-scertpass");

        ecert = optval(args, "-ecert");
        ecertpass = optval(args, "-ecertpass");

        if (optext(args, "-hashalg")) {
            hashalg = optval(args, "-hashalg");
        }

        try {
            boolean res = false;

            switch (type) {
                case 0:
                    res = createAuthnRequest();
                    break;
                case 1:
                    res = createLogoutRequest();
                    break;
                case 2:
                    res = createAttributeQuery();
                    break;
                case 3:
                    res = createSubjectQuery();
                    break;
                case 4:
                    res = createAssertion();
                    break;
                case 5:
                    res = createEnvelopedAssertion();
                    break;
                default:
                    displayHelp("Invalid type value.");
                    return;
            }

            if (res)
                System.out.println("The SAML message was created successfully.\n\n");

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



