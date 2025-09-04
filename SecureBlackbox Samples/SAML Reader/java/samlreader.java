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

import static secureblackbox.SAMLAssertionStatement.*;
import static secureblackbox.SAMLCondition.*;
import static secureblackbox.SAMLMessage.*;

public class samlreader extends ConsoleDemo {
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
                "  samlreader -- SecureBlackbox SAMLReader Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  samlreader <-input input_file>  [-vcert verify_certificate_file] [-vcertpass verify_certificate_password]\n" +
                "             [-dcert decrypt_certificate_file] [-dcertpass decrypt_certificate_password]\n\n" +
                "DESCRIPTION\n" +
                "  This sample illustrates the use of SAMLReader to read and decompose SAML messages.\n\n" +
                "  The options are as follows:\n\n" +
                "  -input       An input file to read (Required).\n\n" +
                "  -vcert       The certificate used to verify files.\n\n" +
                "  -vcertpass   The password for the verifying certificate.\n\n" +
                "  -dcert       The certificate used to decrypt files.\n\n" +
                "  -dcertpass   The password for the decrypting certificate.\n\n" +
                "EXAMPLES\n" +
                "  samlreader -input C:\\mess.saml\n\n" +
                "  samlreader -input C:\\mess.saml -vcert C:\\certs\\mycert.pfx -vcertpass mypassword\n\n"
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

    public static void outputSubjectConfirmations(SAMLReader reader) {
        System.out.println("  Subject confirmations:");
        for (int i = 0; i < reader.getSubjectConfirmations().size(); i++) {
            System.out.println("    Subject confirmation #" + (i + 1));
            System.out.println("      Address: " + reader.getSubjectConfirmations().item(i).getAddress());
            System.out.println("      Data: " + reader.getSubjectConfirmations().item(i).getData());
            System.out.println("      Data type: " + reader.getSubjectConfirmations().item(i).getDataType());
            System.out.println("      ID: " + reader.getSubjectConfirmations().item(i).getID());
            System.out.println("      Method: " + reader.getSubjectConfirmations().item(i).getMethod());
            System.out.println("      Not before: " + reader.getSubjectConfirmations().item(i).getNotBefore());
            System.out.println("      Not after: " + reader.getSubjectConfirmations().item(i).getNotOnOrAfter());
        }
        System.out.println();
    }

    public static void outputConditions(SAMLReader reader) {
        System.out.println("  Conditions:");

        for (int i = 0; i < reader.getConditions().size(); i++) {
            System.out.println("    Condition #" + (i + 1) + ":");

            switch (reader.getConditions().item(i).getConditionType()) {
                case csctAudienceRestriction:
                    System.out.println("      Condition type: Audience restriction");
                    break;
                case csctOneTimeUse:
                    System.out.println("      Condition type: One-time use");
                    break;
                case csctProxyRestriction:
                    System.out.println("      Condition type: Proxy restriction");
                    break;
                case csctNotBefore:
                    System.out.println("      Condition type: Not before");
                    break;
                case csctNotOnOrAfter:
                    System.out.println("      Condition type: Not after");
                    break;
            }

            System.out.println("      Condition: " + reader.getConditions().item(i).getCondition());
        }
        System.out.println();
    }

    public static void outputAttributes(SAMLReader reader) {
        System.out.println("  Attributes:\n");
        for (int i = 0; i < reader.getAttributes().size(); i++) {
            System.out.println("    Attribute #" + (i + 1) + ":");
            System.out.println("      Name: " + reader.getAttributes().item(i).getName());
            System.out.println("      Name Format: " + reader.getAttributes().item(i).getNameFormat());
            System.out.println("      Friendly Name: " + reader.getAttributes().item(i).getFriendlyName());
            System.out.println("      Statement Index: " + reader.getAttributes().item(i).getStatementIndex());
            System.out.println("      Value(s): " + reader.getAttributes().item(i).getValues());
        }
        System.out.println();
    }

    public static void outputSubjectQuery(SAMLReader reader) {
        outputSubjectConfirmations(reader);
    }

    public static void outputAttributeQuery(SAMLReader reader) {
        outputAttributes(reader);
        outputSubjectConfirmations(reader);
    }

    public static void outputAuthnRequest(SAMLReader reader) {
        System.out.println("  Assertion Consumer Service URL: " + reader.getAuthnRequest().getAssertionConsumerServiceURL());
        System.out.println("  Protocol binding: " + reader.getAuthnRequest().getProtocolBinding());
        System.out.println("  Provider name: " + reader.getAuthnRequest().getProviderName());
        System.out.println("  NameID policy format: " + reader.getAuthnRequest().getNameIDPolicyFormat());
        System.out.println("  Context class refs: " + reader.getAuthnRequest().getContextClassRefs());
        outputSubjectConfirmations(reader);
        outputConditions(reader);
    }

    public static void outputLogoutRequest(SAMLReader reader) {
        System.out.println("  NameID: " + reader.getLogoutRequest().getNameID());
        System.out.println("  Not on or after: " + reader.getLogoutRequest().getNotOnOrAfter());
        System.out.println("  Reason: " + reader.getLogoutRequest().getReason());
        System.out.println("  Session index(es): " + reader.getLogoutRequest().getSessionIndexes());
        System.out.println();
    }

    public static void outputStatements(SAMLReader reader) {
        System.out.println("  Statements:\n");

        for (int i = 0; i < reader.getStatements().size(); i++) {
            System.out.println("    Statement #" + (i + 1));
            switch (reader.getStatements().item(i).getStatementType()) {
                case csastAuthn:
                    System.out.println("      Type: AuthnStatement");
                    System.out.println("      Context class ref: " + reader.getStatements().item(i).getAuthnContextClassRef());
                    System.out.println("      Instant: " + reader.getStatements().item(i).getAuthnInstant());
                    System.out.println("      Session index: " + reader.getStatements().item(i).getAuthnSessionIndex());
                    System.out.println("      Not on or after: " + reader.getStatements().item(i).getAuthnSessionNotOnOrAfter());
                    break;
                case csastAttribute:
                    System.out.println("      Type: AttributeStatement (see attributes below)\n");
                    break;
                case csastAuthzDecision:
                    System.out.println("      Type: AuthzDecisionStatement\n");
                    System.out.println("      Actions: " + reader.getStatements().item(i).getAuthzActions());
                    System.out.println("      Evidence: " + reader.getStatements().item(i).getAuthzDecisionEvidence());
                    switch (reader.getStatements().item(i).getAuthzDecision()) {
                        case csadnPermit:
                            System.out.println("      Decision: Permit");
                            break;
                        case csadnDeny:
                            System.out.println("      Decision: Deny");
                            break;

                        default:
                            System.out.println("      Decision: Indeterminate");
                            break;
                    }
                    break;
                default:
                    System.out.println("      Type: unknown");
                    break;
            }
        }
        System.out.println();
    }

    public static void outputAssertion(SAMLReader reader) {
        // outputting currently selected ("pinned") assertion
        System.out.println("  Issuer: " + reader.getPinnedAssertion().getIssuer());
        System.out.println("  Assertion ID: " + reader.getPinnedAssertion().getID());
        System.out.println("  Subject: " + reader.getPinnedAssertion().getSubject());
        System.out.println();

        outputStatements(reader);
        outputAttributes(reader);
        outputConditions(reader);
        outputSubjectConfirmations(reader);
    }

    public static void outputResponse(SAMLReader reader) {
        // outputting all assertions, one by one
        System.out.println("  Assertions found: " + reader.getAssertionCount());

        for (int i = 0; i < reader.getAssertionCount(); i++) {
            System.out.println("    Assertion #" + i);
            try {
                reader.pinAssertion(i); // selecting the assertion to process
                outputAssertion(reader);
            } catch (Exception ex) {
                ConsoleDemo.displayError(ex);
            }
        }
    }

    public static void main(String[] args) {
        if (args.length == 0) {
            displayHelp("");
            return;
        }

        SAMLReader reader = new SAMLReader();

        try {
            reader.addSAMLReaderEventListener(new SAMLReaderEventListener() {
                @Override
                public void encrypted(SAMLReaderEncryptedEvent e) {
                    System.out.println("The message is encrypted.");
                    if (e.needCredential)
                        System.out.println("The decryption certificate is not available. If you did provide a decryption certificate, make sure it matches the private key used by the message creator.");
                    else
                        System.out.println("The decryption certificate provided matches.");
                }

                @Override
                public void error(SAMLReaderErrorEvent e) {

                }

                @Override
                public void notification(SAMLReaderNotificationEvent e) {

                }

                @Override
                public void signatureFound(SAMLReaderSignatureFoundEvent e) {
                    System.out.println("A signature was found in the entity being processed.");
                    if (e.scope == 2) // sssAssertion
                        System.out.println("The signature covers an assertion.");
                    else if (e.scope == 1) // sssMessage
                        System.out.println("The signature covers the whole message.");
                    else if (e.scope == 3) // sssBinding
                        System.out.println("The signature was made over the binding.");
                    else
                        System.out.println("The signature covers something else - this doesn't look right.");

                    if (e.certFound) {
                        System.out.println("The verification certificate is available, either by being included in the signature or provided by you.");
                        e.validate = true;
                    }
                    else
                    {
                        System.out.println("The verification certificate was not found - will be unable to validate the signature.");
                        e.validate = false;
                    }
                }

                @Override
                public void signatureValidated(SAMLReaderSignatureValidatedEvent e) {
                    switch (e.validationResult)
                    {
                        case svtValid:
                            System.out.println("The signature has been validated. Validation result: VALID");
                            break;
                        case svtCorrupted:
                            System.out.println("The signature has been validated. Validation result: CORRUPTED");
                            break;
                        case svtSignerNotFound:
                            System.out.println("The signature has been validated. Validation result: SIGNING CERTIFICATE NOT FOUND");
                            break;
                        case svtFailure:
                            System.out.println("The signature has been validated. Validation result: FAILED TO VALIDATE");
                            break;
                        case svtReferenceCorrupted:
                            System.out.println("The signature has been validated. Validation result: REFERENCE CORRUPTED");
                            break;
                        default:
                            System.out.println("The signature has been validated. Validation result: UNKNOWN");
                            break;
                    }
                }

                @Override
                public void supercoreIntercept(SAMLReaderSupercoreInterceptEvent e) {

                }
            });

            String input;
            if (optext(args, "-input")) {
                input = optval(args, "-input");
            } else {
                displayHelp("-input is required.");
                return;
            }

            if (optext(args, "-vcert")) {
                CertificateManager vcm = new CertificateManager();
                vcm.importFromFile(optval(args, "-vcert"), optval(args, "-vcertpass"));
                reader.getCertificates().add(vcm.getCertificate());
            }

            if (optext(args, "-dcert")) {
                CertificateManager dcm = new CertificateManager();
                dcm.importFromFile(optval(args, "-dcert"), optval(args, "-dcertpass"));
                reader.setDecryptionCertificate(dcm.getCertificate());
            }

            // opening the message
            System.out.println("Starting message processing.\n");

            reader.openFile(input);

            // outputting the details
            System.out.println("Message loaded successfully.");
            System.out.println("General details: ");
            System.out.println("  Destination: " + reader.getMessage().getDestination());
            System.out.println("  Issue instant: " + reader.getMessage().getIssueInstant());
            System.out.println("  ID: " + reader.getMessage().getID());
            System.out.println("  Issuer: " + reader.getMessage().getIssuer());
            System.out.println("  In response to: " + reader.getMessage().getInResponseTo());

            switch (reader.getMessage().getContentType())
            {
                case cstyNone:
                    System.out.println("Message type: Unknown\n");
                    break;
                case cstyAssertionIDRequest:
                    System.out.println("Message type: AssertionIDRequest\n");
                    break;
                case cstySubjectQuery:
                    System.out.println("Message type: SubjectQuery\n");
                    outputSubjectQuery(reader);
                    break;
                case cstyAuthnQuery:
                    System.out.println("Message type: AuthnQuery\n");
                    break;
                case cstyAttributeQuery:
                    System.out.println("Message type: AttributeQuery\n");
                    outputAttributeQuery(reader);
                    break;
                case cstyAuthzDecisionQuery:
                    System.out.println("Message type: AuthzDecisionQuery\n");
                    break;
                case cstyAuthnRequest:
                    System.out.println("Message type: AuthnRequest\n");
                    outputAuthnRequest(reader);
                    break;
                case cstyManageNameIDRequest:
                    System.out.println("Message type: ManageNameIDRequest\n");
                    break;
                case cstyLogoutRequest:
                    System.out.println("Message type: LogoutRequest\n");
                    outputLogoutRequest(reader);
                    break;
                case cstyNameIDMappingRequest:
                    System.out.println("Message type: NameIDMappingRequest\n");
                    break;
                case cstyArtifactResolve:
                    System.out.println("Message type: ArtifactResolve\n");
                    break;
                case cstyResponse:
                    System.out.println("Message type: Response\n");
                    outputResponse(reader);
                    break;
                default:
                    break;
            }

            // global security props
            if (reader.getMessage().getSigned() || reader.getPinnedAssertion().getSigned()) {
                System.out.println("The processed message was signed by its author.");
                if (reader.getSigningCertificate() != null)
                    System.out.println("Signing certificate: " + reader.getSigningCertificate().getSubjectRDN());
                else
                    System.out.println("Signing certificate was not found");
                System.out.println("Signature validation result: " + reader.getMessage().getSignatureValidationResult());
                System.out.println("Digest method: " + reader.getSecurity().getDigestMethod());
            }

            if (((reader.getMessage().getContentType() == cstyAssertion) || (reader.getMessage().getContentType() == cstyResponse)) &&
                    (reader.getPinnedAssertion().getAssertionType() == SAMLAssertion.csatEncryptedAssertion))
            {
                System.out.println("The assertion was encrypted by its author.");
                System.out.println("Encryption method: " + reader.getSecurity().getEncryptionMethod());
            }

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



