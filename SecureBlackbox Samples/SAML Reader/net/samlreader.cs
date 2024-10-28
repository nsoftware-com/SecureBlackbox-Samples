/*
 * SecureBlackbox 2024 .NET Edition - Sample Project
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
 * 
 */

﻿using System;
using _CA;
using nsoftware.SecureBlackbox;

class samlreader
{
    private static string optval(string[] args, string option)
    {
        for (int x = 0; x < args.Length - 1; x++)
        {
            if (args[x].Equals(option, StringComparison.CurrentCultureIgnoreCase))
            {
                return args[x + 1];
            }
        }
        return "";
    }

    private static bool optext(string[] args, string option)
    {
        for (int x = 0; x < args.Length - 1; x++)
        {
            if (args[x].Equals(option, StringComparison.CurrentCultureIgnoreCase))
            {
                return true;
            }
        }
        return false;
    }

    private static void displayHelp(string errMes)
    {
        Console.WriteLine(
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

        if (errMes.Length > 0)
        {
            Console.WriteLine("Error: " + errMes);
            Console.WriteLine();
        }

        confirmExit();
    }

    private static void confirmExit()
    {
        Console.WriteLine("Press Enter to exit the demo.");
        Console.ReadLine();
    }

    public static void outputSubjectConfirmations(SAMLReader reader)
    {
        Console.WriteLine("  Subject confirmations:");
        for (int i = 0; i < reader.SubjectConfirmations.Count; i++)
        {
            Console.WriteLine("    Subject confirmation #" + (i + 1));
            Console.WriteLine("      Address: " + reader.SubjectConfirmations[i].Address);
            Console.WriteLine("      Data: " + reader.SubjectConfirmations[i].Data);
            Console.WriteLine("      Data type: " + reader.SubjectConfirmations[i].DataType);
            Console.WriteLine("      ID: " + reader.SubjectConfirmations[i].ID);
            Console.WriteLine("      Method: " + reader.SubjectConfirmations[i].Method);
            Console.WriteLine("      Not before: " + reader.SubjectConfirmations[i].NotBefore);
            Console.WriteLine("      Not after: " + reader.SubjectConfirmations[i].NotOnOrAfter);
        }
        Console.WriteLine();
    }

    public static void outputConditions(SAMLReader reader)
    {
        Console.WriteLine("  Conditions:");

        for (int i = 0; i < reader.Conditions.Count; i++)
        {
            Console.WriteLine("    Condition #" + (i + 1) + ":");

            switch (reader.Conditions[i].ConditionType)
            {
                case SAMLConditionTypes.csctAudienceRestriction:
                    Console.WriteLine("      Condition type: Audience restriction");
                    break;
                case SAMLConditionTypes.csctOneTimeUse:
                    Console.WriteLine("      Condition type: One-time use");
                    break;
                case SAMLConditionTypes.csctProxyRestriction:
                    Console.WriteLine("      Condition type: Proxy restriction");
                    break;
                case SAMLConditionTypes.csctNotBefore:
                    Console.WriteLine("      Condition type: Not before");
                    break;
                case SAMLConditionTypes.csctNotOnOrAfter:
                    Console.WriteLine("      Condition type: Not after");
                    break;
            }

            Console.WriteLine("      Condition: " + reader.Conditions[i].Condition);
        }
        Console.WriteLine();
    }

    public static void outputAttributes(SAMLReader reader)
    {
        Console.WriteLine("  Attributes:\n");
        for (int i = 0; i < reader.Attributes.Count; i++)
        {
            Console.WriteLine("    Attribute #" + (i + 1) + ":");
            Console.WriteLine("      Name: " + reader.Attributes[i].Name);
            Console.WriteLine("      Name Format: " + reader.Attributes[i].NameFormat);
            Console.WriteLine("      Friendly Name: " + reader.Attributes[i].FriendlyName);
            Console.WriteLine("      Statement Index: " + reader.Attributes[i].StatementIndex);
            Console.WriteLine("      Value(s): " + reader.Attributes[i].Values);
        }
        Console.WriteLine();
    }

    public static void outputSubjectQuery(SAMLReader reader)
    {
        outputSubjectConfirmations(reader);
    }

    public static void outputAttributeQuery(SAMLReader reader)
    {
        outputAttributes(reader);
        outputSubjectConfirmations(reader);
    }

    public static void outputAuthnRequest(SAMLReader reader)
    {
        Console.WriteLine("  Assertion Consumer Service URL: " + reader.AuthnRequest.AssertionConsumerServiceURL);
        Console.WriteLine("  Protocol binding: " + reader.AuthnRequest.ProtocolBinding);
        Console.WriteLine("  Provider name: " + reader.AuthnRequest.ProviderName);
        Console.WriteLine("  NameID policy format: " + reader.AuthnRequest.NameIDPolicyFormat);
        Console.WriteLine("  Context class refs: " + reader.AuthnRequest.ContextClassRefs);
        outputSubjectConfirmations(reader);
        outputConditions(reader);
    }

    public static void outputLogoutRequest(SAMLReader reader)
    {
        Console.WriteLine("  NameID: " + reader.LogoutRequest.NameID);
        Console.WriteLine("  Not on or after: " + reader.LogoutRequest.NotOnOrAfter);
        Console.WriteLine("  Reason: " + reader.LogoutRequest.Reason);
        Console.WriteLine("  Session index(es): " + reader.LogoutRequest.SessionIndexes);
        Console.WriteLine();
    }

    public static void outputStatements(SAMLReader reader)
    {
        Console.WriteLine("  Statements:\n");

        for (int i = 0; i < reader.Statements.Count; i++)
        {
            Console.WriteLine("    Statement #" + (i + 1));
            switch (reader.Statements[i].StatementType)
            {
                case SAMLAssertionStatementTypes.csastAuthn:
                    Console.WriteLine("      Type: AuthnStatement");
                    Console.WriteLine("      Context class ref: " + reader.Statements[i].AuthnContextClassRef);
                    Console.WriteLine("      Instant: " + reader.Statements[i].AuthnInstant);
                    Console.WriteLine("      Session index: " + reader.Statements[i].AuthnSessionIndex);
                    Console.WriteLine("      Not on or after: " + reader.Statements[i].AuthnSessionNotOnOrAfter);
                    break;
                case SAMLAssertionStatementTypes.csastAttribute:
                    Console.WriteLine("      Type: AttributeStatement (see attributes below)\n");
                    break;
                case SAMLAssertionStatementTypes.csastAuthzDecision:
                    Console.WriteLine("      Type: AuthzDecisionStatement\n");
                    Console.WriteLine("      Actions: " + reader.Statements[i].AuthzActions);
                    Console.WriteLine("      Evidence: " + reader.Statements[i].AuthzDecisionEvidence);
                    switch (reader.Statements[i].AuthzDecision)
                    {
                        case SAMLDecisions.csadnPermit:
                            Console.WriteLine("      Decision: Permit");
                            break;
                        case SAMLDecisions.csadnDeny:
                            Console.WriteLine("      Decision: Deny");
                            break;

                        default:
                            Console.WriteLine("      Decision: Indeterminate");
                            break;
                    }
                    break;
                default:
                    Console.WriteLine("      Type: unknown");
                    break;
            }
        }
        Console.WriteLine();
    }

    public static void outputAssertion(SAMLReader reader)
    {
        // outputting currently selected ("pinned") assertion
        Console.WriteLine("  Issuer: " + reader.PinnedAssertion.Issuer);
        Console.WriteLine("  Assertion ID: " + reader.PinnedAssertion.ID);
        Console.WriteLine("  Subject: " + reader.PinnedAssertion.Subject);
        Console.WriteLine();

        outputStatements(reader);
        outputAttributes(reader);
        outputConditions(reader);
        outputSubjectConfirmations(reader);
    }

    public static void outputResponse(SAMLReader reader)
    {
        // outputting all assertions, one by one
        Console.WriteLine("  Assertions found: " + reader.AssertionCount);

        for (int i = 0; i < reader.AssertionCount; i++)
        {
            Console.WriteLine("    Assertion #" + i);
            try
            {
                reader.PinAssertion(i); // selecting the assertion to process
                outputAssertion(reader);
            }
            catch (Exception ex)
            {
                Console.WriteLine("Error: " + ex.Message);
            }
        }
    }

    static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            displayHelp("");
            return;
        }

        SAMLReader reader = new SAMLReader();      
        string input;

        if (optext(args, "-input"))
        {
            input = optval(args, "-input");
        }
        else
        {
            displayHelp("-input is required.");
            return;
        }

        if (optext(args, "-vcert"))
        {
            CertificateManager vcm = new CertificateManager();
            vcm.ImportFromFile(optval(args, "-vcert"), optval(args, "-vcertpass"));
            reader.Certificates.Add(vcm.Certificate);
        }

        if (optext(args, "-dcert"))
        {
            CertificateManager dcm = new CertificateManager();
            dcm.ImportFromFile(optval(args, "-dcert"), optval(args, "-dcertpass"));
            reader.DecryptionCertificate = dcm.Certificate;
        }

        // opening the message
        Console.WriteLine("Starting message processing.\n");

        try
        {
            reader.OpenFile(input);

            // outputting the details
            Console.WriteLine("Message loaded successfully.\n");
            Console.WriteLine("General details: ");
            Console.WriteLine("  Destination: " + reader.Message.Destination);
            Console.WriteLine("  Issue instant: " + reader.Message.IssueInstant);
            Console.WriteLine("  ID: " + reader.Message.ID);
            Console.WriteLine("  Issuer: " + reader.Message.Issuer);
            Console.WriteLine("  In response to: " + reader.Message.InResponseTo);

            switch (reader.Message.ContentType)
            {
                case SAMLContentTypes.cstyNone:
                    Console.WriteLine("  Message type: Unknown\n");
                    break;
                case SAMLContentTypes.cstyAssertionIDRequest:
                    Console.WriteLine("  Message type: AssertionIDRequest\n");
                    break;
                case SAMLContentTypes.cstySubjectQuery:
                    Console.WriteLine("  Message type: SubjectQuery\n");
                    outputSubjectQuery(reader);
                    break;
                case SAMLContentTypes.cstyAuthnQuery:
                    Console.WriteLine("  Message type: AuthnQuery\n");
                    break;
                case SAMLContentTypes.cstyAttributeQuery:
                    Console.WriteLine("  Message type: AttributeQuery\n");
                    outputAttributeQuery(reader);
                    break;
                case SAMLContentTypes.cstyAuthzDecisionQuery:
                    Console.WriteLine("  Message type: AuthzDecisionQuery\n");
                    break;
                case SAMLContentTypes.cstyAuthnRequest:
                    Console.WriteLine("  Message type: AuthnRequest\n");
                    outputAuthnRequest(reader);
                    break;
                case SAMLContentTypes.cstyManageNameIDRequest:
                    Console.WriteLine("  Message type: ManageNameIDRequest\n");
                    break;
                case SAMLContentTypes.cstyLogoutRequest:
                    Console.WriteLine("  Message type: LogoutRequest\n");
                    outputLogoutRequest(reader);
                    break;
                case SAMLContentTypes.cstyNameIDMappingRequest:
                    Console.WriteLine("  Message type: NameIDMappingRequest\n");
                    break;
                case SAMLContentTypes.cstyArtifactResolve:
                    Console.WriteLine("  Message type: ArtifactResolve\n");
                    break;
                case SAMLContentTypes.cstyResponse:
                    Console.WriteLine("  Message type: Response\n");
                    outputResponse(reader);
                    break;
                default:
                    break;
            }

            // global security props
            if (reader.Message.Signed || reader.PinnedAssertion.Signed)
            {
                Console.WriteLine("The processed message was signed by its author.");
                if (reader.SigningCertificate != null)
                    Console.WriteLine("Signing certificate: " + reader.SigningCertificate.SubjectRDN);
                else
                    Console.WriteLine("Signing certificate was not found");
                Console.WriteLine("Signature validation result: " + reader.Message.SignatureValidationResult);
                Console.WriteLine("Digest method: " + reader.Security.DigestMethod);
            }

            if (((reader.Message.ContentType == SAMLContentTypes.cstyAssertion) || (reader.Message.ContentType == SAMLContentTypes.cstyResponse)) &&
                (reader.PinnedAssertion.AssertionType == SAMLAssertionTypes.csatEncryptedAssertion))
            {
                Console.WriteLine("The assertion was encrypted by its author.");
                Console.WriteLine("Encryption method: " + reader.Security.EncryptionMethod);
            }

            confirmExit();
        }
        catch (Exception ex)
        {
            Console.WriteLine("Error: " + ex.Message);
        }
    }
}





class ConsoleDemo
{
  /// <summary>
  /// Takes a list of switch arguments or name-value arguments and turns it into a dictionary.
  /// </summary>
  public static System.Collections.Generic.Dictionary<string, string> ParseArgs(string[] args)
  {
    System.Collections.Generic.Dictionary<string, string> dict = new System.Collections.Generic.Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // Add a key to the dictionary for each argument.
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/", then it is a value.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Save the value and skip the next entry in the list of arguments.
          dict.Add(args[i].ToLower().TrimStart('/'), args[i + 1]);
          i++;
        }
        else
        {
          // If the next argument starts with a "/", then we assume the current one is a switch.
          dict.Add(args[i].ToLower().TrimStart('/'), "");
        }
      }
      else
      {
        // If the argument does not start with a "/", store the argument based on the index.
        dict.Add(i.ToString(), args[i].ToLower());
      }
    }
    return dict;
  }
  /// <summary>
  /// Asks for user input interactively and returns the string response.
  /// </summary>
  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}