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
using nsoftware.SecureBlackbox;

class samlwriter
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

    private static void displayHelp(String errMes)
    {
        Console.WriteLine(
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

    private static String currentDateTime(int num)
    {
        DateTime currDate = DateTime.Now;
        currDate.AddYears(num);

        Utils utils = new Utils();
        return utils.DateToString(currDate);
    }

    private static void applySecurity()
    {
        if (scert.Length > 0)
        {
            CertificateManager scm = new CertificateManager();
            scm.ImportFromFile(scert, scertpass);

            writer.Message.Signed = true;
            writer.SigningCertificate = scm.Certificate;
            writer.Security.DigestMethod = hashalg;
        }

        if (ecert.Length > 0)
        {
            CertificateManager ecm = new CertificateManager();
            ecm.ImportFromFile(ecert, ecertpass);

            writer.EncryptionCertificate = ecm.Certificate;
            writer.Security.EncryptionMethod = "AES128";
            writer.Assertion.AssertionType = SAMLAssertionTypes.csatEncryptedAssertion;
        }
    }

    private static void createAuthnRequest()
    {
        // creating a message of AuthnRequest type
        writer.CreateNew((int)SAMLContentTypes.cstyAuthnRequest);

        // main message properties
        writer.Message.ID = "my-message-id-123";
        writer.Message.IssueInstant = currentDateTime(0);
        writer.Message.Destination = dest;
        writer.Message.Issuer = writer.FormatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", "");

        // adding a subject confirmation
        writer.Message.Subject = writer.Message.Issuer;
        writer.AddSubjectConfirmation("scmethod", "http://scaddress.com", "screcipient", "",
            currentDateTime(-1), currentDateTime(1), "", "sctype", "scdata");

        // adding a couple of conditions
        writer.AddCondition((int)SAMLConditionTypes.csctAudienceRestriction, "PSCU:saml20:dev");
        writer.AddCondition((int)SAMLConditionTypes.csctNotBefore, currentDateTime(0));

        // setting up authnrequest parameters
        writer.AuthnRequest.AssertionConsumerServiceIndex = 0;
        writer.AuthnRequest.AssertionConsumerServiceURL = service;
        writer.AuthnRequest.ProtocolBinding = "urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST";
        writer.AuthnRequest.ProviderName = "My Application";
        writer.AuthnRequest.NameIDPolicyFormat = "urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress";
        writer.AuthnRequest.NameIDPolicyAllowCreate = true;
        writer.AuthnRequest.ContextComparison = SAMLAuthnContextComparisons.cacctExact;
        writer.AuthnRequest.ContextClassRefs = "urn:oasis:names:tc:SAML:2.0:ac:classes:PasswordProtectedTransport";
        writer.AuthnRequest.ContextRefType = SAMLAuthnRefTypes.cacrtClass;

        // applying security
        applySecurity();

        // Saving the output
        writer.SaveFile(output);
    }

    private static void createLogoutRequest()
    {
        // creating a message of LogoutRequest type
        writer.CreateNew((int)SAMLContentTypes.cstyLogoutRequest);

        // main message properties
        writer.Message.ID = "my-message-id-123";
        writer.Message.IssueInstant = currentDateTime(0);
        writer.Message.Destination = dest;
        writer.Message.Issuer = writer.FormatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", "");

        // setting up logoutrequest parameters
        writer.LogoutRequest.NameID = writer.FormatID("id-abcdefghijkl", "", "urn:oasis:names:tc:SAML:2.0:nameid-format:transient", "", "", "");
        writer.LogoutRequest.NotOnOrAfter = currentDateTime(0);
        writer.LogoutRequest.Reason = "Requested by user";
        writer.LogoutRequest.SessionIndexes = "id-01234567890";

        // applying security
        applySecurity();

        // Saving the output
        writer.SaveFile(output);
    }

    private static void createAttributeQuery()
    {
        // creating a message of AttributeQuery type
        writer.CreateNew((int)SAMLContentTypes.cstyAttributeQuery);

        // main message properties
        writer.Message.ID = "my-message-id-123";
        writer.Message.IssueInstant = currentDateTime(0);
        writer.Message.Destination = dest;
        writer.Message.Issuer = writer.FormatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", "");

        // setting up attributequery parameters: a couple of attributes we want
        writer.AddAttribute("clientID", "JohnDoe123", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
            "xs:string", 0);
        writer.AddAttribute("clientCategorty", "Business", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
            "xs:string", 0);

        // ... and a subject confirmation
        writer.Message.Subject = writer.Message.Issuer;
        writer.AddSubjectConfirmation("scmethod", "http://scaddress.com", "screcipient", "",
            currentDateTime(-1), currentDateTime(1), "", "sctype", "scdata");

        // applying security
        applySecurity();

        // Saving the output
        writer.SaveFile(output);
    }

    private static void createSubjectQuery()
    {
        // creating a message of SubjectQuery type
        writer.CreateNew((int)SAMLContentTypes.cstySubjectQuery);

        // main message properties
        writer.Message.ID = "my-message-id-123";
        writer.Message.IssueInstant = currentDateTime(0);
        writer.Message.Destination = dest;
        writer.Message.Issuer = writer.FormatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", "");

        // setting up subjectquery parameters: a couple of subject confirmations
        writer.Message.Subject = writer.Message.Issuer;
        writer.AddSubjectConfirmation("scmethod1", "http://scaddress.com", "screcipient", "",
            currentDateTime(-1), currentDateTime(1), "", "sctype", "scdata");
        writer.AddSubjectConfirmation("scmethod2", "Sandford, Gloucestershire", "screcipient", "",
            currentDateTime(-1), currentDateTime(3), "", "sctype", "scdata");

        // applying security
        applySecurity();

        // Saving the output
        writer.SaveFile(output);
    }

    private static void createAssertion()
    {
        // creating a message of Assertion type
        writer.CreateNew((int)SAMLContentTypes.cstyAssertion);

        // main message properties
        writer.Message.ID = "my-message-id-123";
        writer.Message.IssueInstant = currentDateTime(0);
        writer.Message.Destination = dest;
        writer.Message.Issuer = writer.FormatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", "");

        // a message may contain multiple complex assertions, each of which
        // should be enveloped into BeginAssertion/CompleteAssertion calls.
        writer.BeginAssertion();
        writer.Assertion.Issuer = writer.Message.Issuer; // keeping it simple
        writer.Assertion.AssertionType = SAMLAssertionTypes.csatAssertion;

        // An assertion may contain multiple attributes within multiple statements.
        // If we add an attribute without adding a statement first, a new statement
        // will be created automatically, and the attribute added to it.
        // For example, the below two attributes go to the first statement,
        // which is created automatically:
        writer.AddAttribute("clientID", "JohnDoe123", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
            "xs:string", 0);
        writer.AddAttribute("clientCategorty", "Business", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
            "xs:string", 0);

        // Let"s add another statement
        writer.AddAttributeStatement(); // returns the index, which is going to be 1

        // Adding one attribute with two values - these are going to be merged
        // because their names are identical.
        writer.AddAttribute("eduPersonAffiliation", "users", "", "", 1);
        writer.AddAttribute("eduPersonAffiliation", "examplerole1", "", "", 1);

        // adding an authentication statement
        writer.AddAuthnStatement(currentDateTime(0), "id-1234567890",
            currentDateTime(1), "urn:oasis:names:tc:SAML:2.0:ac:classes:Password");

        // adding an authorization statement
        writer.AddAuthzDecisionStatement((int)SAMLDecisions.csadnPermit, "Evidence", "Resource", "namespace=ns1;value=value1");

        // adding assertion conditions (audience and time scope)
        writer.AddCondition((int)SAMLConditionTypes.csctAudienceRestriction, "PSCU:saml20:dev");
        writer.AddCondition((int)SAMLConditionTypes.csctNotOnOrAfter, currentDateTime(1));

        // setting custom assertion ID (optional)
        writer.Assertion.ID = "unique-id-123456";

        // adding subject confirmations
        writer.Assertion.Subject = writer.FormatID("Subject", "Subject", "", "", "", "");
        writer.AddSubjectConfirmation("scmethod1", "http://scaddress.com", "screcipient", "",
            currentDateTime(-1), currentDateTime(1), "", "sctype", "scdata");

        // applying security: as it applies to the assertion, it should precede
        // the CompleteAssertion() call
        applySecurity();

        // adding ("committing") the formed assertion to the SAML message
        writer.CompleteAssertion();

        // Saving the output
        writer.SaveFile(output);
    }

    private static void createEnvelopedAssertion()
    {
        // creating a message of Response type
        writer.CreateNew((int)SAMLContentTypes.cstyResponse);

        // main message properties
        writer.Message.ID = "my-message-id-123";
        writer.Message.IssueInstant = currentDateTime(0);
        writer.Message.Destination = dest;
        writer.Message.Issuer = writer.FormatID(issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", "");

        // a message may contain multiple complex assertions, each of which
        // should be enveloped into BeginAssertion/CompleteAssertion calls.
        writer.BeginAssertion();
        writer.Assertion.Issuer = writer.Message.Issuer; // keeping it simple
        writer.Assertion.AssertionType = SAMLAssertionTypes.csatAssertion;

        // An assertion may contain multiple attributes within multiple statements.
        // If we add an attribute without adding a statement first, a new statement
        // will be created automatically, and the attribute added to it.
        // For example, the below two attributes go to the first statement,
        // which is created automatically:
        writer.AddAttribute("clientID", "JohnDoe123", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
            "xs:string", 0);
        writer.AddAttribute("clientCategorty", "Business", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic",
            "xs:string", 0);

        // adding an authentication statement
        writer.AddAuthnStatement(currentDateTime(0), "id-1234567890",
            currentDateTime(1), "urn:oasis:names:tc:SAML:2.0:ac:classes:Password");

        // adding assertion conditions (audience and time scope)
        writer.AddCondition((int)SAMLConditionTypes.csctAudienceRestriction, "PSCU:saml20:dev");
        writer.AddCondition((int)SAMLConditionTypes.csctNotOnOrAfter, currentDateTime(1));

        // setting custom assertion ID (optional)
        writer.Assertion.ID = "unique-id-123456";

        // adding subject confirmations
        writer.Assertion.Subject = writer.FormatID("Subject", "Subject", "", "", "", "");
        writer.AddSubjectConfirmation("scmethod1", "http://scaddress.com", "screcipient", "",
            currentDateTime(-1), currentDateTime(1), "", "sctype", "scdata");

        // applying security: as it applies to the assertion, it should precede
        // the CompleteAssertion() call
        applySecurity();

        // adding ("committing") the formed assertion to the SAML message
        writer.CompleteAssertion();

        // Saving the output
        writer.SaveFile(output);
    }

    static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            displayHelp("");
            return;
        }

        writer = new SAMLWriter();

        int type = -1;
        if (optext(args, "-type"))
        {
            try
            {
                type = int.Parse(optval(args, "-type"));
            }
            catch (Exception ex)
            {
                displayHelp("-type invalid value.");
                return;
            }
        }
        else
        {
            displayHelp("-type is required.");
            return;
        }

        if (optext(args, "-output"))
        {
            output = optval(args, "-output");
        }
        else
        {
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

        if (optext(args, "-hashalg"))
        {
            hashalg = optval(args, "-hashalg");
        }

        try
        {
            switch (type)
            {
                case 0:
                    createAuthnRequest();
                    break;
                case 1:
                    createLogoutRequest();
                    break;
                case 2:
                    createAttributeQuery();
                    break;
                case 3:
                    createSubjectQuery();
                    break;
                case 4:
                    createAssertion();
                    break;
                case 5:
                    createEnvelopedAssertion();
                    break;
                default:
                    displayHelp("Invalid type value.");
                    confirmExit();
                    return;
            }

            Console.WriteLine("The SAML message was created successfully.\n");

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