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
using SBMath;

class soapverifier
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
        for (int x = 0; x < args.Length; x++)
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
                "  soapverifier -- SecureBlackbox SOAPVerifier Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  soapverifier <-input input_file> [-cert certificate_file] [-certpass certificate_password] [-showrefs]\n" +
                "DESCRIPTION\n" +
                "  SOAPVerifier demonstrates the usage of SOAPVerifier from SecureBlackbox.\n" +
                "  Used to verify the signature.\n\n" +
                "  The options are as follows:\n\n" +
                "  -input        An input file to verify (Required).\n\n" +
                "  -cert         The certificate used to sign files.\n\n" +
                "  -certpass     The password for the signing certificate.\n\n" +
                "  -showrefs     Whether to display detailed results of reference verification.\n"
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

    private static bool showrefs = false;
    private static SOAPVerifier verifier;

    private static void _verifier_OnReferenceValidated(object sender, SOAPVerifierReferenceValidatedEventArgs e)
    {
        if (showrefs)
        {
            String valid = "false";
            if (e.DigestValid)
                valid = "true";
            Console.WriteLine(e.ID + "	" + e.URI + "	" + e.RefType + "	" + valid);
        }
    }

    static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            displayHelp("");
            return;
        }

        SOAPVerifier verifier = new SOAPVerifier();
        try
        {
            verifier.OnReferenceValidated += _verifier_OnReferenceValidated;

            if (optext(args, "-input"))
            {
                verifier.InputFile = optval(args, "-input");
            }
            else
            {
                displayHelp("-input is required.");
                return;
            }

            if (optext(args, "-cert"))
            {
                CertificateManager cm = new CertificateManager();
                cm.ImportFromFile(optval(args, "-cert"), optval(args, "-certpass"));
                verifier.KnownCertificates.Add(cm.Certificate);
            }

            if (optext(args, "-showrefs"))
            {
                showrefs = true;
            }

            if (showrefs)
            {
                Console.WriteLine("ID	URI	RefType	DigestValid");
            }

            verifier.Verify();

            Console.WriteLine("There are " + verifier.Signatures.Count + " signatures in this file.\n");
            for (int i = 0; i < verifier.Signatures.Count; i++)
            {
                SOAPSignature sig = verifier.Signatures[i];
                Console.WriteLine("Signature " + (i + 1) + "\n");
                Console.WriteLine("  Claimed signing time: " + sig.ClaimedSigningTime + "\n");

                String s = "";
                switch (sig.SignatureValidationResult)
                {
                    case SignatureValidities.svtValid:
                        s = "Valid";
                        break;
                    case SignatureValidities.svtCorrupted:
                        s = "Corrupted";
                        break;
                    case SignatureValidities.svtSignerNotFound:
                        s = "SignerNotFound";
                        break;
                    case SignatureValidities.svtFailure:
                        s = "Failure";
                        break;
                    case SignatureValidities.svtReferenceCorrupted:
                        s = "ReferenceCorrupted";
                        break;
                    default:
                        s = "Unknown";
                        break;
                }
                Console.WriteLine("Signature validation result: " + s + "\n");

                s = "";
                switch (sig.ChainValidationResult)
                {
                    case ChainValidities.cvtValid:
                        s = "Valid";
                        break;
                    case ChainValidities.cvtValidButUntrusted:
                        s = "ValidButUntrusted";
                        break;
                    case ChainValidities.cvtInvalid:
                        s = "Invalid";
                        break;
                    case ChainValidities.cvtCantBeEstablished:
                        s = "CantBeEstablished";
                        break;
                    default:
                        s = "Unknown";
                        break;
                }
                Console.WriteLine("Chain Validation Result: " + s + "\n");
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