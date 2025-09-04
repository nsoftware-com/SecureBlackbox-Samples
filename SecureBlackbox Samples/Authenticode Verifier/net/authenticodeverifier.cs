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

class authenticodeverifier
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
                "  authenticodeverifier -- SecureBlackbox AuthenticodeVerifier Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  authenticodeverifier <-input input_file> [-cert certificate_file] [-certpass certificate_password]\n\n" +
                "DESCRIPTION\n" +
                "  AuthenticodeVerifier demonstrates the usage of AuthenticodeVerifier from SecureBlackbox.\n" +
                "  Used to verify the signature.\n\n" +
                "  The options are as follows:\n\n" +
                "  -input        An input file to verify (Required).\n\n" +
                "  -cert         The certificate used to sign files.\n\n" +
                "  -certpass     The password for the signing certificate.\n\n" +
                "EXAMPLES\n" +
                "  authenticodeverifier -input C:\\myexe.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
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

    public static String translateSigValidity(SignatureValidities type)
    {
        switch (type)
        {
            case SignatureValidities.svtValid:
                return "Valid";
            case SignatureValidities.svtCorrupted:
                return "Corrupted";
            case SignatureValidities.svtSignerNotFound:
                return "Signer not found";
            case SignatureValidities.svtFailure:
                return "Failure";
            default:
                return "Unknown";
        }
    }

    static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            displayHelp("");
            return;
        }

        try
        {
            AuthenticodeVerifier verifier = new AuthenticodeVerifier();
            CertificateManager certmanager = new CertificateManager();

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
                certmanager.ImportFromFile(optval(args, "-cert"), optval(args, "-certpass"));
                verifier.KnownCertificates.Add(certmanager.Certificate);
            }

            verifier.Verify();

            if (!verifier.Signed)
            {
                Console.WriteLine("The file is not singed.\n");
            }
            else
            {
                Console.WriteLine("There are " + verifier.Signatures.Count + " signatures in this file.");
                for (int x = 0; x < verifier.Signatures.Count; x++)
                {
                    Console.WriteLine("Signature #" + (x + 1));
                    Console.WriteLine("  Hash algorithm: " + verifier.Signatures[x].HashAlgorithm);
                    Console.WriteLine("  Description: " + verifier.Signatures[x].Description);
                    Console.WriteLine("  URL: " + verifier.Signatures[x].URL);
                    Console.WriteLine("  Validity: " + translateSigValidity(verifier.Signatures[x].SignatureValidationResult));
                    Console.WriteLine("");
                }
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