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

class jadesverifier
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
                "  jadesverifier -- SecureBlackbox JAdESVerifier Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  jadesverifier <-input input_file> [-data data_file] [-checkrev] [-ignoreerrors] [-offline]\n\n" +
                "DESCRIPTION\n" +
                "  This sample illustrates the use of JAdESVerifier component for validating JAdES signatures.\n\n" +
                "  The options are as follows:\n\n" +
                "  -input         An input file to verify (Required).\n\n" +
                "  -data          The payload to be validated (for detached signatures).\n\n" +
                "  -checkrev      Whether certificate revocation information should be checked.\n\n" +
                "  -ignoreerrors  Whether to ignore chain validation errors.\n\n" +
                "  -offline       Whether offline mode be used.\n\n" +
                "EXAMPLES\n" +
                "  jadesverifier -input C:\\mydll.scs -data C:\\helloworld.dll -checkrev -ignoreerrors\n"
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

    private static void printJAdESSignature(int idx, JAdESVerifier verifier)
    {
        Console.WriteLine("Signature " + (idx + 1));
        Console.WriteLine("  Timestamp:         " + verifier.Signatures[idx].ValidatedSigningTime);
        Console.Write("  Validation Result: " + verifier.Signatures[idx].SignatureValidationResult + ", ");

        switch (verifier.Signatures[idx].SignatureValidationResult)
        {
            case SignatureValidities.svtValid:
                Console.WriteLine("The signature is valid.");
                break;
            case SignatureValidities.svtCorrupted:
                Console.WriteLine("The signature is corrupted.");
                break;
            case SignatureValidities.svtSignerNotFound:
                Console.WriteLine("Failed to acquire the signing certificate. The signature cannot be validated.");
                break;
            case SignatureValidities.svtFailure:
                Console.WriteLine("General failure.");
                break;
            default:
                Console.WriteLine("Signature validity is unknown.");
                break;
        }

        Console.WriteLine("  Chain Result:      " + verifier.Signatures[idx].ChainValidationResult);
        Console.WriteLine();
    }

    static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            displayHelp("");
            return;
        }

        JAdESVerifier verifier = new JAdESVerifier();

        try
        {
            if (optext(args, "-input"))
            {
                verifier.InputFile = optval(args, "-input");
            }
            else
            {
                displayHelp("-input is required.");
                return;
            }

            if (optext(args, "-data"))
            {
                verifier.DataFile = optval(args, "-data");
            }

            if (optext(args, "-checkrev"))
            {
                verifier.RevocationCheck = JAdESVerifierRevocationChecks.crcAuto;
            }
            else
            {
                verifier.RevocationCheck = JAdESVerifierRevocationChecks.crcNone;
            }

            verifier.IgnoreChainValidationErrors = optext(args, "-ignoreerrors");
            verifier.OfflineMode = optext(args, "-offline");

            verifier.Verify();

            for (int i = 0; i < verifier.Signatures.Count; i++)
            {
                printJAdESSignature(i, verifier);
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