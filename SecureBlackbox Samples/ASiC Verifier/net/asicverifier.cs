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

class asicverifier
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
                "  asicverifier -- SecureBlackbox ASiCVerifier Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  asicverifier <-input input_file> [-extractpath extract_path]\n\n" +
                "DESCRIPTION\n" +
                "  ASiCVerifier demonstrates the usage of ASiCVerifier from SecureBlackbox.\n" +
                "  Used to verify the signature of and optionally extract any files in an Associated Signature Container (ASic).\n\n" +
                "  The options are as follows:\n\n" +
                "  -input         The ASiC to verify (Required).\n\n" +
                "  -extractpath   The path to extract files to. If unspecified, files will not be extracted.\n\n" +
                "EXAMPLES\n" +
                "  asicverifier -input C:\\asic\\myasic.scs\n"
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

    private static String translateSigType(ASiCSignatureTypes type)
    {
        switch (type)
        {
            case ASiCSignatureTypes.castCAdES:
                return "CAdES";
            case ASiCSignatureTypes.castXAdES:
                return "XAdES";
            case ASiCSignatureTypes.castTimestamp:
                return "Timestamp";
            default:
                return "Unknown";
        }
    }

    private static String translateValidationResult(SignatureValidities res)
    {
        switch (res)
        {
            case SignatureValidities.svtValid:
                return "The signature is valid.";
            case SignatureValidities.svtUnknown:
                return "Signature validity is unknown.";
            case SignatureValidities.svtCorrupted:
                return "The signature is corrupted.";
            case SignatureValidities.svtSignerNotFound:
                return "Failed to acquire the signing certificate. The signature cannot be validated.";
            case SignatureValidities.svtFailure:
                return "General failure.";
            default:
                return "Signature validity is unknown.";
        }
    }

    static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            displayHelp("");
            return;
        }

        ASiCVerifier verifier = new ASiCVerifier();

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

            if (optext(args, "-extractpath"))
            {
                verifier.ExtractionMode = ASiCVerifierExtractionModes.aemAll;
                verifier.OutputPath = optval(args, "-extractpath");
            }

            verifier.Verify();

            Console.WriteLine("There are " + verifier.Signatures.Count + " signatures in this file.");
            for (int i = 0; i < verifier.Signatures.Count; i++)
            {
                Console.WriteLine("Signature #" + (i + 1));
                Console.WriteLine("  SignatureType: " + translateSigType(verifier.Signatures[i].SignatureType));
                Console.WriteLine("  File(s): " + verifier.Signatures[i].SignedFiles);

                Console.WriteLine("  Validation Result: " + verifier.Signatures[i].SignatureValidationResult + ", " + translateValidationResult(verifier.Signatures[i].SignatureValidationResult));
                Console.WriteLine("  Chain Result: " + verifier.Signatures[i].ChainValidationResult + "\n");
            }
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