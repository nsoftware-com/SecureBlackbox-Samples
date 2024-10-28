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

class cadesverifier
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
            "  cadesverifier -- SecureBlackbox CAdESVerifier Demo Application\n\n" +
            "SYNOPSIS\n" +
            "  cadesverifier <-input input_file> [-output output_file] [-data data_file] [-checkrev] [-ignoreerrors] [-offline]\n\n" +
            "DESCRIPTION\n" +
            "  This sample shows processing of CAdES signatures. \n\n" +
            "  The options are as follows:\n\n" +
            "  -input         An input file to verify (Required).\n\n" +
            "  -output        Where to save the verified, unpacked message.\n\n" +
            "  -data          The payload to be validated (for detached signatures).\n\n" +
            "  -checkrev      Whether certificate revocation information should be checked.\n\n" +
            "  -ignoreerrors  Whether to ignore chain validation errors.\n\n" +
            "  -offline       Whether offline mode be used.\n\n" +
            "EXAMPLES\n" +
            "  cadesverifier -input C:\\myexe.scs -output C:\\helloworld.exe -offline\n\n" +
            "  cadesverifier -input C:\\mydll.scs -data C:\\helloworld.dll -checkrev -ignoreerrors\n"
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

    private static void printSignerCertificate(int idx, CAdESVerifier verifier)
    {
        Console.WriteLine("Certificate " + verifier.Certificates[idx].Subject);
        Console.WriteLine("  Valid:            from " + verifier.Certificates[idx].ValidFrom + " to " + verifier.Certificates[idx].ValidTo);
        Console.WriteLine("  Self-signed:            " + verifier.Certificates[idx].SelfSigned);
        Console.WriteLine("  Subject RDN:                " + verifier.Certificates[idx].SubjectRDN);
    }

    static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            displayHelp("");
            return;
        }

        CAdESVerifier verifier = new CAdESVerifier();

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

            if (optext(args, "-output"))
            {
                verifier.Detached = false;
                verifier.OutputFile = optval(args, "-output");
            }
            else if (optext(args, "-data"))
            {
                verifier.Detached = true;
                verifier.DataFile = optval(args, "-data");
            }
            else
            {
                displayHelp("-output or -data is required.");
                return;
            }

            if (optext(args, "-checkrev"))
            {
                verifier.RevocationCheck = CAdESVerifierRevocationChecks.crcAuto;
            }
            else
            {
                verifier.RevocationCheck = CAdESVerifierRevocationChecks.crcNone;
            }

            if (optext(args, "-ignoreerrors"))
            {
                verifier.IgnoreChainValidationErrors = true;
            }
            else
            {
                verifier.IgnoreChainValidationErrors = false;
            }

            if (optext(args, "-offline"))
            {
                verifier.OfflineMode = true;
            }
            else
            {
                verifier.OfflineMode = false;
            }

            verifier.Verify();

            switch (verifier.Signatures[0].SignatureValidationResult)
            {
                case SignatureValidities.svtValid:
                    Console.WriteLine("The signature is valid.");
                    break;
                case SignatureValidities.svtUnknown:
                    Console.WriteLine("Signature verification failed: Unknown signature.");
                    break;
                case SignatureValidities.svtFailure:
                case SignatureValidities.svtCorrupted:
                    Console.WriteLine("Signature verification failed: The signature is corrupt or invalid.");
                    break;
                case SignatureValidities.svtSignerNotFound:
                    Console.WriteLine("Signature verification failed: The signature does not contain a signer.");
                    break;
                default:
                    Console.WriteLine("Signature verification failed.");
                    break;
            }
            Console.WriteLine();

            for (int i = 0; i < verifier.Certificates.Count; i++)
            {
                printSignerCertificate(i, verifier);
            }
            Console.WriteLine();

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