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

class messagetimestampverifier
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
                "  messagetimestampverifier -- SecureBlackbox MessageTimestampVerifier Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  messagetimestampverifier <-input input_file> [-output output_file] [-data data_file] \n\n" +
                "DESCRIPTION\n" +
                "  MessageTimestampVerifier demonstrates the usage of MessageTimestampVerifier from SecureBlackbox.\n" +
                "  Used to facilities in validating PKCS#7-compliant timestamped files. \n\n" +
                "  The options are as follows:\n\n" +
                "  -input        A timestamped file (Required). \n\n" +
                "  -output       Where to save the verified, unpacked message (Required for non detached timestamped file).\n\n" +
                "  -data         The original data (Required for detached signature).\n\n" +
                "EXAMPLES\n" +
                "  messagetimestampverifier -input C:\\pkcs7\\mymes.pkcs7 -output C:\\pkcs7\\helloworld.txt \n\n" +
                "  messagetimestampverifier -input C:\\pkcs7\\mymes.pkcs7 -data C:\\pkcs7\\helloworld.txt \n"
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

    static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            displayHelp("");
            return;
        }

        MessageTimestampVerifier verifier = new MessageTimestampVerifier();

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

            bool detached = false;
            if (optext(args, "-output"))
            {
                verifier.OutputFile = optval(args, "-output");
            }
            else if (optext(args, "-data"))
            {
                verifier.DataFile = optval(args, "-data");
                detached = true;
            }
            else
            {
                Console.WriteLine();
                displayHelp("-output or -datafile is required.");
                return;
            }

            // Verify
            if (detached)
            {
                verifier.VerifyDetached();
            }
            else
            {
                verifier.Verify();
            }

            switch (verifier.SignatureValidationResult)
            {
                case MessageTimestampVerifierSignatureValidationResults.svtValid:
                    Console.WriteLine("The signature is valid.");
                    break;
                case MessageTimestampVerifierSignatureValidationResults.svtUnknown:
                    Console.WriteLine("Signature verification failed: Unknown signature.");
                    break;
                case MessageTimestampVerifierSignatureValidationResults.svtCorrupted:
                    Console.WriteLine("Signature verification failed: The signature is corrupt or invalid.");
                    break;
                case MessageTimestampVerifierSignatureValidationResults.svtSignerNotFound:
                    Console.WriteLine("Signature verification failed: The signature does not contain a signer.");
                    break;
                case MessageTimestampVerifierSignatureValidationResults.svtFailure:
                    Console.WriteLine("Signature verification failed.");
                    break;
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