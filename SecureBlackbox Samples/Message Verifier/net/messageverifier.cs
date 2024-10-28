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

class messageverifier
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
                "  messageverifier -- SecureBlackbox MessageVerifier Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  messageverifier <-input input_file> [-output output_file] [-data data_file]\n" +
                "            [-cert certificate_file] [-certpass certificate_password]\n\n" +
                "DESCRIPTION\n" +
                "  MessageVerifier demonstrates the usage of MessageVerifier from SecureBlackbox.\n" +
                "  Used to validating PKCS#7-compliant signed files.\n\n" +
                "  The options are as follows:\n\n" +
                "  -input        A signature to verify (Required). If the signature is detached, this will take\n" +
                "                the signature file and -datafile will take the original data.\n\n" +
                "  -output       Where to save the verified, unpacked message (Required for non detached signature).\n\n" +
                "  -data         The original data (Required for detached signature).\n\n" +
                "  -cert         The certificate used to sign files.\n\n" +
                "  -certpass     The password for the signing certificate.\n\n" +
                "EXAMPLES\n" +
                "  messageverifier -input C:\\mes\\mymes.scs -detached -datafile C:\\mes\\helloworld.txt -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n" +
                "  messageverifier -input C:\\mes\\mymes.scs -output C:\\mes\\helloworld.txt\n"
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

        MessageVerifier verifier = new MessageVerifier();
        CertificateManager cm = new CertificateManager();

        try
        {
            if (optext(args, "-input"))
            {
                verifier.InputFile = optval(args, "-input");
            }
            else
            {
                Console.WriteLine();
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
                displayHelp("-output -data is required.");
                return;
            }

            if (optext(args, "-cert"))
            {
                cm.ImportFromFile(optval(args, "-cert"), optval(args, "-certpass"));
                verifier.KnownCertificates.Add(cm.Certificate);
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
                case MessageVerifierSignatureValidationResults.svtValid:
                    Console.WriteLine("The signature is valid.\nHash algorithm: " + verifier.HashAlgorithm);
                    break;
                case MessageVerifierSignatureValidationResults.svtUnknown:
                    Console.WriteLine("Signature verification failed: Unknown signature.");
                    break;
                case MessageVerifierSignatureValidationResults.svtCorrupted:
                    Console.WriteLine("Signature verification failed: The signature is corrupt or invalid.");
                    break;
                case MessageVerifierSignatureValidationResults.svtSignerNotFound:
                    Console.WriteLine("Signature verification failed: The signature does not contain a signer.");
                    break;
                case MessageVerifierSignatureValidationResults.svtFailure:
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