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
using System.Text;
using nsoftware.SecureBlackbox;

class jwsigner
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
                "  jwsigner -- SecureBlackbox SymmetricCrypto Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  jwsigner <-s/-v> <-input input_data> <-cert certificate_file> [-certpass certificate_password] [-sig signature_data] [-compact]\n\n" +
                "DESCRIPTION\n" +
                "  This sample illustrates how to create a detached signature over a text string.\n" +
                "  Used to sign and verify data.\n\n" +
                "  The options are as follows:\n\n" +
                "  -s            Whether to sign input data. \n\n" +
                "  -v            Whether to verify signature data. \n\n" +
                "  -input        An input data to sign/verify (Required). \n\n" +
                "  -cert         The certificate used to encrypt file (Required). \n\n" +
                "  -certpass     The password for the certificate. \n\n" +
                "  -sig          An signature data to verify (Required to verify). \n\n" +
                "  -compact      Whether to use compact format \n\n" +
                "EXAMPLES\n" +
                "	jwsigner -s -input \"And now that you don't have to be perfect, you can be good.\" -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n" +
                "	jwsigner -v -input \"And now that you don't have to be perfect, you can be good.\" \n" +
                "		-sig eyJhbGciOiJkaXIiLCJlbmMiOiJBMjU2Q0JDLUhTNTEyIn0..kuN2U -cert C:\\certs\\mycert.pfx -certpass mypassword -compact \n"
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

        try
        {
            PublicKeyCrypto crypto = new PublicKeyCrypto();
            bool sign = false;
            bool verify = false;
            String input = "";
            String sig = "";
            String certfile = "";
            String certpass = "";
            bool compact = false;

            for (int i = 0; i < args.Length; i++)
            {
                if (args[i].StartsWith("-"))
                {
                    if (args[i].Equals("-s", StringComparison.CurrentCultureIgnoreCase))
                        sign = true;
                    if (args[i].Equals("-v", StringComparison.CurrentCultureIgnoreCase))
                        verify = true;
                    if (args[i].Equals("-input", StringComparison.CurrentCultureIgnoreCase))
                        input = args[i + 1]; // args[i+1] corresponds to the value of argument [i]
                    if (args[i].Equals("-sig", StringComparison.CurrentCultureIgnoreCase))
                        sig = args[i + 1];
                    if (args[i].Equals("-cert", StringComparison.CurrentCultureIgnoreCase))
                        certfile = args[i + 1];
                    if (args[i].Equals("-certpass", StringComparison.CurrentCultureIgnoreCase))
                        certpass = args[i + 1];
                    if (args[i].Equals("-compact", StringComparison.CurrentCultureIgnoreCase))
                        compact = true;
                }
            }

            if (!(sign || verify))
            {
                displayHelp("-s or -v is required.");
                return;
            }

            if (sign && verify)
            {
                displayHelp("Use only one -s or -v parameter.");
                return;
            }

            if (input.Length == 0)
            {
                displayHelp("-input is required.");
                return;
            }

            if (verify && sig.Length == 0)
            {
                displayHelp("-sig is required.");
                return;
            }

            if (certfile.Length == 0)
            {
                displayHelp("-cert is required.");
                return;
            }

            byte[] inputB = Encoding.Default.GetBytes(input);

            // load key from certificate file
            CryptoKeyManager keymanager = new CryptoKeyManager();
            CertificateManager certmanager = new CertificateManager();
            certmanager.ImportFromFile(certfile, certpass);
            keymanager.Certificate = certmanager.Certificate;
            keymanager.ImportFromCert();
            crypto.Key = keymanager.Key;

            if (sign)
            {
                if (compact)
                {
                    crypto.OutputEncoding = PublicKeyCryptoOutputEncodings.cetCompact;
                }
                else
                {
                    crypto.OutputEncoding = PublicKeyCryptoOutputEncodings.cetJSON;
                }

                byte[] outputB = crypto.Sign(inputB, true);

                Console.WriteLine("Signature: " + Encoding.Default.GetString(outputB));
            }
            else
            {
                if (compact)
                {
                    crypto.InputEncoding = PublicKeyCryptoInputEncodings.cetCompact;
                }
                else
                {
                    crypto.InputEncoding = PublicKeyCryptoInputEncodings.cetJSON;
                }

                byte[] sigB = Encoding.Default.GetBytes(sig);

                crypto.VerifyDetached(inputB, sigB);

                switch (crypto.SignatureValidationResult)
                {
                    case PublicKeyCryptoSignatureValidationResults.svtValid:
                        Console.WriteLine("Verification succeeded");
                        break;
                    case PublicKeyCryptoSignatureValidationResults.svtCorrupted:
                        Console.WriteLine("Verification corrupted");
                        break;
                    case PublicKeyCryptoSignatureValidationResults.svtFailure:
                        Console.WriteLine("Verification failed");
                        break;
                    default:
                        Console.WriteLine("Verification unknown");
                        break;
                }
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