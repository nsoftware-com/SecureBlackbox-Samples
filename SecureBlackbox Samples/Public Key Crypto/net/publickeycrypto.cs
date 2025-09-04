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

class publickeycrypto
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
                "  publickeycrypto -- SecureBlackbox PublicKeyCrypto Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  publickeycrypto <-s/-v> <-input input_file> <-cert certificate_file> [-certpass certificate_password] \n" +
                "             [-output output_file] [-sig signature_file] [-encoding encoding]\n\n" +
                "DESCRIPTION\n" +
                "  PublicKeyCrypto demonstrates the usage of PublicKeyCrypto from SecureBlackbox.\n" +
                "  Used to sign and verify files.\n\n" +
                "  The options are as follows:\n\n" +
                "  -s            Sign input file and save to output \n\n" +
                "  -v            Verify signature using original file (input) \n\n" +
                "  -input        An input file to sign or verify (Required). \n\n" +
                "  -cert         The certificate used to sign files (Required).\n\n" +
                "  -certpass     The password for the signing certificate.\n\n" +
                "  -output       Where the signed file will be saved (Required for signing). \n\n" +
                "  -sig          An signature file to verify (Required on verifing). \n\n" +
                "  -encoding     The encoding of hash. Valid values:\n\n" +
                "                  0 - CET_DEFAULT\n" +
                "                  1 - CET_BINARY\n" +
                "                  2 - CET_BASE_64\n" +
                "                  3 - CET_COMPACT\n" +
                "                  4 - CET_JSON\n\n" +
                "EXAMPLES\n" +
                "  publickeycrypto -s -input C:\\cypto\\helloworld.txt -output C:\\cypto\\signature.dat -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n" +
                "  publickeycrypto -v -input C:\\cypto\\helloworld.txt -signature C:\\cypto\\signature.dat -cert C:\\certs\\mycert.pfx -certpass mypassword -encoding 2 \n"
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

        PublicKeyCrypto crypto = new PublicKeyCrypto();
        try
        {
            bool sign = false;
            bool verify = false;
            String input;

            if (optext(args, "-s"))
            {
                sign = true;
            }

            if (optext(args, "-v"))
            {
                verify = true;
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

            if (optext(args, "-input"))
            {
                input = optval(args, "-input");
            }
            else
            {
                displayHelp("-input is required.");
                return;
            }

            if (optext(args, "-cert"))
            {
                CertificateManager cm = new CertificateManager();
                CryptoKeyManager ckm = new CryptoKeyManager();

                cm.ImportFromFile(optval(args, "-cert"), optval(args, "-certpass"));

                ckm.Certificate = cm.Certificate;
                ckm.ImportFromCert();

                crypto.Key = ckm.Key;
            }
            else
            {
                displayHelp("-cert is required.");
                return;
            }

            if (sign)
            {
                String output;
                if (optext(args, "-output"))
                {
                    output = optval(args, "-output");
                }
                else
                {
                    displayHelp("-output is required.");
                    return;
                }

                if (optext(args, "-encoding"))
                {
                    crypto.OutputEncoding = (PublicKeyCryptoOutputEncodings)int.Parse(optval(args, "-encoding"));
                }
                else
                {
                    crypto.OutputEncoding = PublicKeyCryptoOutputEncodings.cetBase64;
                }

                crypto.SignFile(input, output, true);

                Console.WriteLine("The file was signed successfully.");
            }
            else
            {
                String signature;
                if (optext(args, "-sig"))
                {
                    signature = optval(args, "-sig");
                }
                else
                {
                    displayHelp("-sig is required.");
                    return;
                }


                if (optext(args, "-encoding"))
                {
                    crypto.InputEncoding = (PublicKeyCryptoInputEncodings)int.Parse(optval(args, "-encoding"));
                }
                else
                {
                    crypto.InputEncoding = PublicKeyCryptoInputEncodings.cetBase64;
                }

                crypto.VerifyDetachedFile(input, signature);

                switch (crypto.SignatureValidationResult)
                {
                    case PublicKeyCryptoSignatureValidationResults.svtValid:
                        Console.WriteLine("Verification succeeded.\n");
                        break;
                    case PublicKeyCryptoSignatureValidationResults.svtCorrupted:
                        Console.WriteLine("Verification corrupted.\n");
                        break;
                    case PublicKeyCryptoSignatureValidationResults.svtSignerNotFound:
                        Console.WriteLine("Signer not found.\n");
                        break;
                    case PublicKeyCryptoSignatureValidationResults.svtFailure:
                        Console.WriteLine("Verification failed.\n");
                        break;
                    default:
                        Console.WriteLine("Verification unknown.\n");
                        break;
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