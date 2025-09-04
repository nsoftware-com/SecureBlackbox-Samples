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

class symmetriccrypto
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
                "  symmetriccrypto -- SecureBlackbox SymmetricCrypto Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  symmetriccrypto <-e/-d> <-input input_file> <-output output_file> <-pass encryption_password> [-encoding encoding]\n\n" +
                "DESCRIPTION\n" +
                "  SymmetricCrypto demonstrates the usage of SymmetricCrypto from SecureBlackbox.\n" +
                "  Used to sign and verify files.\n\n" +
                "  The options are as follows:\n\n" +
                "  -e            Encrypt input file and save to output \n\n" +
                "  -d            Decrypt input file and save to output \n\n" +
                "  -input        An input file to encrypt or decrypt (Required). \n\n" +
                "  -output       Where the encrypted or decrypted file will be saved (Required). \n\n" +
                "  -pass         The password for encryption/decryption (Required).\n\n" +
                "  -encoding     The encoding of hash. Valid values:\n\n" +
                "                  0 - CET_DEFAULT\n" +
                "                  1 - CET_BINARY\n" +
                "                  2 - CET_BASE_64\n" +
                "                  3 - CET_COMPACT\n" +
                "                  4 - CET_JSON\n\n" +
                "EXAMPLES\n" +
                "  symmetriccrypto -e -input C:\\cypto\\helloworld.txt -output C:\\cypto\\helloworld.enc -pass mypassword \n\n" +
                "  symmetriccrypto -d -input C:\\cypto\\helloworld.enc -output C:\\cypto\\helloworld.txt -pass mypassword -encoding 2 \n"
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

        SymmetricCrypto crypto = new SymmetricCrypto();
        try
        {
            bool encrypt = false;
            bool decrypt = false;
            String input;
            String output;
            String pass;

            if (optext(args, "-e"))
            {
                encrypt = true;
            }

            if (optext(args, "-d"))
            {
                decrypt = true;
            }

            if (!(encrypt || decrypt))
            {
                displayHelp("-e or -d is required.");
                return;
            }

            if (encrypt && decrypt)
            {
                displayHelp("Use only one -e or -d parameter.");
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

            if (optext(args, "-output"))
            {
                output = optval(args, "-output");
            }
            else
            {
                displayHelp("-output is required.");
                return;
            }

            if (optext(args, "-pass"))
            {
                pass = optval(args, "-pass");
            }
            else
            {
                displayHelp("-pass is required.");
                return;
            }

            crypto.EncryptionAlgorithm = "AES256";

            // genarate key from password
            CryptoKeyManager keymanager = new CryptoKeyManager();
            keymanager.DeriveKey(256, pass, "");
            keymanager.Key.IV = new byte[16];
            crypto.Key = keymanager.Key;

            if (encrypt)
            {
                if (optext(args, "-encoding"))
                {
                    crypto.OutputEncoding = (SymmetricCryptoOutputEncodings)int.Parse(optval(args, "-encoding"));
                }
                else
                {
                    crypto.OutputEncoding = SymmetricCryptoOutputEncodings.cetBase64;
                }

                crypto.EncryptFile(input, output);

                Console.WriteLine("The file was encrypted successfully.\n");
            }
            else
            {
                if (optext(args, "-encoding"))
                {
                    crypto.InputEncoding = (SymmetricCryptoInputEncodings)int.Parse(optval(args, "-encoding"));
                }
                else
                {
                    crypto.InputEncoding = SymmetricCryptoInputEncodings.cetBase64;
                }

                crypto.DecryptFile(input, output);

                Console.WriteLine("The file was decrypted successfully.\n");
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