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

class jwencryption
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
                        "  jwencryption -- SecureBlackbox SymmetricCrypto Demo Application\n\n" +
                        "SYNOPSIS\n" +
                        "  jwencryption <-e/-d> <-input input_data> <-pass encryption_password> [-compact] [-encalg encryption_algorithm]\n\n" +
                        "DESCRIPTION\n" +
                        "  This sample illustrates how to encrypt text to a JW token with a password.\n" +
                        "  Used to encrypt and decrypt data.\n\n" +
                        "  The options are as follows:\n\n" +
                        "  -e            Whether to encrypt input data. \n\n" +
                        "  -d            Whether to decrypt input data. \n\n" +
                        "  -input        An input data to encrypt/decrypt (Required). \n\n" +
                        "  -pass         Password for encryption (Required). \n\n" +
                        "  -compact      Whether to use compact format \n\n" +
                        "  -encalg       The encryption algorithm to use. Valid values: RC2, RC4, DES, 3DES, AES128, AES192, AES256, Blowfish \n\n" +
                        "EXAMPLES\n" +
                        "	jwencryption -e -input \"And now that you donâ€™t have to be perfect, you can be good.\" -pass mypassword -encalg AES256 \n\n" +
                        "	jwencryption -d -input eyJhbGciOiJkaXIiLCJlbmMiOiJBMjU2Q0JDLUhTNTEyIn0..kuN2U -pass mypassword -compact \n"
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
            byte[] inputB;
            String pass = "";
            bool compact = false;

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
                inputB = Encoding.Default.GetBytes(optval(args, "-input"));
            }
            else
            {
                displayHelp("-input is required.");
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

            if (optext(args, "-encalg"))
            {
                crypto.EncryptionAlgorithm = optval(args, "-encalg");
            }

            if (optext(args, "-compact"))
            {
                compact = true;
            }

            // genarate key from password
            CryptoKeyManager keymanager = new CryptoKeyManager();
            keymanager.DeriveKey(256, pass, "");
            keymanager.Key.IV = new byte[16];
            crypto.Key = keymanager.Key;

            if (encrypt)
            {
                if (compact)
                {
                    crypto.OutputEncoding = SymmetricCryptoOutputEncodings.cetCompact;
                }
                else
                {
                    crypto.OutputEncoding = SymmetricCryptoOutputEncodings.cetJSON;
                }

                byte[] outputB = crypto.Encrypt(inputB);

                Console.WriteLine("Encrypted token: " + Encoding.Default.GetString(outputB));
            }
            else
            {
                if (compact)
                {
                    crypto.InputEncoding = SymmetricCryptoInputEncodings.cetCompact;
                }
                else
                {
                    crypto.InputEncoding = SymmetricCryptoInputEncodings.cetJSON;
                }

                byte[] outputB = crypto.Decrypt(inputB);

                Console.WriteLine("Decrypted string: " + Encoding.Default.GetString(outputB));
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