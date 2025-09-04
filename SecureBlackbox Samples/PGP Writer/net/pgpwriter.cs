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
using System.Reflection.PortableExecutable;
using nsoftware.SecureBlackbox;

class pgpwriter
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
                "  pgpwriter -- SecureBlackbox PGPWriter Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  pgpwriter <-input input_file> <-output output_file> <-pubkey public_key_file> <-seckey secret_key_file>\n" +
                "            <-keypass keys_password> <-pass encryption_password> \n\n" +
                "DESCRIPTION\n" +
                "  PGPWriter demonstrates the usage of PGPWriter from SecureBlackbox.\n" +
                "  Used to create encrypted and signed OpenPGP-compliant files. \n\n" +
                "  The options are as follows:\n\n" +
                "  -input        An input file to sign (Required).\n\n" +
                "  -output       Where the ASiC will be saved (Required).\n\n" +
                "  -pubkey       The public key used to encrypt file (Required).\n\n" +
                "  -seckey       The secret (private) key used to sign file (Required).\n\n" +
                "  -keypass      The password for the keys (Required).\n\n" +
                "  -pass         The password for encryption (Required).\n"
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

    private static string keypass = "";
    private static PGPWriter writer;

    private static void _writer_OnKeyPassphraseNeeded(object sender, PGPWriterKeyPassphraseNeededEventArgs e)
    {
        e.Passphrase = keypass;
    }

    static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            displayHelp("");
            return;
        }

        writer = new PGPWriter();
        PGPKeyring keyring = new PGPKeyring();

        try
        {
            writer.OnKeyPassphraseNeeded += _writer_OnKeyPassphraseNeeded;


            if (optext(args, "-input"))
            {
                writer.InputFile = optval(args, "-input");
            }
            else
            {
                displayHelp("-input is required.");
                return;
            }

            if (optext(args, "-output"))
            {
                writer.OutputFile = optval(args, "-output");
            }
            else
            {
                displayHelp("-output is required.");
                return;
            }

            if (optext(args, "-pass"))
            {
                writer.Passphrase = optval(args, "-pass");
            }
            else
            {
                displayHelp("-pass is required.");
                return;
            }

            if (optext(args, "-keypass"))
            {
                keypass = optval(args, "-keypass");
            }
            else
            {
                displayHelp("-keypass is required.");
                return;
            }

            if (optext(args, "-pubkey"))
            {
                keyring.ImportFromFile(optval(args, "-pubkey"));
            }
            else
            {
                displayHelp("-pubkey is required.");
                return;
            }

            if (optext(args, "-seckey"))
            {
                keyring.ImportFromFile(optval(args, "-seckey"));
            }
            else
            {
                displayHelp("-seckey is required.");
                return;
            }

            for (int i = 0; i < keyring.Keys.Count; i++)
            {
                if (keyring.Keys[i].IsSecret)
                {
                    writer.SigningKeys.Add(keyring.Keys[i]);
                    break;
                }
            }

            for (int i = 0; i < keyring.Keys.Count; i++)
            {
                if (keyring.Keys[i].IsPublic)
                {
                    writer.EncryptingKeys.Add(keyring.Keys[i]);
                    break;
                }
            }

            writer.EncryptAndSign();

            Console.WriteLine("The file were encrypted and signed successfully.\n");

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