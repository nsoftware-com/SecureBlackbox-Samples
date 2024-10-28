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
using SBMath;

class pgpreader
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
                "  pgpreader -- SecureBlackbox PGPReader Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  pgpreader <-input input_file> <-output output_file> <-pubkey public_key_file> <-seckey secret_key_file>\n" +
                "            <-keypass keys_password> <-pass encryption_password> \n\n" +
                "DESCRIPTION\n" +
                "  PGPReader demonstrates the usage of PGPReader from SecureBlackbox.\n" +
                "  Used to decrypted and verify OpenPGP-compliant files. \n\n" +
                "  The options are as follows:\n\n" +
                "  -input        An input file to decrypt and verify (Required).\n\n" +
                "  -output       Where the decrypted file will be saved (Required).\n\n" +
                "  -pubkey       The public key used to verify file (Required).\n\n" +
                "  -seckey       The secret (private) key used to decrypt file (Required).\n\n" +
                "  -keypass      The password for the keys (Required).\n\n" +
                "  -pass         The password for decryption (Required).\n"
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

    private static String translateSigValidity(SignatureValidities type)
    {
        switch (type)
        {
            case SignatureValidities.svtValid: return "Valid";
            case SignatureValidities.svtFailure: return "Failure";
            case SignatureValidities.svtCorrupted: return "Corrupted";
            case SignatureValidities.svtReferenceCorrupted: return "Reference Corrupted";
            case SignatureValidities.svtSignerNotFound: return "Signing key not found, unable to verify";
            default: return "Unknown";
        }
    }

    private static string keypass = "";
    private static PGPReader reader;

    private static void _reader_OnKeyPassphraseNeeded(object sender, PGPReaderKeyPassphraseNeededEventArgs e)
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

        reader = new PGPReader();
        PGPKeyring keyring = new PGPKeyring();
        try
        {
            reader.OnKeyPassphraseNeeded += _reader_OnKeyPassphraseNeeded;


            if (optext(args, "-input"))
            {
                reader.InputFile = optval(args, "-input");
            }
            else
            {
                displayHelp("-input is required.");
                return;
            }

            if (optext(args, "-output"))
            {
                reader.OutputFile = optval(args, "-output");
            }
            else
            {
                displayHelp("-output is required.");
                return;
            }

            if (optext(args, "-pass"))
            {
                reader.Passphrase = optval(args, "-pass");
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
                reader.VerifyingKeys.Add(keyring.Keys[i]);
                if (keyring.Keys[i].IsSecret)
                {
                    reader.DecryptingKeys.Add(keyring.Keys[i]);
                }
            }

            reader.DecryptAndVerify();

            Console.WriteLine("Signatures:");
            for (int x = 0; x < reader.Signatures.Count; x++)
            {
                String username = "No name";
                for (int y = 0; y < keyring.Keys.Count; y++)
                {
                    if (keyring.Keys[y].IsPublic && !keyring.Keys[y].IsSubkey &&
                            keyring.Keys[y].KeyID.Equals(reader.Signatures[x].SignerKeyID))
                    {
                        username = keyring.Keys[y].Username;
                        break;
                    }
                }
                Console.WriteLine("	" + username + " - " + translateSigValidity(reader.Signatures[x].Validity));
            }

            Console.WriteLine("The file were decrypted successfully.\n");

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