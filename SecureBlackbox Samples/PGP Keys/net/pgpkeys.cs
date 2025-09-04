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
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using nsoftware.SecureBlackbox;

class pgpkeys
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
            "  pgpkeys -- SecureBlackbox PGPKeys Demo Application\n\n" +
            "SYNOPSIS\n" +
            "  pgpkeys <-new/-open> [-sec secret_keys] [-pub public_keys]\n\n" +
            "DESCRIPTION\n" +
            "  This sample is a simple OpenPGP keyring manager. \n\n" +
            "  The options are as follows:\n\n" +
            "  -new        Whether new keys storage.\n\n" +
            "  -open       Whether open existing keys storage.\n\n" +
            "  -sec        The secret keys file.\n\n" +
            "  -pub        The public keys file.\n\n" +
            "EXAMPLES\n" +
            "  pgpkeys -new\n\n" +
            "  pgpkeys -open -sec C:\\secbbox.skr -pub C:\\secbbox.pkr\n"
        );

        if (errMes.Length > 0)
        {
            Console.WriteLine("Error: " + errMes);
            Console.WriteLine();
        }

        confirmExit();
    }

    private static List<string> waitCommand(string promptMes)
    {
        Console.Write(promptMes);
        List<string> result = Regex.Matches(Console.ReadLine(), @"[\""].+?[\""]|[^ ]+").Cast<Match>().Select(m => m.Value).ToList();
        for (int x = 0; x < result.Count; x++)
        {
            if (result[x].StartsWith("\"") && result[x].EndsWith("\""))
            {
                result[x] = result[x].Remove(result[x].Length - 1);
                result[x] = result[x].Remove(0, 1);
            }
        }

        return result;
    }

    private static void displayCommands()
    {
        Console.WriteLine("PGPKeys Commands:\n" +
            "   list                           keys list\n" +
            "   new <username>                 generate new key\n" +
            "   add <key file>                 add key from file\n" +
            "   del <keyId>                    delete key\n" +
            "   export <keyId> <key file>      export key to file\n" +
            "   save <sec file> <pub file>     save keys to files\n" +
            "   exit, quit, q, bye             exit\n" +
            "   ?, help, man                   help");
    }

    private static void confirmExit()
    {
        Console.WriteLine("Press Enter to exit the demo.");
        Console.ReadLine();
    }

    static void Main(string[] args)
    {
        if (args.Length < 1)
        {
            displayHelp("");
            return;
        }

        bool newF = false;
        bool openF = false;
        string seckey = "";
        string pubkey = "";

        if (optext(args, "-new"))
        {
            newF = true;
        }

        if (optext(args, "-open"))
        {
            openF = true;
        }

        if (!(newF || openF))
        {
            displayHelp("-new or -open is required.");
            return;
        }

        if (newF && openF)
        {
            displayHelp("Use only one -new or -open parameter.");
            return;
        }

        if (optext(args, "-sec"))
        {
            seckey = optval(args, "-sec");
        }

        if (optext(args, "-pub"))
        {
            pubkey = optval(args, "-pub");
        }

        PGPKeyring keyring = new PGPKeyring();

        try
        {
            if (openF)
            {
                if (seckey.Length == 0 && pubkey.Length == 0)
                {
                    displayHelp("-sec or -pub is required when use -open");
                    return;
                }

                if (pubkey.Length > 0)
                {
                    keyring.ImportFromFile(pubkey);
                }

                if (seckey.Length > 0)
                {
                    keyring.ImportFromFile(seckey);
                }
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine("Error: " + ex.Message + "\n" + ex.StackTrace);
            return;
        }

        // main loop to check for commands
        while (true)
        {
            try
            {
                var commands = waitCommand("pgp>");

                if (commands.Count > 0)
                {
                    if (commands[0].Equals("list", StringComparison.CurrentCultureIgnoreCase))
                    {
                        Console.WriteLine("Keys:");

                        for (int x = 0; x < keyring.Keys.Count; x++)
                        {
                            if (!keyring.Keys[x].IsSubkey)
                            {
                                Console.WriteLine("  Key id: " + keyring.Keys[x].KeyID);
                                Console.WriteLine("  Key FP: " + keyring.Keys[x].KeyFP);
                                Console.WriteLine("  Algorithm: " + keyring.Keys[x].PublicKeyAlgorithm + " (" + keyring.Keys[x].BitsInKey.ToString() + ")");
                                Console.WriteLine("  Created: " + keyring.Keys[x].Timestamp);
                                Console.WriteLine("  Valid to: " + keyring.Keys[x].ValidTo);

                                PGPKeyManager km = new PGPKeyManager();

                                km.PinnedKey = keyring.Keys[x];
                                km.ImportPinned();

                                Console.WriteLine("  Users (" + km.Users.Count.ToString() + "):");
                                for (int y = 0; y < km.Users.Count; y++)
                                {
                                    Console.WriteLine("    Username: " + km.Users[y].Username);
                                    Console.WriteLine("    Signatures:");
                                    for (int k = 0; k < km.Signatures.Count; k++)
                                    {
                                        if (km.Signatures[k].Target.Equals(km.Users[y].Username))
                                        {
                                            if (km.Signatures[k].Revocation)
                                            {
                                                Console.WriteLine("      Type: Revocation");
                                            }
                                            else
                                            {
                                                Console.WriteLine("      Type: Signature");
                                            }

                                            Console.WriteLine("      Signer: " + km.Signatures[k].SignerKeyID);
                                            Console.WriteLine("      Created: " + km.Signatures[k].CreationTime);
                                        }
                                    }
                                }

                                Console.WriteLine("  Subkeys (" + km.Subkeys.Count.ToString() + "):");
                                for (int y = 0; y < km.Subkeys.Count; y++)
                                {
                                    Console.WriteLine("    Key id: " + km.Subkeys[y].KeyID);
                                    Console.WriteLine("    Key FP: " + km.Subkeys[y].KeyFP);
                                    Console.WriteLine("    Algorithm: " + km.Subkeys[y].PublicKeyAlgorithm + " (" + km.Subkeys[y].BitsInKey.ToString() + ")");
                                    Console.WriteLine("    Created: " + km.Subkeys[y].Timestamp);
                                    Console.WriteLine("    Valid to: " + km.Subkeys[y].ValidTo);

                                    Console.WriteLine("    Signatures:");
                                    for (int k = 0; k < km.Signatures.Count; k++)
                                    {
                                        if (km.Signatures[k].Target.Equals(km.Subkeys[y].KeyID))
                                        {
                                            if (km.Signatures[k].Revocation)
                                            {
                                                Console.WriteLine("      Type: Revocation");
                                            }
                                            else
                                            {
                                                Console.WriteLine("      Type: Signature");
                                            }

                                            Console.WriteLine("      Signer: " + km.Signatures[k].SignerKeyID);
                                            Console.WriteLine("      Created: " + km.Signatures[k].CreationTime);
                                        }
                                    }
                                }
                                Console.WriteLine();
                            }
                        }
                    }
                    else if (commands[0].Equals("new", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <username> parameter");
                        }
                        else
                        {
                            PGPKeyManager km = new PGPKeyManager();

                            km.GeneratePair(6, commands[1], 0, "", "");

                            keyring.PinnedKey = km.Key;
                            keyring.ImportPinned();

                            Console.WriteLine("Generate new PGP key pair");
                        }
                    }
                    else if (commands[0].Equals("add", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <key file> parameter");
                        }
                        else
                        {
                            keyring.ImportFromFile(commands[1]);

                            Console.WriteLine("Added key from file: " + commands[1]);
                        }
                    }
                    else if (commands[0].Equals("del", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <keyId> parameter");
                        }
                        else
                        {
                            keyring.RemoveByID(commands[1]);

                            Console.WriteLine("Key was successfully removed.");
                        }
                    }
                    else if (commands[0].Equals("export", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 3)
                        {
                            Console.WriteLine("You need to pass two parameters: <keyId> and <key file>");
                        }
                        else
                        {
                            int idx = -1;
                            for (int x = 0; x < keyring.Keys.Count; x++)
                            {
                                if (!keyring.Keys[x].IsSubkey && keyring.Keys[x].KeyID.Equals(commands[1]))
                                {
                                    idx = x;
                                    break;
                                }
                            }

                            if (idx >= 0)
                            {
                                PGPKeyManager km = new PGPKeyManager();
                                km.PinnedKey = keyring.Keys[idx];
                                km.ImportPinned();

                                km.ExportToFile(commands[2], false);

                                Console.WriteLine("Export public key to file: " + commands[2]);
                            }
                            else
                            {
                                Console.WriteLine("Key with Id not found.");
                            }
                        }
                    }
                    else if (commands[0].Equals("save", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 3)
                        {
                            Console.WriteLine("You need to pass two parameters: <sec file> and <pub file>");
                        }
                        else
                        {
                            if (commands[1].Length > 0)
                            {
                                keyring.ExportToFile(commands[1], true);
                            }

                            if (commands[2].Length > 0)
                            {
                                keyring.ExportToFile(commands[2], false);
                            }

                            Console.WriteLine("Save keys to files: " + commands[1] + " and " + commands[2]);
                        }
                    }
                    else if (commands[0].Equals("bye", StringComparison.CurrentCultureIgnoreCase) || commands[0].Equals("exit", StringComparison.CurrentCultureIgnoreCase) || commands[0].Equals("quit", StringComparison.CurrentCultureIgnoreCase) || commands[0].Equals("q", StringComparison.CurrentCultureIgnoreCase))
                    {
                        Console.WriteLine();
                        break;
                    }
                    else if (commands[0].Equals("?", StringComparison.CurrentCultureIgnoreCase) || commands[0].Equals("help", StringComparison.CurrentCultureIgnoreCase) || commands[0].Equals("man", StringComparison.CurrentCultureIgnoreCase))
                    {
                        displayCommands();
                    }
                    else
                    {
                        Console.WriteLine("Command not recognized.");
                        displayCommands();
                    }
                }
                else
                {
                    Console.WriteLine("Command not recognized.");
                    displayCommands();
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("Error: " + ex.Message);
            }
        }

        confirmExit();
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