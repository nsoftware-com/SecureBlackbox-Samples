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
using System.IO;
using nsoftware.SecureBlackbox;

class passwordvault
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
                "  passwordvault -- SecureBlackbox PasswordVault Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  passwordvault <-set|-get|-list|-del> [-entry <entry> [-pass <password>]] [-field <field> [-value <value>]] <vault-file> [-vaultpass <vault-password>]\n\n" +
                "DESCRIPTION\n" +
                "  PasswordVault demonstrates the usage of PasswordVault from SecureBlackbox.\n" +
                "  Used to create vaults with user information.\n\n" +
                "  The options are as follows:\n\n" +
                "  -set          Whether to add new entry or add/modify field value.\n\n" +
                "  -get          Whether to get field value.\n\n" +
                "  -list         Whether to get list of entries or fields.\n\n" +
                "  -del          Whether to remove entry or field value.\n\n" +
                "  -entry        The entry name\n\n" +
                "  -pass         The password for the entry.\n\n" +
                "  -field        The field name\n\n" +
                "  -value        The new value of field\n\n" +
                "  vault-file    The vault file\n\n" +
                "  -vaultpass    The password for the vault\n\n" +
                "EXAMPLES\n" +
                "	passwordvault -list myvault.bin\n\n" +
                "	passwordvault -get -entry myftpserver -pass mypassword -field password myvault.bin\n\n" +
                "	passwordvault -set -entry myftpserver -field username -value newusername myvault.bin\n\n" +
                "	passwordvault -set -entry newentry myvault.bin -vaultpass mypassword\n\n" +
                "	passwordvault -del -entry myftpserver -field username myvault.bin\n"
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
            PasswordVault vault = new PasswordVault();

            bool list = false;
            bool get = false;
            bool set = false;
            bool del = false;
            String vaultfile = "";
            String vaultpass = "";
            String entry = "";
            String entrypass = "";
            String field = "";
            String value = "";

            int i = 0;
            while (i < args.Length)
            {
                if (args[i].StartsWith("-"))
                {
                    if (args[i].Equals("-list", StringComparison.CurrentCultureIgnoreCase))
                        list = true;
                    else if (args[i].Equals("-get", StringComparison.CurrentCultureIgnoreCase))
                        get = true;
                    else if (args[i].Equals("-set", StringComparison.CurrentCultureIgnoreCase))
                        set = true;
                    else if (args[i].Equals("-del", StringComparison.CurrentCultureIgnoreCase))
                        del = true;
                    else if (args[i].Equals("-vaultpass", StringComparison.CurrentCultureIgnoreCase))
                    {
                        vaultpass = args[i + 1]; // args[i+1] corresponds to the value of argument [i]
                        i++;
                    }
                    else if (args[i].Equals("-entry", StringComparison.CurrentCultureIgnoreCase))
                    {
                        entry = args[i + 1];
                        i++;
                    }
                    else if (args[i].Equals("-pass", StringComparison.CurrentCultureIgnoreCase))
                    {
                        entrypass = args[i + 1];
                        i++;
                    }
                    else if (args[i].Equals("-field", StringComparison.CurrentCultureIgnoreCase))
                    {
                        field = args[i + 1];
                        i++;
                    }
                    else if (args[i].Equals("-value", StringComparison.CurrentCultureIgnoreCase))
                    {
                        value = args[i + 1];
                        i++;
                    }
                }
                else
                    vaultfile = args[i];

                i++;
            }

            if (!(list || get || set || del))
            {
                displayHelp("-list or -get or -set or -del is required.");
                return;
            }

            if ((list && (get || set || del)) || (get && (list || set || del)) || (set && (get || list || del)) || (del && (get || set || list)))
            {
                displayHelp("Use only one -list or -get or -set or -del parameter.");
                return;
            }

            if (vaultfile.Length == 0)
            {
                displayHelp("vault-file is required.");
                return;
            }

            vault.Password = vaultpass;

            bool openVault = false;
            bool saveVault = set || del;
            if (set)
            {
                openVault = File.Exists(vaultfile);
            }
            else
                openVault = true;

            if (openVault)
            {
                vault.OpenFile(vaultfile);
            }


            if (list)
            {
                if (entry.Length == 0)
                {
                    string[] entries = vault.ListEntries().Split(vault.Config("ListDelimiter"));

                    Console.WriteLine("Entries: ");
                    for (int x = 0; x < entries.Length; x++)
                    {
                        Console.WriteLine("    " + entries[x]);
                    }
                }
                else
                {
                    string[] fields = vault.ListFields(entry, true).Split(vault.Config("ListDelimiter")); ;

                    Console.WriteLine("Fields in \"" + entry + "\": ");
                    for (int x = 0; x < fields.Length; x++)
                    {
                        Console.WriteLine("    " + fields[x]);
                    }
                }
            }
            else if (get)
            {
                if (entry.Length == 0)
                {
                    displayHelp("-entry is required.");
                    return;
                }

                if (field.Length == 0)
                {
                    displayHelp("-field is required.");
                    return;
                }

                vault.EntryPassword = entrypass;

                value = vault.GetEntryValueStr(entry, field);
                Console.WriteLine("Value: " + value);
            }
            else if (set)
            {
                if (entry.Length == 0)
                {
                    displayHelp("-entry is required.");
                    return;
                }

                if (field.Length == 0)
                {
                    vault.AddEntry(entry);
                    vault.ChangeEntryPassword(entry, entrypass);
                    Console.WriteLine("Entry \"" + entry + "\" successfully added.");
                }
                else
                {
                    string[] entries = vault.ListEntries().Split(vault.Config("ListDelimiter"));
                    bool found = false;
                    for (int x = 0; x < entries.Length; x++)
                    {
                        if (entries[x].Equals(entry))
                        {
                            found = true;
                            break;
                        }
                    }
                    if (!found)
                    {
                        vault.AddEntry(entry);
                        vault.ChangeEntryPassword(entry, entrypass);
                    }
                    vault.EntryPassword = entrypass;

                    vault.SetEntryValueStr(entry, field, value, entrypass.Length != 0);
                    Console.WriteLine("Field \"" + field + "\" successfully added/modify.");
                }
            }
            else // del
            {
                if (entry.Length == 0)
                {
                    displayHelp("-entry is required.");
                    return;
                }

                if (field.Length == 0)
                {
                    vault.RemoveEntry(entry);
                    Console.WriteLine("Entry \"" + entry + "\" successfully removed.");
                }
                else
                {
                    vault.RemoveField(entry, field);
                    Console.WriteLine("Field \"" + field + "\" successfully removed.");
                }
            }

            if (saveVault)
            {
                vault.SaveFile(vaultfile);
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