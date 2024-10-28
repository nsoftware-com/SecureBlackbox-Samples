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

class usermanager
{
    private static void displayHelp(string errMes)
    {
        Console.WriteLine(
                "NAME\n" +
                "  usermanager -- SecureBlackbox UserManager Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  usermanager <-set|-check|-list|-del> <users-file> <-filepass file-password> [-user user_name] [-pass password]\n\n" +
                "DESCRIPTION\n" +
                "  UserManager demonstrates the usage of UserManager from SecureBlackbox.\n" +
                "  Used to create file with user information.\n\n" +
                "  The options are as follows:\n\n" +
                "  -set          Whether to add/modify user.\n\n" +
                "  -check        Whether to verify user password.\n\n" +
                "  -list         Whether to get list of users.\n\n" +
                "  -del          Whether to remove user.\n\n" +
                "  -filepass     The password for the file (Required).\n\n" +
                "  -user         The user name\n\n" +
                "  -pass         The user's password.\n\n" +
                "EXAMPLES\n" +
                "	usermanager -list myusers.bin\n\n" +
                "	usermanager -check -user myuser -pass mypassword -filepass password myusers.bin\n\n" +
                "	usermanager -set -user myuser -pass newpass -filepass password myusers.bin\n\n" +
                "	usermanager -set -user myuser -filepass mypassword myusers.bin\n\n" +
                "	usermanager -del -user myuser -filepass password myusers.bin\n"
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
            UserManager um = new UserManager();

            bool list = false;
            bool check = false;
            bool set = false;
            bool del = false;
            String filename = "";
            String filepass = "";
            String user = "";
            String pass = "";

            int i = 0;
            while (i < args.Length)
            {
                if (args[i].StartsWith("-"))
                {
                    if (args[i].Equals("-list", StringComparison.CurrentCultureIgnoreCase))
                        list = true;
                    else if (args[i].Equals("-check", StringComparison.CurrentCultureIgnoreCase))
                        check = true;
                    else if (args[i].Equals("-set", StringComparison.CurrentCultureIgnoreCase))
                        set = true;
                    else if (args[i].Equals("-del", StringComparison.CurrentCultureIgnoreCase))
                        del = true;
                    else if (args[i].Equals("-filepass", StringComparison.CurrentCultureIgnoreCase))
                    {
                        filepass = args[i + 1]; // args[i+1] corresponds to the value of argument [i]
                        i++;
                    }
                    else if (args[i].Equals("-user", StringComparison.CurrentCultureIgnoreCase))
                    {
                        user = args[i + 1];
                        i++;
                    }
                    else if (args[i].Equals("-pass", StringComparison.CurrentCultureIgnoreCase))
                    {
                        pass = args[i + 1];
                        i++;
                    }
                }
                else
                    filename = args[i];

                i++;
            }

            if (!(list || check || set || del))
            {
                displayHelp("-list or -check or -set or -del is required.");
                return;
            }

            if ((list && (check || set || del)) || (check && (list || set || del)) || (set && (check || list || del)) || (del && (check || set || list)))
            {
                displayHelp("Use only one -list or -check or -set or -del parameter.");
                return;
            }

            if (filename.Length == 0)
            {
                displayHelp("users-file is required.");
                return;
            }

            if (filepass.Length == 0)
            {
                displayHelp("filepass is required.");
                return;
            }

            bool openFile = false;
            bool saveFile = set || del;
            if (set)
            {
                openFile = File.Exists(filename);
            }
            else
                openFile = true;

            if (openFile)
            {
                um.ImportFromFile(filename, filepass, true);
            }

            if (list)
            {
                Console.WriteLine(um.Users.Count + " user(s) have been found:");

                for (int x = 0; x < um.Users.Count; x++)
                {
                    Console.WriteLine("   - User " + (x + 1) + ": " + um.Users[x].Username + " (" +
                            (um.Users[x].Password.Length > 0 ? "with password" : "no password") + ")");
                }
            }
            else if (check)
            {
                if (user.Length == 0)
                {
                    displayHelp("-user is required.");
                    return;
                }

                if (pass.Length == 0)
                {
                    displayHelp("-pass is required.");
                    return;
                }
                
                if (um.VerifyUser(user, pass))
                {
                    Console.WriteLine("Password is correct");
                }
                else
                {
                    Console.WriteLine("Password is incorrect");
                }
            }
            else if (set)
            {
                if (user.Length == 0)
                {
                    displayHelp("-user is required.");
                    return;
                }

                int Idx = -1;
                for (int x = 0; x < um.Users.Count; x++)
                {
                    if (user.Equals(um.Users[x].Username))
                    {
                        Idx = x;
                        break;
                    }
                }

                if (Idx == -1)
                {
                    Idx = um.AddUser(user);
                    um.Users[Idx].Password = pass;
                    Console.WriteLine("New user added");
                }
                else
                {
                    um.Users[Idx].Password = pass;
                    Console.WriteLine("User has been changed");
                }
            }
            else // del
            {
                if (user.Length == 0)
                {
                    displayHelp("-user is required.");
                    return;
                }

                bool found = false;
                for (int x = 0; x < um.Users.Count; x++)
                {
                    if (user.Equals(um.Users[x].Username))
                    {
                        um.Users.Remove(x);
                        found = true;
                        break;
                    }
                }

                if (found)
                {
                    Console.WriteLine("User has been deleted");
                }
                else
                {
                    Console.WriteLine("User not found");
                }
            }

            if (saveFile)
            {
                um.ExportToFile(filename, filepass);
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