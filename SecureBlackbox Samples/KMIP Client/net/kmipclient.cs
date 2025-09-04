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

class kmipclient
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
            "  kmipclient -- SecureBlackbox KMIPClient Demo Application\n\n" +
            "SYNOPSIS\n" +
            "  kmipclient <url> [-username username] [-password password] [-encoder encoder_type]\n\n" +
            "DESCRIPTION\n" +
            "  This sample illustrates basic FTP client operations.\n\n" +
            "  The options are as follows:\n\n" +
            "  url         The url of server (Required).\n\n" +
            "  -username   The user identifier to use for login.\n\n" +
            "  -password   The password to log in.\n\n" +
            "  -encoder    The encoder type. Enter the corresponding string. Valid values: ttlv, xml, json.\n\n" +
            "EXAMPLES\n" +
            "  kmipclient kmip://127.0.0.1:5696\n\n" +
            "  kmipclient https://127.0.0.1:5696 -username testuser -password pass -encoder xml\n"
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
        Console.WriteLine("KMIP Commands:\n" +
            "   ls                                   list objects\n" +
            "   nc <group>                           create new certificate\n" +
            "   nk <group>                           create new key\n" +
            "   ac <cert file> <cert pass> <group>   add certificate from file\n" +
            "   ak <key file> <key pass> <group>     add key from file\n" +
            "   del <object id>                      delete object\n" +
            "   e <object id> <input> <output> <IV>  encrypt input file\n" +
            "   d <object id> <input> <output> <IV>  decrypt input file\n" +
            "   s <object id> <input> <output>       sign input file\n" +
            "   v <object id> <data> <signature>     verify signature file\n" +
            "   exit, quit, q, bye                   exit\n" +
            "   ?, help, man                         help");
    }

    private static void confirmExit()
    {
        Console.WriteLine("Press Enter to exit the demo.");
        Console.ReadLine();
    }

    private static void _client_OnTLSCertValidate(object sender, KMIPClientTLSCertValidateEventArgs e)
    {
        e.Accept = true;
    }

    static void Main(string[] args)
    {
        if (args.Length < 1)
        {
            displayHelp("url is required.");
            return;
        }

        KMIPClient client = new KMIPClient();

        try
        {
            client.BaseURL = args[0];

            client.OnTLSCertValidate += _client_OnTLSCertValidate;
            client.TLSSettings.AutoValidateCertificates = false;

            if (optext(args, "-username"))
            {
                client.Username = optval(args, "-username");
            }
            else
            {
                client.Username = "anonymous";
            }

            if (optext(args, "-password"))
            {
                client.Password = optval(args, "-password");
            }

            if (optext(args, "-encoder"))
            {
                String encoder = optval(args, "-encoder");
                if (encoder.Equals("ttlv", StringComparison.CurrentCultureIgnoreCase))
                {
                    client.Encoding = KMIPClientEncodings.etTTLV;
                }
                else if (encoder.Equals("xml", StringComparison.CurrentCultureIgnoreCase))
                {
                    client.Encoding = KMIPClientEncodings.etXML;
                }
                else if (encoder.Equals("json", StringComparison.CurrentCultureIgnoreCase))
                {
                    client.Encoding = KMIPClientEncodings.etJSON;
                }
            }
            else
            {
                client.Encoding = KMIPClientEncodings.etTTLV;
            }

            Console.WriteLine("Connecting to " + args[0]);

            client.List(0, "", 0, 0, false);
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
                var commands = waitCommand("kmip>");

                if (commands.Count > 0)
                {
                    if (commands[0].Equals("ls", StringComparison.CurrentCultureIgnoreCase))
                    {
                        client.List(0, "", 0, 0, false);

                        Console.WriteLine("Objects(" + client.Objects.Count.ToString() + "):");

                        for (int x = 0; x < client.Objects.Count; x++)
                        {

                            Console.WriteLine("  Object id: " + client.Objects[x].ObjectId);
                            switch (client.Objects[x].ObjectType)
                            {
                                case KMIPObjectTypes.otCertificate:
                                    Console.WriteLine("  Object type: Certificate");
                                    break;
                                case KMIPObjectTypes.otSymmetricKey:
                                    Console.WriteLine("  Object type: Symmetric Key");
                                    break;
                                case KMIPObjectTypes.otPublicKey:
                                    Console.WriteLine("  Object type: Public Key");
                                    break;
                                case KMIPObjectTypes.otPrivateKey:
                                    Console.WriteLine("  Object type: Private Key");
                                    break;
                                default:
                                    Console.WriteLine("  Object type: Unknown");
                                    break;
                            }
                            Console.WriteLine("  Algorithm: " + client.Objects[x].KeyAlgorithm);
                            Console.WriteLine("  Key bits: " + client.Objects[x].KeyBits);
                            Console.WriteLine("  Group: " + client.Objects[x].ObjectGroup);
                            Console.WriteLine();
                        }
                    }
                    else if (commands[0].Equals("nc", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <group> parameter");
                        }
                        else
                        {
                            String keyIds = client.GenerateKey("RSA", "", "", 1024, commands[1], true);

                            String pubKeyId = keyIds.Substring(keyIds.IndexOf(client.Config("ListDelimiter")) + 1);

                            String certId = client.Generate(pubKeyId, true);

                            Console.WriteLine("Generate certificate with Id: " + certId + " and keys: " + keyIds);
                        }
                    }
                    else if (commands[0].Equals("nk", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 3)
                        {
                            Console.WriteLine("You need to pass <group> parameter");
                        }
                        else
                        {
                            String keyId = client.GenerateKey("AES", "", "", 256, commands[1], true);

                            Console.WriteLine("Generate key with Id: " + keyId);
                        }
                    }
                    else if (commands[0].Equals("ac", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 4)
                        {
                            Console.WriteLine("You need to pass three parameters: <cert file>, <cert pass> and <group>");
                        }
                        else
                        {
                            CertificateManager cm = new CertificateManager();
                            cm.ImportFromFile(commands[1], commands[2]);

                            client.Certificate = cm.Certificate;
                            String certId = client.Add(true, commands[3], true);

                            Console.WriteLine("Added certificate with Id: " + certId);
                        }
                    }
                    else if (commands[0].Equals("ak", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 4)
                        {
                            Console.WriteLine("You need to pass three parameters: <key file>, <key pass> and <group>");
                        }
                        else
                        {
                            CryptoKeyManager km = new CryptoKeyManager();
                            km.ImportFromFile(commands[1], 1, "", "", "", 0, commands[2]);

                            client.Key = km.Key;
                            String keyId = client.AddKey(commands[3], true);

                            Console.WriteLine("Added key with Id: " + keyId);
                        }
                    }
                    else if (commands[0].Equals("del", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <object id> parameter");
                        }
                        else
                        {
                            client.Remove(commands[1]);
                            Console.WriteLine("Delete object with Id: " + commands[1]);
                        }
                    }
                    else if (commands[0].Equals("e", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 5)
                        {
                            Console.WriteLine("You need to pass four parameters: <object id>, <input>, <output> and <IV>");
                        }
                        else
                        {
                            client.InputFile = commands[2];
                            client.OutputFile = commands[3];

                            Utils utils = new Utils();
                            byte[] IV = utils.HexDecode(commands[4]);

                            client.Encrypt(commands[1], "", IV, "", "", 0);

                            if (client.AuxResult.Length > 0)
                                Console.WriteLine("The file successfully encrypted. IV: " + client.AuxResult);
                            else
                                Console.WriteLine("The file successfully encrypted.");
                        }
                    }
                    else if (commands[0].Equals("d", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 5)
                        {
                            Console.WriteLine("You need to pass four parameters: <object id>, <input>, <output> and <IV>");
                        }
                        else
                        {
                            client.InputFile = commands[2];
                            client.OutputFile = commands[3];

                            Utils utils = new Utils();
                            byte[] IV = utils.HexDecode(commands[4]);

                            client.Decrypt(commands[1], "", IV, "", "", 0);

                            Console.WriteLine("The file successfully decrypted.");
                        }
                    }
                    else if (commands[0].Equals("s", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 4)
                        {
                            Console.WriteLine("You need to pass three parameters: <object id>, <input> and <output>");
                        }
                        else
                        {
                            client.InputFile = commands[2];
                            client.OutputFile = commands[3];

                            client.Sign(commands[1], "", "", "SHA256", false);

                            Console.WriteLine("The file successfully signed.");
                        }
                    }
                    else if (commands[0].Equals("v", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 4)
                        {
                            Console.WriteLine("You need to pass three parameters: <object id>, <data> and <signature>");
                        }
                        else
                        {
                            client.DataFile = commands[2];
                            client.InputFile = commands[3];

                            client.Verify(commands[1], "", "", "SHA256", false);

                            switch (client.SignatureValidationResult)
                            {
                                case KMIPClientSignatureValidationResults.svtValid:
                                    Console.WriteLine("Verification succeeded.");
                                    break;
                                case KMIPClientSignatureValidationResults.svtCorrupted:
                                    Console.WriteLine("Verification corrupted.");
                                    break;
                                case KMIPClientSignatureValidationResults.svtFailure:
                                    Console.WriteLine("Verification failed.");
                                    break;
                                default:
                                    Console.WriteLine("Verification unknown.");
                                    break;
                            }
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