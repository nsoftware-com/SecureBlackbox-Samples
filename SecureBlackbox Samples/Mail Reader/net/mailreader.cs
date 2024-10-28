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

class mailreader
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
                "  mailreader -- SecureBlackbox MailReader Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  mailreader <-input input_file> [-cert certificate_file] [-certpass certificate_password]\n\n" +
                "DESCRIPTION\n" +
                "  This sample shows how to parse an e-mail message, including signed  and/or encrypted messages.\n\n" +
                "  The options are as follows:\n\n" +
                "  -input        An input file to verify (Required).\n\n" +
                "  -cert         The certificate used to sign files.\n\n" +
                "  -certpass     The password for the signing certificate.\n\n" +
                "EXAMPLES\n" +
                "  mailreader -input C:\\mymail.eml -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
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

    private static void showMailInfo(MailReader reader)
    {
        Console.WriteLine();
        Console.WriteLine("Sender = " + reader.Message.Sender);
        Console.WriteLine("From = " + reader.Message.From);
        Console.WriteLine("SendTo = " + reader.Message.SendTo);
        Console.WriteLine("CC = " + reader.Message.Cc);
        Console.WriteLine("BCC = " + reader.Message.Bcc);
        Console.WriteLine("\n\t\t***Security Info***");
        Console.WriteLine("Encrypted = " + (reader.SecurityInfo.Encrypted ? "true" : "false"));
        if (reader.SecurityInfo.Encrypted)
        {
            Console.Write("Decryption Certificate = ");
            if (reader.DecryptionCertificate == null)
            {
                Console.WriteLine("[certificate not provided]");
            }
            else
            {
                Console.WriteLine(reader.DecryptionCertificate.SubjectRDN);
            }
            Console.WriteLine("Encryption algorithm = " + reader.SecurityInfo.EncryptionAlgorithm);
        }
        Console.WriteLine("Signed = " + (reader.SecurityInfo.Signed ? "true" : "false"));
        if (reader.SecurityInfo.Signed)
        {
            Console.Write("Signing Certificate = ");
            if (reader.SigningCertificate == null)
            {
                Console.WriteLine("[certificate not found]");
            }
            else
            {
                Console.WriteLine(reader.SigningCertificate.SubjectRDN);
            }
            Console.Write("Signature validation = ");
            switch (reader.SecurityInfo.SignatureValidationResult)
            {
                case SignatureValidities.svtValid:
                    Console.WriteLine("VALID");
                    break;

                case SignatureValidities.svtCorrupted:
                    Console.WriteLine("CORRUPTED");
                    break;

                case SignatureValidities.svtSignerNotFound:
                    Console.WriteLine("SIGNER NOT FOUND");
                    break;

                case SignatureValidities.svtFailure:
                    Console.WriteLine("FAILURE");
                    break;

                default:
                    Console.WriteLine("UNKNOWN");
                    break;
            }

            Console.WriteLine("Hash algorithm = " + reader.SecurityInfo.HashAlgorithm);
        }
        Console.WriteLine("\t***End Security Info Block***\n");

        Console.WriteLine("Subject = " + reader.Message.Subject);
        Console.Write("Priority = ");
        switch (reader.Message.Priority)
        {
            case MailPriorities.mpLowest:
                Console.WriteLine("LOWEST");
                break;

            case MailPriorities.mpLow:
                Console.WriteLine("LOW");
                break;

            case MailPriorities.mpNormal:
                Console.WriteLine("NORMAL");
                break;

            case MailPriorities.mpHigh:
                Console.WriteLine("HIGH");
                break;

            case MailPriorities.mpHighest:
                Console.WriteLine("HIGHEST");
                break;

            default:
                Console.WriteLine("UNKNOWN");
                break;
        }
        Console.WriteLine("Delivery receipt = " + (reader.Message.DeliveryReceipt ? "true" : "false"));
        Console.WriteLine("Read receipt = " + (reader.Message.ReadReceipt ? "true" : "false"));
        Console.WriteLine("\n\t\t***Plain Text***");
        Console.WriteLine(reader.Message.PlainText);
        Console.WriteLine("\t\t***Html Text***");
        Console.WriteLine(reader.Message.HtmlText);
        Console.WriteLine("\nAttachments:");
        for (int i = 0; i < reader.Attachments.Count; i++)
        {
            Console.WriteLine(reader.Attachments[i].FileName + "\t\tSize = " + reader.Attachments[i].Size);
        }
        Console.WriteLine("\n**************************END**************************\n");
    }

    private static MailReader reader;
    private static void _reader_OnDecryptionInfoNeeded(object sender, MailReaderDecryptionInfoNeededEventArgs e)
    {
        Console.WriteLine("************************************************************************");
        Console.WriteLine("Decryption needed! Please try again with correct path to certificate - the 2nd argument");
        Console.WriteLine("************************************************************************");
    }

    static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            displayHelp("");
            return;
        }

        reader = new MailReader();
        CertificateManager cm = new CertificateManager();
        String input;

        try
        {
            reader.OnDecryptionInfoNeeded += _reader_OnDecryptionInfoNeeded;


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
                cm.ImportFromFile(optval(args, "-cert"), optval(args, "-certpass"));
                reader.DecryptionCertificate = cm.Certificate;
            }

            reader.LoadFromFile(input);

            showMailInfo(reader);

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