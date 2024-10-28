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
using System.Runtime.InteropServices;
using nsoftware.SecureBlackbox;

class mailwriter
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
                "  mailwriter -- SecureBlackbox MailWriter Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  mailwriter <from> <sender> <to> <cc> <bcc> <subject> <priority> <output>\n" +
                "            [-plain plain_file] [-html html_file] [-scert certificate_file] [-scertpass certificate_password]\n" +
                "            [-ecert certificate_file] [-ecertpass certificate_password] [-hashalg hashalg] [-encalg encalg]\n" +
                "            [-format signature_format] [-a attach_file]\n\n" +
                "DESCRIPTION\n" +
                "  This sample shows how to compose an e-mail message. Also, it's possible to sign and/or encrypt the message with certificates.\n\n" +
                "  The options are as follows:\n\n" +
                "  from          The sender mail address (Required).\n\n" +
                "  sender        The sender name (Required).\n\n" +
                "  to            The recipient mail (Required).\n\n" +
                "  cc            The carbon copy mail address (Required).\n\n" +
                "  bcc           The blind carbon copy mail address (Required).\n\n" +
                "  subject       The letter subject (Required).\n\n" +
                "  priority      The priority of letter. Enter the corresponding number from 0 (the lowest) to 4 (the highest) (Required).\n\n" +
                "  output        The output file (Required).\n\n" +
                "  -plain        The file with plain text message.\n\n" +
                "  -html         The file with html text message.\n\n" +
                "  -scert        The certificate used to sign files.\n\n" +
                "  -scertpass    The password for the signing certificate.\n\n" +
                "  -ecert        The certificate used to encrypt files.\n\n" +
                "  -ecertpass    The password for the encryption certificate.\n\n" +
                "  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224, MD5\n\n" +
                "  -encalg       The encryption algorithm. Enter the corresponding string. Valid values: DES, 3DES, AES128, AES192, AES256, Blowfish, Twofish, Camellia, Serpent\n\n" +
                "  -format       The signature format. Enter the corresponding number. Valid values:\n\n" +
                "                  0  - MS_MULTIPART_SIGNED\n" +
                "                  1  - MS_SIGNED_DATA\n\n" +
                "  -a            The attach file.\n\n" +
                "EXAMPLES\n" +
                "  mailwriter Sbb@mail.com SbbTeam user@mail.com \"alluser@mail.com allpeople@mail.com\" ghost@mail.com \"test example\" 2 mymail.eml\n\n" +
                "  mailwriter Sbb@mail.com SbbTeam user@mail.com \"\" \"\" \"test example\" 1 mymail.eml\n" +
                "          -plain C:\\test.txt -ecert C:\\certs\\mycert.pfx -ecertpass mypassword -hashalg SHA256 -format 0\n"
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
        if (args.Length < 8)
        {
            displayHelp("Required parameters are not specified.");
            return;
        }

        MailWriter writer = new MailWriter();
        CertificateManager cm = new CertificateManager();

        try
        {
            writer.Message.From = args[0];
            writer.Message.Sender = args[1];
            writer.Message.SendTo = args[2];
            writer.Message.Cc = args[3];
            writer.Message.Bcc = args[4];
            writer.Message.Subject = args[5];
            writer.Message.Priority = (MailPriorities)int.Parse(args[6]);
            if (optext(args, "-plain"))
            {
                writer.Message.PlainText = File.ReadAllText(optval(args, "-plain"));
            }
            if (optext(args, "-html"))
            {
                writer.Message.HtmlText = File.ReadAllText(optval(args, "-html"));
            }

            // Additional options
            writer.SecuritySettings.SignBeforeEncrypt = false;
            writer.SecuritySettings.SignMessageHeader = false;

            if (optext(args, "-scert"))
            {
                cm.ImportFromFile(optval(args, "-scert"), optval(args, "-scertpass"));
                writer.SigningCertificate = cm.Certificate;
                writer.SecuritySettings.Sign = true;
                writer.SecuritySettings.HashAlgorithm = "SHA256";
            }

            if (optext(args, "-ecert"))
            {
                cm.ImportFromFile(optval(args, "-ecert"), optval(args, "-ecertpass"));
                writer.EncryptionCertificates.Add(cm.Certificate);
                writer.SecuritySettings.Encrypt = true;
                writer.SecuritySettings.EncryptionAlgorithm = "AES128";
            }

            if (optext(args, "-hashalg"))
            {
                writer.SecuritySettings.HashAlgorithm = optval(args, "-hashalg");
            }

            if (optext(args, "-encalg"))
            {
                writer.SecuritySettings.EncryptionAlgorithm = optval(args, "-encalg");
            }

            if (optext(args, "-format"))
            {
                writer.SecuritySettings.SignatureFormat = (MailSignatureFormats)int.Parse(optval(args, "-format"));
            }

            if (optext(args, "-a"))
            {
                writer.AttachFile(optval(args, "-a"));
            }

            // Save to file
            writer.SaveToFile(args[7]);

            Console.WriteLine("A message has been assembled and saved successfully.\n");

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