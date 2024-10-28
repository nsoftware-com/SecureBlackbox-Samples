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

class xmldecryptor
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
                "  xmldecryptor -- SecureBlackbox XMLDecryptor Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  xmldecryptor <-input input_file> <-output output_file> [-cert certificate_file] [-certpass certificate_password] [-pass key_password] \n\n" +
                "DESCRIPTION\n" +
                "  XMLDecryptor demonstrates the usage of XMLDecryptor from SecureBlackbox.\n" +
                "  Used to decrypt XML file.\n\n" +
                "  The options are as follows:\n\n" +
                "  -input        An input file to decrypt (Required). \n\n" +
                "  -output       Where the decrypted XML file will be saved (Required). \n\n" +
                "  -cert         The certificate used to decrypt file. \n\n" +
                "  -certpass     The password for the certificate. \n\n" +
                "  -pass         The password for the decrypting. \n\n" +
                "EXAMPLES\n" +
                "  xmldecryptor -input C:\\xml\\myenc.xml -output C:\\xml\\myfile.xml -pass mtpassword \n" +
                "  xmldecryptor -input C:\\xml\\myenc.xml -output C:\\xml\\myfile.xml -cert C:\\certs\\mycert.pfx -certpass test -external C:\\xml\\external.xml \n"
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

    private static byte[] getKey(string algorithm, string pass)
    {
        int len = 0;

        if (algorithm.Equals("AES128", StringComparison.CurrentCultureIgnoreCase))
            len = 16;
        else if (algorithm.Equals("AES192", StringComparison.CurrentCultureIgnoreCase))
            len = 24;
        else if (algorithm.Equals("AES256", StringComparison.CurrentCultureIgnoreCase))
            len = 32;
        else if (algorithm.Equals("Camellia128", StringComparison.CurrentCultureIgnoreCase))
            len = 16;
        else if (algorithm.Equals("Camellia192", StringComparison.CurrentCultureIgnoreCase))
            len = 24;
        else if (algorithm.Equals("Camellia256", StringComparison.CurrentCultureIgnoreCase))
            len = 32;
        else if (algorithm.Equals("DES", StringComparison.CurrentCultureIgnoreCase))
            len = 8;
        else if (algorithm.Equals("3DES", StringComparison.CurrentCultureIgnoreCase))
            len = 24;
        else if (algorithm.Equals("RC4", StringComparison.CurrentCultureIgnoreCase))
            len = 16;
        else if (algorithm.Equals("SEED", StringComparison.CurrentCultureIgnoreCase))
            len = 16;

        // simple key derivation function from a Passphrase
        // TODO: replace with SHA256 hash or KDF
        string s = pass;
        while (s.Length < len)
            s = s + "/" + pass;

        byte[] res = new byte[len];
        for (int i = 0; i < len; i++)
            res[i] = (byte)s[i];

        return res;
    }

    private static XMLDecryptor decryptor;
    private static string certFile = "";
    private static string certPass = "";
    private static string keyPass = "";
    private static string externalFile = "";

    private static void _decryptor_OnDecryptionInfoNeeded(object sender, XMLDecryptorDecryptionInfoNeededEventArgs e)
    {
        String t = "";
        if (decryptor.UseGCM)
            t = "-GCM";
        Console.WriteLine("Encryption method: " + decryptor.EncryptionMethod + t);

        if (decryptor.EncryptedDataType == XMLDecryptorEncryptedDataTypes.cxedtElement)
            t = "Element";
        else if (decryptor.EncryptedDataType == XMLDecryptorEncryptedDataTypes.cxedtContent)
            t = "Content";
        else
            t = "External";
        Console.WriteLine("Encrypted data type: " + t);

        if (decryptor.EncryptKey)
        {
            Console.WriteLine("EncryptKey: true");
            if (decryptor.KeyEncryptionType == XMLDecryptorKeyEncryptionTypes.cxetKeyTransport)
            {
                Console.WriteLine("Key encryption type: transport");
                if (decryptor.KeyTransportMethod == XMLDecryptorKeyTransportMethods.cxktRSA15)
                    t = "RSA v1.5";
                else
                    t = "RSA-OAEP";

                Console.WriteLine("Key transport method: " + t);
            }
            else
            {
                Console.WriteLine("Key encryption type: wrap");
                Console.WriteLine("Key wrap method: " + decryptor.KeyWrapMethod);
            }
        }
        else
            Console.WriteLine("EncryptKey: false");

        try
        {
            t = decryptor.Config("KeyName");
            if (t.Length > 0)
                Console.WriteLine("Key name: " + t);

            t = decryptor.Config("MimeType");
            if (t.Length > 0)
                Console.WriteLine("Mime type: " + t);
        }
        catch (Exception ex)
        {
            Console.WriteLine(ex.Message);
        }
        Console.WriteLine("");

        try
        {
            if (decryptor.EncryptKey)
            {
                if (decryptor.KeyEncryptionType == XMLDecryptorKeyEncryptionTypes.cxetKeyTransport)
                {
                    CertificateManager cm = new CertificateManager();
                    cm.ImportFromFile(certFile, certPass);
                    decryptor.KeyDecryptionCertificate = cm.Certificate;
                }
                else
                {
                    decryptor.KeyDecryptionKey = getKey(decryptor.KeyWrapMethod, keyPass);
                }
            }
            else
            {
                decryptor.DecryptionKey = getKey(decryptor.EncryptionMethod, keyPass);
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine(ex.Message);
        }
    }

    private static void _decryptor_OnSaveExternalData(object sender, XMLDecryptorSaveExternalDataEventArgs e)
    {
        try
        {
            File.WriteAllBytes(externalFile, e.ExternalData);
        }
        catch (Exception ex)
        {
        }
    }

    static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            displayHelp("");
            return;
        }

        decryptor = new XMLDecryptor();
        try
        {
            if (optext(args, "-input"))
            {
                decryptor.InputFile = optval(args, "-input");
            }
            else
            {
                displayHelp("-input is required.");
                return;
            }

            if (optext(args, "-output"))
            {
                decryptor.OutputFile = optval(args, "-output");
            }
            else
            {
                displayHelp("-output is required.");
                return;
            }

            if (optext(args, "-cert"))
            {
                certFile = optval(args, "-cert");
            }

            if (optext(args, "-certpass"))
            {
                certPass = optval(args, "-certpass");
            }

            if (optext(args, "-external"))
            {
                externalFile = optval(args, "-external");
            }

            if (optext(args, "-pass"))
            {
                keyPass = optval(args, "-pass");
            }

            decryptor.OnDecryptionInfoNeeded += _decryptor_OnDecryptionInfoNeeded;
            decryptor.OnSaveExternalData += _decryptor_OnSaveExternalData;

            decryptor.Decrypt();

            Console.WriteLine("XML file successfully decrypted.\n");

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