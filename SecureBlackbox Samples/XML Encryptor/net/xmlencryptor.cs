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

class xmlencryptor
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
                "  xmlencryptor -- SecureBlackbox XMLEncryptor Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  xmlencryptor <-input input_file> <-output output_file> [-datatype encrypted_data_type] [-encmethod encryption_method] \n" +
                "            [-xmlnode xml_node] [-enckey] [-enckeytype encryption_key_type] [-transport key_transport_method] \n" +
                "            [-wrap key_wrap_method] [-cert certificate_file] [-certpass certificate_password] [-pass key_password] \n\n" +
                "DESCRIPTION\n" +
                "  XMLEncryptor demonstrates the usage of XMLEncryptor from SecureBlackbox.\n" +
                "  Used to encrypt XML file.\n\n" +
                "  The options are as follows:\n\n" +
                "  -input        An input XML file to encrypt (Required). \n\n" +
                "  -output       Where the encrypted XML file will be saved (Required). \n\n" +
                "  -datatype     The encryption data type to use. Enter the corresponding number. Valid values: \n\n" +
                "                  0 - Element \n" +
                "                  1 - Content \n\n" +
                "  -encmethod    The encryption method to use. Valid values: 3DES, AES128, AES192, AES256, Camellia128, Camellia192, Camellia256, DES, RC4, SEED \n\n" +
                "  -xmlnode      The xml node value. \n\n" +
                "  -enckey       Whether to use key encryption. \n\n" +
                "  -enckeytype   The encryption key type to use. Enter the corresponding number. Valid values: \n\n" +
                "                  0 - KeyTransport \n" +
                "                  1 - KeyWrap \n\n" +
                "  -transport    The key transport method to use. Enter the corresponding number. Valid values: \n\n" +
                "                  0 - RSA15 \n" +
                "                  1 - RSAOAEP \n\n" +
                "  -wrap         The key wrap method to use. Valid values: 3DES, AES128, AES192, AES256, Camellia128, Camellia192, Camellia256, SEED. \n\n" +
                "  -cert         The certificate used to encrypt file. \n\n" +
                "  -certpass     The password for the certificate. \n\n" +
                "  -pass         The password for the encrypting. \n\n" +
                "EXAMPLES\n" +
                "  xmlencryptor -input C:\\xml\\myfile.xml -output C:\\xml\\myencfile.xml -pass mypassword \n\n" +
                "  xmlencryptor -input C:\\xml\\myfile.xml -output C:\\xml\\myencfile.xml -enckey -pass mypassword -datatype 1 -wrap AES192 \n\n" +
                "  xmlencryptor -input C:\\xml\\myfile.xml -output C:\\xml\\myencfile.xml -enckey -enckeytype 0 -cert C:\\certs\\mycert.pfx -certpass mypassword \n" +
                "              -datatype 2 -mimetype \"Test mime type\" -external C:\\xml\\external.xml -transport 1 \n"
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

    static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            displayHelp("");
            return;
        }

        try
        {
            XMLEncryptor encryptor = new XMLEncryptor();
            encryptor.UseGCM = false;

            bool encryptKey = false;
            int encryptedDataType = 0;
            int enckeytype = 1;
            int transportmethod = 0;
            String wrapmethod = "3DES";
            String mimetype = "";
            String keyPass = "";
            String externalFile = "";

            if (optext(args, "-input"))
            {
                encryptor.InputFile = optval(args, "-input");
            }
            else
            {
                displayHelp("-input is required.");
                return;
            }

            if (optext(args, "-output"))
            {
                encryptor.OutputFile = optval(args, "-output");
            }
            else
            {
                displayHelp("-output is required.");
                return;
            }

            if (optext(args, "-encmethod"))
            {
                encryptor.EncryptionMethod = optval(args, "-encmethod");
            }

            if (optext(args, "-xmlnode"))
            {
                encryptor.XMLNode = optval(args, "-xmlnode");
            }

            if (optext(args, "-datatype"))
            {
                encryptedDataType = int.Parse(optval(args, "-datatype"));
            }

            if (optext(args, "-mimetype"))
            {
                mimetype = optval(args, "-mimetype");
            }

            if (optext(args, "-enckey"))
            {
                encryptKey = true;
            }

            if (optext(args, "-enckeytype"))
            {
                enckeytype = int.Parse(optval(args, "-enckeytype"));
            }

            if (optext(args, "-transport"))
            {
                transportmethod = int.Parse(optval(args, "-transport"));
            }

            if (optext(args, "-wrap"))
            {
                wrapmethod = optval(args, "-wrap");
            }

            if (optext(args, "-pass"))
            {
                keyPass = optval(args, "-pass");
            }

            if (optext(args, "-external"))
            {
                externalFile = optval(args, "-external");
            }

            encryptor.EncryptKey = encryptKey;
            switch (encryptedDataType)
            {
                case 1:
                    encryptor.EncryptedDataType = XMLEncryptorEncryptedDataTypes.cxedtContent;
                    break;
                case 2:
                    encryptor.EncryptedDataType = XMLEncryptorEncryptedDataTypes.cxedtExternal;
                    if (mimetype.Length > 0)
                        encryptor.Config("MimeType=" + mimetype);

                    try
                    {
                        encryptor.ExternalData = File.ReadAllBytes(externalFile);
                    }
                    catch (Exception ex)
                    {
                        Console.WriteLine("Error: " + ex.Message);
                    }

                    break;
                default:
                    encryptor.EncryptedDataType = XMLEncryptorEncryptedDataTypes.cxedtElement;
                    break;
            }

            if (encryptor.EncryptKey)
            {
                if (enckeytype == 0)
                {
                    String certFile = "";
                    String certPass = "";
                    if (optext(args, "-cert"))
                    {
                        certFile = optval(args, "-cert");
                    }
                    else
                    {
                        Console.WriteLine();
                        displayHelp("-cert is required.");
                        return;
                    }

                    if (optext(args, "-certpass"))
                    {
                        certPass = optval(args, "-certpass");
                    }

                    encryptor.KeyEncryptionType = XMLEncryptorKeyEncryptionTypes.cxetKeyTransport;

                    if (transportmethod == 0)
                        encryptor.KeyTransportMethod = XMLEncryptorKeyTransportMethods.cxktRSA15;
                    else
                        encryptor.KeyTransportMethod = XMLEncryptorKeyTransportMethods.cxktRSAOAEP;

                    CertificateManager cm = new CertificateManager();
                    cm.ImportFromFile(certFile, certPass);
                    encryptor.KeyEncryptionCertificate = cm.Certificate;
                }
                else
                {
                    encryptor.KeyEncryptionType = XMLEncryptorKeyEncryptionTypes.cxetKeyWrap;

                    encryptor.KeyWrapMethod = wrapmethod;
                    encryptor.KeyEncryptionKey = getKey(encryptor.KeyWrapMethod, keyPass);
                }
            }
            else
            {
                encryptor.EncryptionKey = getKey(encryptor.EncryptionMethod, keyPass);
            }

            encryptor.Encrypt();

            Console.WriteLine("XML file successfully encrypted");

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