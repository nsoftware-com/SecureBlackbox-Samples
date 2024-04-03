/*
 * SecureBlackbox 2022 C++ Edition - Sample Project
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
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../include/secureblackbox.h"

namespace ArgParser {
    static char* optval(int argc, char** argv, const char* option) {
        for (int x = 0; x < argc - 1; x++) {
            if (!strcmp(argv[x], option)) {
                return argv[x + 1];
            }
        }
        return (char*)"";
    }

    static bool optext(int argc, char** argv, const char* option) {
        for (int x = 0; x < argc; x++) {
            if (!strcmp(argv[x], option)) {
                return true;
            }
        }
        return false;
    }
};

using namespace ArgParser;

void displayHelp() {
    printf(
        "NAME\n"
        "  xmlencryptor -- SecureBlackbox XMLEncryptor Demo Application\n\n"
        "SYNOPSIS\n"
        "  xmlencryptor [-input input_file] [-output output_file] [-datatype encrypted_data_type] [-encmethod encryption_method] \n"
        "            [-xmlnode xml_node] [-enckey] [-enckeytype encryption_key_type] [-transport key_transport_method] \n"
        "            [-wrap key_wrap_method] [-cert certificate_file] [-certpass certificate_password] [-pass key_password] \n\n"
        "DESCRIPTION\n"
        "  XMLEncryptor demonstrates the usage of XMLEncryptor from SecureBlackbox.\n"
        "  Used to encrypt XML file.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input XML file to encrypt (Required). \n\n"
        "  -output       Where the encrypted XML file will be saved (Required). \n\n"
        "  -datatype     The encryption data type to use. Enter the corresponding number. Valid values: \n\n"
        "                  0 - Element \n"
        "                  1 - Content \n"
        "  -encmethod    The encryption method to use. Valid values: 3DES, AES128, AES192, AES256, Camellia128, Camellia192, Camellia256, DES, RC4, SEED \n\n"
        "  -xmlnode      The xml node value. \n\n"
        "  -enckey       Whether to use key encryption. \n\n"
        "  -enckeytype   The encryption key type to use. Enter the corresponding number. Valid values: \n\n"
        "                  0 - KeyTransport \n"
        "                  1 - KeyWrap \n"
        "  -transport    The key transport method to use. Enter the corresponding number. Valid values: \n\n"
        "                  0 - RSA15 \n"
        "                  1 - RSAOAEP \n"
        "  -wrap         The key wrap method to use. Valid values: 3DES, AES128, AES192, AES256, Camellia128, Camellia192, Camellia256, SEED. \n\n"
        "  -cert         The certificate used to encrypt file. \n\n"
        "  -certpass     The password for the certificate. \n\n"
        "  -pass         The password for the encrypting. \n\n"
        "EXAMPLES\n"
        "  xmlencryptor -input C:\\xml\\myfile.xml -output C:\\xml\\myencfile.xml -pass mypassword \n\n"
        "  xmlencryptor -input C:\\xml\\myfile.xml -output C:\\xml\\myencfile.xml -enckey -pass mypassword -datatype 1 -wrap AES192 \n\n"
        "  xmlencryptor -input C:\\xml\\myfile.xml -output C:\\xml\\myencfile.xml -enckey -enckeytype 0 -cert C:\\certs\\mycert.pfx -certpass mypassword \n"
        "              -datatype 2 -mimetype \"Test mime type\" -external C:\\xml\\external.xml -transport 1 \n\n"
    );
}

int getKeyLen(char* algorithm) {
    int len = 0;

    if (!strcmp(algorithm, "AES128") || !strcmp(algorithm, "RC4") || !strcmp(algorithm, "Camellia128") || !strcmp(algorithm, "SEED")) {
        len = 16;
    } else if (!strcmp(algorithm, "AES192") || !strcmp(algorithm, "Camellia192") || !strcmp(algorithm, "3DES")) {
        len = 24;
    } else if (!strcmp(algorithm, "AES256") || !strcmp(algorithm, "Camellia256")) {
        len = 32;
    } else if (!strcmp(algorithm, "DES")) {
        len = 8;
    }
    return len;
}

char* getKey(char* algorithm, char* pass) {
    const int len = getKeyLen(algorithm);

    // simple key derivation function from a Passphrase
    // TODO: replace with SHA256 hash or KDF
    char* res = new char[len];
    strcpy(res, pass);
    while (strlen(res) < len) {
        strcat(res, "/");
        strcat(res, pass);
    }

    return res;
}

int main(int argc, char** argv) {
    XMLEncryptor encryptor;
    encryptor.SetUseGCM(false);
    CertificateManager cm;

    bool encryptKey = false;
    int encryptedDataType = 0;
    int enckeytype = 1;
    int transportmethod = 0;
    char* wrapmethod = "3DES";

    // Validate input
    if (argc < 5) {
        displayHelp();
        goto done;
    }

    char* input = optval(argc, argv, "-input");
    if (!strcmp(input, "")) {
        printf("-input is required.");
        displayHelp();
        goto done;
    }

    char* output = optval(argc, argv, "-output");
    if (!strcmp(output, "")) {
        printf("-output is required.");
        displayHelp();
        goto done;
    }

    if (optext(argc, argv, "-datatype")) {
        encryptedDataType = atoi(optval(argc, argv, "-datatype"));
    }

    if (optext(argc, argv, "-encmethod")) {
        encryptor.SetEncryptionMethod(optval(argc, argv, "-datatype"));
    }

    if (optext(argc, argv, "-xmlnode")) {
        encryptor.SetXMLNode(optval(argc, argv, "-xmlnode"));
    }

    if (optext(argc, argv, "-enckey")) {
        encryptKey = true;
    }

    if (optext(argc, argv, "-enckeytype")) {
        enckeytype = atoi(optval(argc, argv, "-enckeytype"));
    }

    if (optext(argc, argv, "-transport")) {
        transportmethod = atoi(optval(argc, argv, "-transport"));
    }

    if (optext(argc, argv, "-wrap")) {
        wrapmethod = optval(argc, argv, "-wrap");
    }

    char* cert = optval(argc, argv, "-cert");

    char* certpass = optval(argc, argv, "-certpass");

    char* keypass = optval(argc, argv, "-pass");

    if (encryptKey && enckeytype == 0) {
        if (!strcmp(cert, "")) {
            printf("-cert is required.");
            displayHelp();
            goto done;
        }

        if (!strcmp(certpass, "")) {
            printf("-certpass is required.");
            displayHelp();
            goto done;
        }
    } else {
        if (!strcmp(keypass, "")) {
            printf("-pass is required.");
            displayHelp();
            goto done;
        }
    }

    encryptor.SetInputFile(input);
    encryptor.SetOutputFile(output);

    encryptor.SetEncryptKey(encryptKey);
    encryptor.SetEncryptedDataType(encryptedDataType);

    if (encryptor.GetEncryptKey()) {
        encryptor.SetKeyEncryptionType(enckeytype);
        if (enckeytype == 0) {
            encryptor.SetKeyTransportMethod(transportmethod);

            cm.ImportFromFile(cert, certpass);
            encryptor.SetKeyEncryptionCertHandle(cm.GetCertHandle());
        } else {
            encryptor.SetKeyWrapMethod(wrapmethod);
            encryptor.SetKeyEncryptionKey(getKey(wrapmethod, keypass), getKeyLen(wrapmethod));
        }
    } else {
        encryptor.SetEncryptionKey(getKey(encryptor.GetEncryptionMethod(), keypass),
                                   getKeyLen(encryptor.GetEncryptionMethod()));
    }

    if (encryptor.Encrypt()) {
        goto done;
    }

    printf("XML file successfully encrypted.\n");

done:
    if (encryptor.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", encryptor.GetLastErrorCode(), encryptor.GetLastError());
    }
    getchar();
}


