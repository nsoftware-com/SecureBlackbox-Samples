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
#include <ctype.h>
#include <stdlib.h>
#include "../../include/secureblackbox.h"

int main(int argc, char* argv[]) {

    if (argc < 13) {
        fprintf(stderr, "\n");
        fprintf(stderr, "  from             the sender mail address\n");
        fprintf(stderr, "  sender           the sender name\n");
        fprintf(stderr, "  to               the recipient mail\n");
        fprintf(stderr, "  cc               the carbon copy mail address\n");
        fprintf(stderr, "  bcc              the blind carbon copy mail address\n");
        fprintf(stderr, "  subject          the letter subject\n");
        fprintf(stderr, "  priority         the priority of letter: default is normal(2), 0 - the lowest, 4 - the highest\n");
        fprintf(stderr, "  delivery         use the delivery receipt: default is false(0), 1 - true\n");
        fprintf(stderr, "  read             use the read receipt: default is false(0), 1 - true\n");
        fprintf(stderr, "  plain            the plain text message\n");
        fprintf(stderr, "  html             the html text message\n");
        fprintf(stderr, "  filename         the output file\n");
        fprintf(stderr, "  Option flags:\n");
        fprintf(stderr, "  -sc <path> <password>    signing certificate\n");
        fprintf(stderr, "  -ec <path> <password>    encryption certificate\n");
        fprintf(stderr,"  -ha <algorithm>          hash algorithm - default(SHA256): MD5, SHA1, SHA224, SHA256, SHA384, SHA512\n");
        fprintf(stderr, "  -ea <algorithm>          encryption algorithm - default(AES128): "
                "DES, 3DES, AES128, AES192, AES256, Blowfish, Twofish, Camellia, Serpent\n");
        fprintf(stderr, "  -sf <format>             signature format - default(0): 0 - multipart/signed, 1 - signed-data\n");
        fprintf(stderr, "  -a <path>                attach file, for multiple files, use multiple \"-a\" flag usage\n");
        fprintf(stderr, "\nExample: mailwriter Sbb@mail.com SbbTeam user@mail.com \"alluser@mail.com allpeople@mail.com\" "
                "ghost@mail.com \"test example\" 2 0 0 \"Have a nice day from Sbb team!\" \"\" output.txt\n\n");
        printf("Press enter to continue.\n");
        getchar();
    } else {
        MailWriter writer;
        CertificateManager certmanager;

        int ret_code = 0;

        writer.SetMsgFrom(argv[1]);
        writer.SetMsgSender(argv[2]);
        writer.SetMsgSendTo(argv[3]);
        writer.SetMsgCc(argv[4]);
        writer.SetMsgBcc(argv[5]);
        writer.SetMsgSubject(argv[6]);
        if (atoi(argv[7]) == 0) {
            writer.SetMsgPriority(MP_LOWEST);
        } else if (atoi(argv[7]) == 1) {
            writer.SetMsgPriority(MP_LOW);
        } else if (atoi(argv[7]) == 2) {
            writer.SetMsgPriority(MP_NORMAL);
        } else if (atoi(argv[7]) == 3) {
            writer.SetMsgPriority(MP_HIGH);
        } else if (atoi(argv[7]) == 4) {
            writer.SetMsgPriority(MP_HIGHEST);
        }
        writer.SetMsgDeliveryReceipt(atoi(argv[8]));
        writer.SetMsgReadReceipt(atoi(argv[9]));
        writer.SetMsgPlainText(argv[10]);
        writer.SetMsgHtmlText(argv[11]);

        writer.SetSecSettingsSignBeforeEncrypt(false);
        writer.SetSecSettingsSignMessageHeader(false);

        writer.SetSigningCertHandle(0);

        bool hasHA = false;
        bool hasSF = false;
        bool hasEA = false;
        if (argc > 13) {
            for (int i = 13; i < argc; i++) {
                if (!strcmp(argv[i], "-sc") && argc > i + 2) {
                    ret_code = certmanager.ImportFromFile(argv[i + 1], argv[i + 2]);
                    if (ret_code == 0) {
                        writer.SetSigningCertHandle(certmanager.GetCertHandle());
                    } else {
                        goto done;
                    }
                } else if (!strcmp(argv[i], "-ec") && argc > i + 2) {
                    ret_code = certmanager.ImportFromFile(argv[i + 1], argv[i + 2]);
                    if (ret_code == 0) {
                        writer.SetEncryptionCertCount(writer.GetEncryptionCertCount() + 1);
                        writer.SetEncryptionCertHandle(writer.GetEncryptionCertCount() - 1, certmanager.GetCertHandle());
                    } else {
                        goto done;
                    }
                } else if (!strcmp(argv[i], "-ha") && argc > i + 1) {
                    hasHA = true;
                    ret_code = writer.SetSecSettingsHashAlgorithm(argv[i + 1]);
                    if (ret_code) {
                        goto done;
                    }
                } else if (!strcmp(argv[i], "-ea") && argc > i + 1) {
                    hasEA = true;
                    ret_code = writer.SetSecSettingsEncryptionAlgorithm(argv[i + 1]);
                    if (ret_code) {
                        goto done;
                    }
                } else if (!strcmp(argv[i], "-sf") && argc > i + 1) {
                    hasSF = true;
                    ret_code = writer.SetSecSettingsSignatureFormat(atoi(argv[i + 1]));
                    if (ret_code) {
                        goto done;
                    }
                } else if (!strcmp(argv[i], "-a") && argc > i + 1) {
                    ret_code = writer.AttachFile(argv[i + 1]);
                    if (ret_code) {
                        goto done;
                    }
                }
            }
        }
        writer.SetSecSettingsSign(writer.GetSigningCertHandle() != 0);
        if (writer.GetSecSettingsSign()) {
            if (!hasHA) {
                writer.SetSecSettingsHashAlgorithm("SHA256");
            }
            if (!hasSF) {
                writer.SetSecSettingsSignatureFormat(0);
            }
        }
        writer.SetSecSettingsEncrypt(writer.GetEncryptionCertCount() > 0);
        if (writer.GetSecSettingsEncrypt() && !hasEA) {
            writer.SetSecSettingsEncryptionAlgorithm("AES128");
        }

        ret_code = writer.SaveToFile(argv[12]);
        if (ret_code == 0) {
            printf("A message has been assembled and saved successfully.\n");
        } else {
            printf("Failed to assemble and/or save a message.\n");
        }
    done:
        if (ret_code) // Got an error.  The user is done.
        {
            printf("\nError: %d\n", ret_code);
            if (writer.GetLastError()) {
                printf(" \"%s\"\n", writer.GetLastError());
            }
        }
        printf("Exiting... (press enter)\n");
        getchar();
        exit(ret_code);
    }
}


