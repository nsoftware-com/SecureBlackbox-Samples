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

class MyMailReader : public MailReader {
    int FireDecryptionInfoNeeded(MailReaderDecryptionInfoNeededEventParams* e) override {
        printf("************************************************************************\n");
        printf("Decryption needed! Please try again with correct path to certificate - the 2nd argument\n");
        printf("************************************************************************\n");
        printf("Exiting... (press enter)\n");
        getchar();
        exit(0);
    };
};

int main(int argc, char* argv[]) {

    if (argc < 2) {
        fprintf(stderr, "\n");
        fprintf(stderr, "  filename     path to the recorded letter\n");
        fprintf(stderr, "  certificate  path to the decryption certificate(If you need a certificate.)\n");
        fprintf(stderr, "  password     password of the decryption certificate(If you need a certificate.)\n");
        fprintf(stderr, "  \nExample    mailreader filename\n");
        fprintf(stderr, "  \nExample    mailreader filename certificate 1234\n");
        printf("Press enter to continue.\n");
        getchar();
    } else {
        CertificateManager manager;
        MyMailReader reader;
        int ret_code = 0;

        if (argc > 2) {
            ret_code = manager.ImportFromFile(argv[2], argc > 3 ? argv[3] : "");
            if (ret_code == 0) {
                printf("Certificate loaded successfully!\n");
                reader.SetDecryptionCertHandle(manager.GetCertHandle());
            } else {
                printf("Failed to load a certificate.\n");
                goto done;
            }
        }

        if (ret_code = reader.LoadFromFile(argv[1])) {
            goto done;
        };

        printf("\nSender = %s\n", reader.GetMsgSender());
        printf("From = %s\n", reader.GetMsgFrom());
        printf("SendTo = %s\n", reader.GetMsgSendTo());
        printf("CC = %s\n", reader.GetMsgCc());
        printf("BCC = %s\n", reader.GetMsgBcc());
        printf("\n\t\t***Security Info***\n");
        printf("Encrypted = %s\n", reader.GetSecInfoEncrypted() ? "true" : "false");
        if (ret_code = reader.GetSecInfoEncrypted()) {
            printf("Decryption Certificate = ");
            if (reader.GetDecryptionCertHandle() == NULL) {
                printf("[certificate not provided]\n");
            } else {
                printf("%s\n", reader.GetDecryptionCertSubjectRDN());
            }
            printf("Encryption algorithm = %s\n", reader.GetSecInfoEncryptionAlgorithm());

        }
        printf("Signed = %s\n", reader.GetSecInfoSigned() ? "true" : "false");
        if (reader.GetSecInfoSigned()) {
            printf("Signing Certificate = ");
            if (ret_code = reader.GetSigningCertHandle() == NULL) {
                printf("[certificate not found]\n");
            } else {
                printf("%s\n", reader.GetSigningCertSubjectRDN());
            }
            printf("Signature validation = ");
            switch (reader.GetSecInfoSignatureValidationResult()) {
            case 0:
                printf("VALID\n");
                break;

            case 1:
                printf("UNKNOWN\n");
                break;

            case 2:
                printf("CORRUPTED\n");
                break;

            case 3:
                printf("SIGNER NOT FOUND\n");
                break;

            case 4:
                printf("FAILURE\n");
                break;

            default:
                goto done;
                break;
            }

            printf("Hash algorithm = %s\n", reader.GetSecInfoHashAlgorithm());
        }
        printf("\t***End Security Info Block***\n\n");

        printf("Subject = %s\n", reader.GetMsgSubject());
        printf("Priority = ");
        switch (reader.GetMsgPriority()) {
        case 0:
            printf("LOWEST\n");
            break;

        case 1:
            printf("LOW\n");
            break;

        case 2:
            printf("NORMAL\n");
            break;

        case 3:
            printf("HIGH\n");
            break;

        case 4:
            printf("HIGHEST\n");
            break;

        default:
            goto done;
            break;
        }
        printf("Delivery receipt = %s\n", reader.GetMsgDeliveryReceipt() ? "true" : "false");
        printf("Read receipt = %s\n", reader.GetMsgReadReceipt() ? "true" : "false");
        printf("\n\t\t***Plain Text***\n");
        printf("%s\n", reader.GetMsgPlainText());
        printf("\t\t***Html Text***\n");
        printf("%s\n", reader.GetMsgHtmlText());
        printf("\nAttachments:\n");
        for (int i = 0; i < reader.GetMsgAttachmentCount(); i++) {
            printf("%s\t\tSize = %lld\n", reader.GetAttachFilename(i), reader.GetAttachSize(i));
        }
        printf("\n**************************END**************************\n\n");

    done:
        if (ret_code) // Got an error.  The user is done.
        {
            printf("\nError: %d\n", ret_code);
            if (reader.GetLastError()) {
                printf(" \"%s\"\n", reader.GetLastError());
            }
        }
        printf("Exiting... (press enter)\n");
        getchar();
        exit(ret_code);
    }
}


