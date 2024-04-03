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
#include <string.h>
#include <stdlib.h>
#include "../../include/secureblackbox.h"
#define LINE_LEN 100

class MyFTP : public FTPClient
{
public:

    bool verbose = false;

    int FireTLSCertValidate(FTPClientTLSCertValidateEventParams* e) override {
        if (e->Accept) {
            return 0;
        }
        printf("Server provided the following certificate:\nIssuer: %s\nSubject: %s\n",
               this->GetServerCertIssuerRDN(0), this->GetServerCertSubjectRDN(0));
        printf("Would you like to continue? [y/n] ");
        if (getchar() == 'y') {
            e->Accept = true;
        } else {
            exit(0);
        }
        return 0;
    }

    int FireControlSend(FTPClientControlSendEventParams* e) override {
        if (verbose) printf("%s\r\n", e->TextLine);
        return 0;
    }

    int FireControlReceive(FTPClientControlReceiveEventParams* e) override {
        if (verbose) printf("%s\r\n", e->TextLine);
        return 0;
    }

    int FireListEntry(FTPClientListEntryEventParams* e) override {
        printf("  %s\r\n", GetCurrListEntryRawData());
        return 0;
    }
};

int main(int argc, char** argv) {
    MyFTP ftp;
    char command[LINE_LEN];
    char server[LINE_LEN];
    char portText[LINE_LEN];
    char* localFile;
    char* remoteFile;
    int port = 21;

    if (argc < 4) {
        fprintf(stderr, "usage: ftp server username password [sslmode]\n");
        fprintf(stderr, "\n");
        fprintf(stderr, "  server    the domain name or IP address of the FTP server\n");
        fprintf(stderr, "  username  the user identifier to use for login\n");
        fprintf(stderr, "  password  the password to log in.\n");
        fprintf(stderr, "  sslmode   The SSL mode. Possible values are: none, explicit, implicit.\n");
        fprintf(stderr, "\nExample (Plaintext):       ftp 127.0.0.1 username password\n");
        fprintf(stderr, "Example (Non-standard port): ftp 127.0.0.1:12345 username password explicit\n\n");
        fprintf(stderr, "Example (SSL Auto):          ftp 127.0.0.1 username password auto\n");
        fprintf(stderr, "Example (SSL Explicit):      ftp 127.0.0.1 username password explicit\n\n");
        printf("Press enter to continue.");
        getchar();
    } else {
        if (argc > 4) {
            if (!strcmp(argv[4], "none")) {
                ftp.SetTLSTLSMode(SM_NO_TLS);
            } else if (!strcmp(argv[4], "explicit")) {
                ftp.SetTLSTLSMode(SM_EXPLICIT_TLS);
            } else if (!strcmp(argv[4], "implicit")) {
                ftp.SetTLSTLSMode(SM_IMPLICIT_TLS);
            }
        }

        // check if non-standard port was provided.
        int idx = 0;
        while (argv[1][idx] != '\0') {
            if (argv[1][idx] == ':') {
                break;
            }
            idx++;
        }

        if (argv[1][idx] != '\0') {
            // get the server
            memcpy(server, argv[1], idx);
            server[idx] = '\0';

            // get the port
            memcpy(portText, &argv[1][idx + 1], strlen(argv[1]) - idx);
            portText[idx] = '\0';
            port = atoi(portText);
        } else {
            memcpy(server, argv[1], strlen(argv[1]));
            server[strlen(argv[1])] = '\0';
        }

        // connect
        printf("Connecting to ftp://%s@%s:%i\r\n", argv[2], server, port);
        ftp.SetUsername(argv[2]);
        ftp.SetPassword(argv[3]);
        ftp.SetTLSAutoValidateCertificates(FALSE);
        if (ftp.Connect(server, port)) {
            goto done;
        }

        // main loop to check for commands
        while (1) {
            printf("ftp> ");
            fgets(command, LINE_LEN, stdin);
            command[strlen(command) - 1] = '\0';
            char* argument = strtok(command, " \t\n");

            if (!strcmp(command, "ascii")) {
                ftp.SetTransferType(CTT_TEXT);
                printf("Transfer mode text.\r\n");
            } else if (!strcmp(command, "binary")) {
                ftp.SetTransferType(CTT_BINARY);
                printf("Transfer mode binary.\r\n");
            } else if (!strcmp(command, "put")) {
                localFile = strtok(nullptr, " \t\n");
                remoteFile = strtok(nullptr, " \t\n");
                if (ftp.UploadFile(localFile, remoteFile)) {
                    printf("Uploaded file: %s -> %s\r\n", localFile, remoteFile);
                } else {
                    printf("Failed to upload file.\r\n");
                }
            } else if (!strcmp(command, "get")) {
                remoteFile = strtok(nullptr, " \t\n");
                localFile = strtok(nullptr, " \t\n");
                if (ftp.DownloadFile(remoteFile, localFile)) {
                    printf("Downloaded file: %s -> %s\r\n", remoteFile, localFile);
                } else {
                    printf("Failed to download file.\r\n");
                }
            } else if (!strcmp(command, "cd")) {
                argument = strtok(nullptr, " \t\n");
                if (!ftp.ChangeDir(argument)) {
                    printf("Changed directory: %s\r\n", argument);
                } else {
                    printf("Failed to change directory.\r\n");
                }
            } else if (!strcmp(command, "ls")) {
                printf("Listing %s\r\n", ftp.GetCurrentDir());
                ftp.ListDir(1, 1);
            } else if (!strcmp(command, "mkdir")) {
                argument = strtok(nullptr, " \t\n");
                if (!ftp.MakeDir(argument)) {
                    printf("Created directory: %s\r\n", argument);
                } else {
                    printf("Failed to create directory.\r\n");
                }
            } else if (!strcmp(command, "pwd")) {
                printf("%s\r\n", ftp.GetCurrentDir());
            } else if (!strcmp(command, "rm")) {
                argument = strtok(nullptr, " \t\n");
                if (!ftp.DeleteFile(argument)) {
                    printf("Deleted file: %s\r\n", argument);
                } else {
                    printf("Failed to delete file.\r\n");
                }
            } else if (!strcmp(command, "rmdir")) {
                argument = strtok(nullptr, " \t\n");
                if (!ftp.DeleteDir(argument)) {
                    printf("Deleted directory: %s\r\n", argument);
                } else {
                    printf("Failed to delete directory.\r\n");
                }
            } else if (!strcmp(command, "verbose")) {
                if (ftp.verbose) {
                    ftp.verbose = false;
                    printf("Verbose mode off.\r\n");
                } else {
                    ftp.verbose = true;
                    printf("Verbose mode on.\r\n");
                }
            } else if (!strcmp(command, "bye") || !strcmp(command, "exit") || !strcmp(command, "quit")) {
                ftp.Disconnect();
                goto done;
            } else if (!strcmp(command, "?") || !strcmp(command, "help") || !strcmp(command, "man")) {
                goto helpcmds;
            } else {
                printf("Command recognized. Choose from these:\r\n\r\n");
            helpcmds:
                printf("?         cd        man       quit    \n"
                    "ascii     get       mkdir     rm      \n"
                    "binary    help      put       rmdir   \n"
                    "bye       ls        pwd       verbose \n");
            }
        }
    }

done:
    if (ftp.GetLastErrorCode()) {
        printf("Error with FTPClient: [%i] %s\r\n", ftp.GetLastErrorCode(), ftp.GetLastError());
    }
    printf("Goodbye.");
    getchar();
}


