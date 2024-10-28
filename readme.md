# SecureBlackbox Sample Projects
The provided sample projects offer a concise yet effective demonstration of the components from SecureBlackbox, showcasing their usage in a straightforward manner. While they are not intended to be complete applications, they serve as valuable illustrations of key features. To enhance clarity, error handling and checks are simplified, allowing users to focus on the core functionality.

## Requirements
SecureBlackbox 2024 libraries for the corresponding edition are required to run these samples.  Free download from: [https://www.nsoftware.com/secureblackbox](https://www.nsoftware.com/secureblackbox)

## Reference Documentation
* [Python Edition](https://cdn.nsoftware.com/help/SBJ/py/)
* [PHP Edition](https://cdn.nsoftware.com/help/SBJ/php/)
* [C++ Edition](https://cdn.nsoftware.com/help/SBJ/cpp/)
* [Delphi Edition](https://cdn.nsoftware.com/help/SBJ/dlp/)
* [macOS Edition](https://cdn.nsoftware.com/help/SBJ/mac/)
* [iOS Edition](https://cdn.nsoftware.com/help/SBJ/mac/)

## Sample Projects
| Sample Project | Description |
| --- | --- |
| [ASiC Signer](./SecureBlackbox%20Samples/ASiC%20Signer) | A simple ASiC signer sample created with the ASiCSigner component. Use it to create XAdES-signed, CAdES-signed, and timestamped archives. |
| [ASiC Verifier](./SecureBlackbox%20Samples/ASiC%20Verifier) | A simple ASiC verifier created with the ASiCVerifier component. Use it to verify ASiC signatures. |
| [Archive Reader](./SecureBlackbox%20Samples/Archive%20Reader) | A simple Archive Reader sample created with the ArchiveReader component. Use it to read and extract files from archives. |
| [Archive Writer](./SecureBlackbox%20Samples/Archive%20Writer) | A simple Archive Writer sample created with the ArchiveWriter component. Use it to create and modify archives. |
| [Authenticator](./SecureBlackbox%20Samples/Authenticator) | A simple Authenticator created with the Authenticator component. Use it to user authentication. |
| [Authenticode Signer](./SecureBlackbox%20Samples/Authenticode%20Signer) | A simple authenticode signer created with the AuthenticodeSigner component. Use it to sign EXE and DLL files in accordance with MS Authenticode technology. |
| [Authenticode Verifier](./SecureBlackbox%20Samples/Authenticode%20Verifier) | A simple authenticode verifier based on the AuthenticodeVerifier component. Use it to verify signed EXE and DLL files. |
| [CAdES Signer](./SecureBlackbox%20Samples/CAdES%20Signer) | A simple CAdES generator created with the CAdESSigner component. The sample supports creation of CAdES signatures of different conformance levels. |
| [CAdES Verifier](./SecureBlackbox%20Samples/CAdES%20Verifier) | A simple CAdES processor created around the CAdESVerifier component. |
| [DCAuth Service](./SecureBlackbox%20Samples/DCAuth%20Service) | A simple example of the DC technology. The sample incorporates two counterparts of DC: the application part is represented with PDFSigner control, and the private key part is represented with DCAuth control. |
| [DCAuth WebServer](./SecureBlackbox%20Samples/DCAuth%20WebServer) | A simple example of the DC technology. The sample incorporates two counterparts of DC: the application part is represented with PDFSigner control, and the private key part is represented with DCAuth control. |
| [DTLS Client](./SecureBlackbox%20Samples/DTLS%20Client) | A simple DTLS client sample. Use this demo together with its DTLS Server counterpart. |
| [DTLS Server](./SecureBlackbox%20Samples/DTLS%20Server) | A simple DTLS server sample. Use this demo together with its DTLS Client counterpart. |
| [Distributed Crypto](./SecureBlackbox%20Samples/Distributed%20Crypto) | A simple example of the DC technology. The sample incorporates two counterparts of DC: the application part is represented with PDFSigner control, and the private key part is represented with DCAuth control. |
| [FTP Client](./SecureBlackbox%20Samples/FTP%20Client) | A simple FTP client implemented around the FTPClient control. A number of core FTP operations, as well as various TLS modes, are supported. |
| [FTP Server](./SecureBlackbox%20Samples/FTP%20Server) | A fully functional FTP server application built around the FTPServer component. Core file operations, user management, and various TLS modes are supported. |
| [HTTP Get](./SecureBlackbox%20Samples/HTTP%20Get) | This tiny sample illustrates the HTTP GET functionality. |
| [HTTP Post](./SecureBlackbox%20Samples/HTTP%20Post) | A simple HTTP POST example. |
| [HTTP Server](./SecureBlackbox%20Samples/HTTP%20Server) | A basic HTTP server application built around the HTTPServer component. Straightforward HTTP request handling is supported. |
| [Hash Function](./SecureBlackbox%20Samples/Hash%20Function) | Use this example to learn about calculate hash with HashFunction control. |
| [IMAP Client](./SecureBlackbox%20Samples/IMAP%20Client) | A small mail receiving program based on the IMAPClient component. It can list mailboxes on the server, list messages in the selected mailbox, download them from the server and upload local messages to the server. |
| [JAdES Signer](./SecureBlackbox%20Samples/JAdES%20Signer) | A simple example of creating JWS/JAdES signature with JAdESSigner control. |
| [JAdES Verifier](./SecureBlackbox%20Samples/JAdES%20Verifier) | Use this demo to learn how to verify JWS/JAdES signature using the JAdESVerifier control. |
| [JWC Encryptor](./SecureBlackbox%20Samples/JWC%20Encryptor) | Use this example to learn about encrypting and decrypting messages in JSON format with SymmetricCrypto control. |
| [JWC Signer](./SecureBlackbox%20Samples/JWC%20Signer) | Use this example to learn about signing and verifying messages in JSON format with PublicKeyCrypto control. |
| [KMIP Client](./SecureBlackbox%20Samples/KMIP%20Client) | A powerful KMIP client sample built around the KMIPClient component. Most of KMIP functionality, such as certificate management and cryptographic operations, is supported. |
| [KMIP Server](./SecureBlackbox%20Samples/KMIP%20Server) | A powerful KMIP server sample built with the KMIPServer component. Most of KMIP functionality, such as certificate management and cryptographic operations, is supported. |
| [Mail Reader](./SecureBlackbox%20Samples/Mail%20Reader) | A simple e-mail messages parsing and reading demo based on the MailReader component. |
| [Mail Writer](./SecureBlackbox%20Samples/Mail%20Writer) | A simple e-mail messages creation demo based on the MailWriter component. |
| [Message Compressor](./SecureBlackbox%20Samples/Message%20Compressor) | A simple example of PKCS7-compliant message compressing functionality. |
| [Message Decompressor](./SecureBlackbox%20Samples/Message%20Decompressor) | This small example illustrates the PKCS7-compliant message decompressing functionality. |
| [Message Decryptor](./SecureBlackbox%20Samples/Message%20Decryptor) | A lightweight example of PKCS7 messaged decryption, built around the MessageDecryptor component. |
| [Message Encryptor](./SecureBlackbox%20Samples/Message%20Encryptor) | This small demo illustrates the use of PKCS7 certificate-based messaged encryption functionality. |
| [Message Signer](./SecureBlackbox%20Samples/Message%20Signer) | Learn how to implement PKCS7 signing in your application with this simple example. MessageSigner is a simpler version of CAdESSigner, which excludes the AdES piece. |
| [Message Timestamp Verifier](./SecureBlackbox%20Samples/Message%20Timestamp%20Verifier) | This small demo shows how to validate PKCS7 timestamped messages with the MessageVerifier class. |
| [Message Timestamper](./SecureBlackbox%20Samples/Message%20Timestamper) | This example illustrates the creation of PKCS7 timestamped messages. |
| [Message Verifier](./SecureBlackbox%20Samples/Message%20Verifier) | This sample illustrates the verification of signed PKCS7 documents. For advanced validations that include certificate chain processing see CAdESVerifier. |
| [OAuth Client](./SecureBlackbox%20Samples/OAuth%20Client) | A small sample that shows how to authorize the users using OAuth 2.0 servers. |
| [OTP Client](./SecureBlackbox%20Samples/OTP%20Client) | A very simple OTP password generator. TOTP and HOTP protocols are supported. |
| [OTP Server](./SecureBlackbox%20Samples/OTP%20Server) | A simple OTP password generator (server side). HOTP and TOTP protocols are supported. |
| [Office Decryptor](./SecureBlackbox%20Samples/Office%20Decryptor) | A very simple office document decryptor app built using the OfficeDecryptor control. |
| [Office Encryptor](./SecureBlackbox%20Samples/Office%20Encryptor) | A lightweight encryptor of Office documents. |
| [Office Signer](./SecureBlackbox%20Samples/Office%20Signer) | A simple example of Office document signing with OfficeSigner control. |
| [Office Verifier](./SecureBlackbox%20Samples/Office%20Verifier) | Use this demo to learn how to verify signed Office document using the OfficeVerifier control. |
| [PDF Decryptor](./SecureBlackbox%20Samples/PDF%20Decryptor) | A simple PDF decryption example. Both certificate- and password-encrypted document types are supported. |
| [PDF Encryptor](./SecureBlackbox%20Samples/PDF%20Encryptor) | A tiny PDF encryption example which supports password- and certificate-based encryption. |
| [PDF Signer](./SecureBlackbox%20Samples/PDF%20Signer) | An easy-to-use PDF signing example. Both generic and PAdES signatures are supported. |
| [PDF Signer External](./SecureBlackbox%20Samples/PDF%20Signer%20External) | An easy-to-use PDF signing example. Both generic and PAdES signatures are supported. |
| [PDF Signer with PKI Proxy](./SecureBlackbox%20Samples/PDF%20Signer%20with%20PKI%20Proxy) | PDF signing with PKI Proxy example. |
| [PDF Verifier](./SecureBlackbox%20Samples/PDF%20Verifier) | This small demo illustrates the use of the PDFVerifier control for processing PDF signatures. |
| [PGP Keys](./SecureBlackbox%20Samples/PGP%20Keys) | A simple PGP keyring manager. Generation and browsing of PGP keys are supported. |
| [PGP Reader](./SecureBlackbox%20Samples/PGP%20Reader) | Use this easy-to-use example to learn about integrating PGP decryption and verification into your application. |
| [PGP Writer](./SecureBlackbox%20Samples/PGP%20Writer) | A simple PGP encryptor-plus-verifier. |
| [PKCS11 Certificate Storage](./SecureBlackbox%20Samples/PKCS11%20Certificate%20Storage) | An easy-to-use Certificate Storage for work with PKCS11 storages. |
| [POP Client](./SecureBlackbox%20Samples/POP%20Client) | A small mail receiving program based on the POP3Client component. It can connect to a POP3 server and receive messages from the mailbox or delete messages directly on the server. |
| [Password Vault](./SecureBlackbox%20Samples/Password%20Vault) | A simple Password Vault to save user's information and passwords. |
| [Public Key Crypto](./SecureBlackbox%20Samples/Public%20Key%20Crypto) | Use this example to learn about sign and verify with PublicKeyCrypto control. |
| [REST Server](./SecureBlackbox%20Samples/REST%20Server) | The sample shows interaction between a web page and a simple REST server built around the RESTServer control. |
| [SAML IdP Server](./SecureBlackbox%20Samples/SAML%20IdP%20Server) | A simple SAML IdP server built with the SAMLIdPServer component. The sample supports creation of the identity provider and user authorization for SAML protocol. |
| [SAML Reader](./SecureBlackbox%20Samples/SAML%20Reader) | A simple SAML Reader built with the SAMLReader control. This demo illustrates the use of SAMLReader to read and decompose SAML messages. |
| [SAML SP Server](./SecureBlackbox%20Samples/SAML%20SP%20Server) | A simple SAML SP server built with the SAMLSPServer control. The sample illustrates creation of a SAML-protected web server. |
| [SAML Writer](./SecureBlackbox%20Samples/SAML%20Writer) | A simple SAML Writer built with the SAMLWriter control. This demo illustrates how to use SAMLWriter to create SAML messages. |
| [SFTP Client](./SecureBlackbox%20Samples/SFTP%20Client) | A small but powerful SFTP client built with the SFTPClient component. Directory browsing, uploads, and downloads are supported. |
| [SFTP Server](./SecureBlackbox%20Samples/SFTP%20Server) | A powerful SFTP server created around the SFTPServer component. Most of SFTP operations are supported. |
| [SMTP Client](./SecureBlackbox%20Samples/SMTP%20Client) | A small mail sender program based on the SMTPClient component. Just creates a plain text message and sends it to the specified SMTP server. |
| [SOAP Client](./SecureBlackbox%20Samples/SOAP%20Client) | This small example illustrates the sending of SOAP messages with SOAPClient control. |
| [SOAP Signer](./SecureBlackbox%20Samples/SOAP%20Signer) | This small example illustrates the signing of SOAP messages with SOAPSigner control. |
| [SOAP Verifier](./SecureBlackbox%20Samples/SOAP%20Verifier) | Use this example to learn about SOAP signature validation with SOAPVerifier control. |
| [Simple Authenticator](./SecureBlackbox%20Samples/Simple%20Authenticator) | A simple Authenticator created with the Authenticator component. Use it to user authentication. |
| [Simple PDF Signer](./SecureBlackbox%20Samples/Simple%20PDF%20Signer) | An easy-to-use PDF signing example. Supported PKCS11 and Win32 storages. |
| [Symmetric Crypto](./SecureBlackbox%20Samples/Symmetric%20Crypto) | Use this example to learn about encrypt and decrypt with SymmetricCrypto control. |
| [TLS Client](./SecureBlackbox%20Samples/TLS%20Client) | A simple TLS client sample. Use this demo together with its TLS Server counterpart. |
| [TLS Server](./SecureBlackbox%20Samples/TLS%20Server) | A simple TLS server sample. Use this demo together with its TLS Client counterpart. |
| [User Manager](./SecureBlackbox%20Samples/User%20Manager) | A simple User Manager to save user's information and passwords. |
| [WebDAV Server](./SecureBlackbox%20Samples/WebDAV%20Server) | A simple webdav server sample. |
| [WebSocket Server](./SecureBlackbox%20Samples/WebSocket%20Server) | This sample illustrates the interaction between a web page and a simple WebSocket server built around the WebSocketServer component. |
| [XAdES Signer](./SecureBlackbox%20Samples/XAdES%20Signer) | Use this demo to learn how to create signed XAdES documents of various levels. |
| [XAdES Verifier](./SecureBlackbox%20Samples/XAdES%20Verifier) | This small demo illustrates the use of XAdESVerifier for XAdES signature validations. |
| [XML Decryptor](./SecureBlackbox%20Samples/XML%20Decryptor) | A tiny XML decryption example. |
| [XML Encryptor](./SecureBlackbox%20Samples/XML%20Encryptor) | A tiny XML encryption example. |
| [XML Signer](./SecureBlackbox%20Samples/XML%20Signer) | This small example shows how to create basic XML signatures with XMLSigner control. See XAdESSigner for more sophisticated signatures. |
| [XML Verifier](./SecureBlackbox%20Samples/XML%20Verifier) | This sample demonstrates the use of XMLVerifier for validating basic XML signatures. For validations involving certificate chain checks, see XAdESVerifier. |

## Support
If you have questions or need help, please contact support@nsoftware.com or explore other support options 
at www.nsoftware.com.
