(*
 * SecureBlackbox 2024 Delphi Edition - Sample Project
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
 *)

program archivewriter;

uses
  Forms,
  certificatef in 'certificatef.pas'   {FormCertificatef},
  newarchivef in 'newarchivef.pas'   {FormNewarchivef},
  openarchivef in 'openarchivef.pas'   {FormOpenarchivef},
  passwordf in 'passwordf.pas'   {FormPasswordf},
  progressf in 'progressf.pas'   {FormProgressf},
  archivewriterf in 'archivewriterf.pas' {FormArchivewriter};

begin
  Application.Initialize;

  Application.CreateForm(TFormArchivewriter, FormArchivewriter);
  Application.CreateForm(TFormCertificate, FormCertificate);

  Application.CreateForm(TFormNewarchive, FormNewarchive);

  Application.CreateForm(TFormOpenarchive, FormOpenarchive);

  Application.CreateForm(TFormPassword, FormPassword);

  Application.CreateForm(TFormProgress, FormProgress);

  Application.Run;
end.


         
