(*
 * SecureBlackbox 2022 Delphi Edition - Sample Project
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

program archivereader;

uses
  Forms,
  certificatef in 'certificatef.pas'   {FormCertificatef},
  openarchivef in 'openarchivef.pas'   {FormOpenarchivef},
  passwordf in 'passwordf.pas'   {FormPasswordf},
  progressf in 'progressf.pas'   {FormProgressf},
  usignf in 'usignf.pas'   {FormUsignf},
  archivereaderf in 'archivereaderf.pas' {FormArchivereader};

begin
  Application.Initialize;

  Application.CreateForm(TFormArchivereader, FormArchivereader);
  Application.CreateForm(TFormCertificate, FormCertificate);

  Application.CreateForm(TFormOpenarchive, FormOpenarchive);

  Application.CreateForm(TFormPassword, FormPassword);

  Application.CreateForm(TFormProgress, FormProgress);

  Application.CreateForm(TFormUsign, FormUsign);

  Application.Run;
end.


         
