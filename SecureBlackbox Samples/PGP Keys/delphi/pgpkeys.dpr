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

program pgpkeys;

uses
  Forms,
  keyringf in 'keyringf.pas'   {FormKeyringf},
  passphraserequestf in 'passphraserequestf.pas'   {FormPassphraserequestf},
  wizardf in 'wizardf.pas'   {FormWizardf},
  pgpkeysf in 'pgpkeysf.pas' {FormPgpkeys};

begin
  Application.Initialize;

  Application.CreateForm(TFormPgpkeys, FormPgpkeys);
  Application.CreateForm(TFormKeyring, FormKeyring);

  Application.CreateForm(TFormPassphraserequest, FormPassphraserequest);

  Application.CreateForm(TFormWizard, FormWizard);

  Application.Run;
end.


         
