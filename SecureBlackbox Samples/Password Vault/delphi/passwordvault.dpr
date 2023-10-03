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

program passwordvault;

uses
  Forms,
  changekeyf in 'changekeyf.pas'   {FormChangekeyf},
  editvaluef in 'editvaluef.pas'   {FormEditvaluef},
  newentryf in 'newentryf.pas'   {FormNewentryf},
  passwordf in 'passwordf.pas'   {FormPasswordf},
  passwordvaultf in 'passwordvaultf.pas' {FormPasswordvault};

begin
  Application.Initialize;

  Application.CreateForm(TFormPasswordvault, FormPasswordvault);
  Application.CreateForm(TFormChangekey, FormChangekey);

  Application.CreateForm(TFormEditvalue, FormEditvalue);

  Application.CreateForm(TFormNewentry, FormNewentry);

  Application.CreateForm(TFormPassword, FormPassword);

  Application.Run;
end.


         
