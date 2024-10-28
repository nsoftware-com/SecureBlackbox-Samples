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

program authenticator;

uses
  Forms,
  authverifyf in 'authverifyf.pas'   {FormAuthverifyf},
  changeauthmethodsf in 'changeauthmethodsf.pas'   {FormChangeauthmethodsf},
  changeauthresultf in 'changeauthresultf.pas'   {FormChangeauthresultf},
  continueauthenticationf in 'continueauthenticationf.pas'   {FormContinueauthenticationf},
  customauthstartf in 'customauthstartf.pas'   {FormCustomauthstartf},
  dcsettingsf in 'dcsettingsf.pas'   {FormDcsettingsf},
  newauthenticationf in 'newauthenticationf.pas'   {FormNewauthenticationf},
  authenticatorf in 'authenticatorf.pas' {FormAuthenticator};

begin
  Application.Initialize;

  Application.CreateForm(TFormAuthenticator, FormAuthenticator);
  Application.CreateForm(TFormAuthverify, FormAuthverify);

  Application.CreateForm(TFormChangeauthmethods, FormChangeauthmethods);

  Application.CreateForm(TFormChangeauthresult, FormChangeauthresult);

  Application.CreateForm(TFormContinueauthentication, FormContinueauthentication);

  Application.CreateForm(TFormCustomauthstart, FormCustomauthstart);

  Application.CreateForm(TFormDcsettings, FormDcsettings);

  Application.CreateForm(TFormNewauthentication, FormNewauthentication);

  Application.Run;
end.


         
