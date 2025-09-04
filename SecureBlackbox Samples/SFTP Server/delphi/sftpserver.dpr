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

program sftpserver;

uses
  Forms,
  adduserf in 'adduserf.pas'   {FormAdduserf},
  connpropsf in 'connpropsf.pas'   {FormConnpropsf},
  settingsf in 'settingsf.pas'   {FormSettingsf},
  sftpserverf in 'sftpserverf.pas' {FormSftpserver};

begin
  Application.Initialize;

  Application.CreateForm(TFormSftpserver, FormSftpserver);
  Application.CreateForm(TFormAdduser, FormAdduser);

  Application.CreateForm(TFormConnprops, FormConnprops);

  Application.CreateForm(TFormSettings, FormSettings);

  Application.Run;
end.


         
