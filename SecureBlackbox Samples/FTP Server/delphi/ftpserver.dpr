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

program ftpserver;

uses
  Forms,
  authsettingsf in 'authsettingsf.pas'   {FormAuthsettingsf},
  connpropsf in 'connpropsf.pas'   {FormConnpropsf},
  serversettingsf in 'serversettingsf.pas'   {FormServersettingsf},
  setpasswordf in 'setpasswordf.pas'   {FormSetpasswordf},
  setpublickeyf in 'setpublickeyf.pas'   {FormSetpublickeyf},
  usersettingsf in 'usersettingsf.pas'   {FormUsersettingsf},
  ftpserverf in 'ftpserverf.pas' {FormFtpserver};

begin
  Application.Initialize;

  Application.CreateForm(TFormFtpserver, FormFtpserver);
  Application.CreateForm(TFormAuthsettings, FormAuthsettings);

  Application.CreateForm(TFormConnprops, FormConnprops);

  Application.CreateForm(TFormServersettings, FormServersettings);

  Application.CreateForm(TFormSetpassword, FormSetpassword);

  Application.CreateForm(TFormSetpublickey, FormSetpublickey);

  Application.CreateForm(TFormUsersettings, FormUsersettings);

  Application.Run;
end.


         
