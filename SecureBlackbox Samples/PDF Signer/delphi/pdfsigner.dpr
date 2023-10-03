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

program pdfsigner;

uses
  Forms,
  pdfsignerf in 'pdfsignerf.pas' {FormPdfsigner};

begin
  Application.Initialize;

  Application.CreateForm(TFormPdfsigner, FormPdfsigner);
  Application.Run;
end.


         
