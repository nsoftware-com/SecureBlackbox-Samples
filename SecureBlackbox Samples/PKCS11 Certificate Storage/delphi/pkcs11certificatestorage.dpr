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

program pkcs11certificatestorage;

uses
  Forms,
  pdfprocessorf in 'pdfprocessorf.pas'   {FormPdfprocessorf},
  pinf in 'pinf.pas'   {FormPinf},
  validationresultf in 'validationresultf.pas'   {FormValidationresultf},
  pkcs11certificatestoragef in 'pkcs11certificatestoragef.pas' {FormPkcs11certificatestorage};

begin
  Application.Initialize;

  Application.CreateForm(TFormPkcs11certificatestorage, FormPkcs11certificatestorage);
  Application.CreateForm(TFormPdfprocessor, FormPdfprocessor);

  Application.CreateForm(TFormPin, FormPin);

  Application.CreateForm(TFormValidationresult, FormValidationresult);

  Application.Run;
end.


         
