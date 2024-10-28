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

program soapsigner;

uses
  Forms,
  referencef in 'referencef.pas'   {FormReferencef},
  referencesf in 'referencesf.pas'   {FormReferencesf},
  soapsignerf in 'soapsignerf.pas' {FormSoapsigner};

begin
  Application.Initialize;

  Application.CreateForm(TFormSoapsigner, FormSoapsigner);
  Application.CreateForm(TFormReference, FormReference);

  Application.CreateForm(TFormReferences, FormReferences);

  Application.Run;
end.


         
