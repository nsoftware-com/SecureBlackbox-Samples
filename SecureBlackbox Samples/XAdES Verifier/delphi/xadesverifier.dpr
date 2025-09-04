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

program xadesverifier;

uses
  Forms,
  referencesf in 'referencesf.pas'   {FormReferencesf},
  usignf in 'usignf.pas'   {FormUsignf},
  xadesf in 'xadesf.pas'   {FormXadesf},
  xadesverifierf in 'xadesverifierf.pas' {FormXadesverifier};

begin
  Application.Initialize;

  Application.CreateForm(TFormXadesverifier, FormXadesverifier);
  Application.CreateForm(TFormReferences, FormReferences);

  Application.CreateForm(TFormUsign, FormUsign);

  Application.CreateForm(TFormXades, FormXades);

  Application.Run;
end.


         
