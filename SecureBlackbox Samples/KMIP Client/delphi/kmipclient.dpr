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

program kmipclient;

uses
  Forms,
  addcertf in 'addcertf.pas'   {FormAddcertf},
  addkeyf in 'addkeyf.pas'   {FormAddkeyf},
  connpropsf in 'connpropsf.pas'   {FormConnpropsf},
  countrylist in 'countrylist.pas' ,
  createcertf in 'createcertf.pas'   {FormCreatecertf},
  createkeyf in 'createkeyf.pas'   {FormCreatekeyf},
  operationf in 'operationf.pas'   {FormOperationf},
  kmipclientf in 'kmipclientf.pas' {FormKmipclient};

begin
  Application.Initialize;

  Application.CreateForm(TFormKmipclient, FormKmipclient);
  Application.CreateForm(TFormAddcert, FormAddcert);

  Application.CreateForm(TFormAddkey, FormAddkey);

  Application.CreateForm(TFormConnprops, FormConnprops);



  Application.CreateForm(TFormCreatecert, FormCreatecert);

  Application.CreateForm(TFormCreatekey, FormCreatekey);

  Application.CreateForm(TFormOperation, FormOperation);

  Application.Run;
end.


         
