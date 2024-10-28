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

program sftpclient;

uses
  Forms,
  connpropsf in 'connpropsf.pas'   {FormConnpropsf},
  progressf in 'progressf.pas'   {FormProgressf},
  promptf in 'promptf.pas'   {FormPromptf},
  sftpclientf in 'sftpclientf.pas' {FormSftpclient};

begin
  Application.Initialize;

  Application.CreateForm(TFormSftpclient, FormSftpclient);
  Application.CreateForm(TFormConnprops, FormConnprops);

  Application.CreateForm(TFormProgress, FormProgress);

  Application.CreateForm(TFormPrompt, FormPrompt);

  Application.Run;
end.


         
