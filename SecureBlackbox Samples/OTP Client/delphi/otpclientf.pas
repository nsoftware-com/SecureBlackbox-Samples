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
unit otpclientf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  SBxOTPClient;

type
  TFormOTPClient = class(TForm)
    bGenerate: TButton;
    ePassword: TEdit;
    Label10: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    eKeySecret: TEdit;
    ePassLen: TEdit;
    cbAlgorithm: TComboBox;
    GroupBox2: TGroupBox;
    lInterval: TLabel;
    eInterval: TEdit;
    cbHashAlgorithm: TComboBox;
    lHashAlgorithm: TLabel;
    cUseBaseTime: TCheckBox;
    eBaseDate: TDateTimePicker;
    eBaseTime: TDateTimePicker;
    procedure cbAlgorithmChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bGenerateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOTPClient: TFormOTPClient;

implementation

{$R *.dfm}

procedure TFormOTPClient.bGenerateClick(Sender: TObject);
var
  Client: TsbxOTPClient;
  HashAlgorithm: string;
begin
  Client := TsbxOTPClient.Create(nil);
  try
    Client.KeySecret := TEncoding.UTF8.GetBytes(eKeySecret.Text);
    Client.PasswordLength := StrToInt(ePassLen.Text);

    if cbAlgorithm.ItemIndex = 0 then
      ePassword.Text := Client.GenerateHOTPPassword(StrToInt(eInterval.Text))
    else
    begin
      if cUseBaseTime.Checked then
        Client.Config('BaseTime=' + DateToStr(eBaseDate.Date) + ' ' + TimeToStr(eBaseTime.Time));

      case cbHashAlgorithm.ItemIndex of
        1: HashAlgorithm := 'SHA256';
        2: HashAlgorithm := 'SHA512';
        else HashAlgorithm := 'SHA1';
      end;

      ePassword.Text := Client.GenerateTOTPPassword(StrToInt(eInterval.Text), HashAlgorithm);
    end;
  finally
    FreeAndNil(Client);
  end;
end;

procedure TFormOTPClient.cbAlgorithmChange(Sender: TObject);
begin
  case cbAlgorithm.ItemIndex of
    0:
    begin
      lInterval.Caption := 'Counter:';
      lHashAlgorithm.Enabled := false;
      cbHashAlgorithm.Enabled := false;
      cbHashAlgorithm.ItemIndex := 0;
      cUseBaseTime.Enabled := false;
      eBaseDate.Enabled := false;
      eBaseTime.Enabled := false;
    end;

    1:
    begin
      lInterval.Caption := 'Time interval:';
      lHashAlgorithm.Enabled := true;
      cbHashAlgorithm.Enabled := true;
      cUseBaseTime.Enabled := true;
      eBaseDate.Enabled := true;
      eBaseTime.Enabled := true;
    end;
  end;
end;

procedure TFormOTPClient.FormCreate(Sender: TObject);
begin
  cbAlgorithmChange(nil);
  eBaseDate.DateTime := Now();
  eBaseTime.DateTime := Now();
end;

end.





