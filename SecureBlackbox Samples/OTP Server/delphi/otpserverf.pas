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
unit otpserverf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  SBxOTPServer;

type
  TFormOTPServer = class(TForm)
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
    lHashAlgorithm: TLabel;
    eInterval: TEdit;
    cbHashAlgorithm: TComboBox;
    cUseBaseTime: TCheckBox;
    eBaseDate: TDateTimePicker;
    eBaseTime: TDateTimePicker;
    GroupBox3: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    bValidate: TButton;
    ePassword: TEdit;
    eDelta: TEdit;
    procedure cbAlgorithmChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bValidateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOTPServer: TFormOTPServer;

implementation

{$R *.dfm}

procedure TFormOTPServer.bValidateClick(Sender: TObject);
var
  Server: TsbxOTPServer;
  HashAlgorithm: string;
  Res: boolean;
begin
  Server := TsbxOTPServer.Create(nil);
  try
    Server.Delta := StrToIntDef(eDelta.Text, 0);

    if cbAlgorithm.ItemIndex = 0 then
      Res := Server.IsHOTPPasswordValid(TEncoding.UTF8.GetBytes(eKeySecret.Text),
        StrToInt(ePassLen.Text), StrToInt(eInterval.Text), ePassword.Text)
    else
    begin
      if cUseBaseTime.Checked then
        Server.BaseTime := DateToStr(eBaseDate.Date) + ' ' + TimeToStr(eBaseTime.Time)
      else
        Server.BaseTime := '';

      case cbHashAlgorithm.ItemIndex of
        1: HashAlgorithm := 'SHA256';
        2: HashAlgorithm := 'SHA512';
        else HashAlgorithm := 'SHA1';
      end;

      Res := Server.IsTOTPPasswordValid(TEncoding.UTF8.GetBytes(eKeySecret.Text),
        StrToInt(ePassLen.Text), StrToInt(eInterval.Text), HashAlgorithm, ePassword.Text);
    end;

    if Res then
      MessageDlg('Password is valid', mtInformation, [mbOK], 0)
    else
      MessageDlg('Password is not valid', mtWarning, [mbOK], 0);
  finally
    FreeAndNil(Server);
  end;
end;

procedure TFormOTPServer.cbAlgorithmChange(Sender: TObject);
begin
  case cbAlgorithm.ItemIndex of
    0:
    begin
      lInterval.Caption := 'Counter:';
      eInterval.Text := '0';
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
      eInterval.Text := '30';
      lHashAlgorithm.Enabled := true;
      cbHashAlgorithm.Enabled := true;
      cUseBaseTime.Enabled := true;
      eBaseDate.Enabled := true;
      eBaseTime.Enabled := true;
    end;
  end;
end;

procedure TFormOTPServer.FormCreate(Sender: TObject);
begin
  cbAlgorithmChange(nil);
  eBaseDate.DateTime := Now();
  eBaseTime.DateTime := Now();
end;

end.





