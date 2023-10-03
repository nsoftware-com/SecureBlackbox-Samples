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
unit messagetimestamperf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxMessageTimestamper;

type
  TFormMessagetimestamper = class(TForm)
    lbInputFile: TLabel;
    edInputFile: TEdit;
    sbBrowseInputFile: TSpeedButton;
    dlgOpenFile: TOpenDialog;
    Label1: TLabel;
    edOutputFile: TEdit;
    sbOutputFile: TSpeedButton;
    dlgSaveFile: TSaveDialog;
    btnTimestamp: TButton;
    Label10: TLabel;
    GroupBox1: TGroupBox;
    cbDetached: TCheckBox;
    Label2: TLabel;
    edTimestampServer: TEdit;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTimestampClick(Sender: TObject);
  private
    { Private declarations }
    FMessageTimestamper: TsbxMessageTimestamper;
  public
    { Public declarations }
  end;

var
  FormMessagetimestamper: TFormMessagetimestamper;

implementation

{$R *.dfm}

procedure TFormMessagetimestamper.btnTimestampClick(Sender: TObject);
begin
  FMessageTimestamper.InputFile := edInputFile.Text;
  FMessageTimestamper.OutputFile := edOutputFile.Text;

  FMessageTimestamper.Detached := cbDetached.Checked;
  FMessageTimestamper.TimestampServer := edTimestampServer.Text;

  try
    FMessageTimestamper.Timestamp();

    MessageDlg('The file successfully timestamped', mtInformation, [mbOk], 0);
  except
    on E: Exception do
    begin
      MessageDlg(e.message, mtError, [mbOk], 0);
    end;
  end;
end;

procedure TFormMessagetimestamper.FormCreate(Sender: TObject);
begin
  FMessageTimestamper := TsbxMessageTimestamper.Create(nil);
end;

procedure TFormMessagetimestamper.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMessageTimestamper);
end;

procedure TFormMessagetimestamper.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpenFile.FileName := edInputFile.Text;
  if dlgOpenFile.Execute then
    edInputFile.Text := dlgOpenFile.FileName;
end;

procedure TFormMessagetimestamper.sbOutputFileClick(Sender: TObject);
begin
  if dlgSaveFile.Execute then
    edOutputFile.Text := dlgSaveFile.FileName;
end;

end.








