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
unit messagedecompressorf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxMessageDecompressor;

type
  TFormMessagedecompressor = class(TForm)
    lbInputFile: TLabel;
    edInputFile: TEdit;
    sbBrowseInputFile: TSpeedButton;
    dlgOpenFile: TOpenDialog;
    lOutputFile: TLabel;
    edOutputFile: TEdit;
    sbOutputFile: TSpeedButton;
    btnDecompress: TButton;
    dlgSaveFile: TSaveDialog;
    Label10: TLabel;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnDecompressClick(Sender: TObject);
  private
    { Private declarations }
    FDecompressor: TsbxMessageDecompressor;
  public
    { Public declarations }
  end;

var
  FormMessagedecompressor: TFormMessagedecompressor;

implementation

{$R *.dfm}

uses resultsf;

function BinaryToString(const Buffer : TBytes): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to Length(Buffer) - 1 do
    Result := Result + IntToHex(Buffer[i], 1);
end;

procedure TFormMessagedecompressor.btnDecompressClick(Sender: TObject);
var
  SigOK: boolean;
begin
  try
    FDecompressor.InputFile := edInputFile.Text;
    FDecompressor.OutputFile := edOutputFile.Text;

    FDecompressor.Decompress;

    FormResults.mResults.Clear;
    FormResults.mResults.Lines.Add('Successfully decompressed!');
    FormResults.mResults.Lines.Add('');
    FormResults.mResults.Lines.Add('Content Type: ' + FDecompressor.Config('ContentType'));

    FormResults.ShowModal;
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormMessagedecompressor.FormCreate(Sender: TObject);
begin
  FDecompressor := TsbxMessageDecompressor.Create(nil);
end;

procedure TFormMessagedecompressor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDecompressor);
end;

procedure TFormMessagedecompressor.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpenFile.FileName := edInputFile.Text;
  if dlgOpenFile.Execute then
    edInputFile.Text := dlgOpenFile.FileName;
end;

procedure TFormMessagedecompressor.sbOutputFileClick(Sender: TObject);
begin
  dlgSaveFile.FileName := edOutputFile.Text;
  if dlgSaveFile.Execute then
    edOutputFile.Text := dlgSaveFile.FileName;
end;

end.







