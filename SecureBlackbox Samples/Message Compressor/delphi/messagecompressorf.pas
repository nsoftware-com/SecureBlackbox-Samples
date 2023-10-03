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
unit messagecompressorf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxMessageCompressor;

type
  TFormMessagecompressor = class(TForm)
    lbInputFile: TLabel;
    edInputFile: TEdit;
    sbBrowseInputFile: TSpeedButton;
    dlgOpenFile: TOpenDialog;
    Label1: TLabel;
    edOutputFile: TEdit;
    sbOutputFile: TSpeedButton;
    dlgSaveFile: TSaveDialog;
    btnCompress: TButton;
    Label10: TLabel;
    GroupBox1: TGroupBox;
    lbSymmetricAlgorithm: TLabel;
    cbCompressionLevel: TComboBox;
    Label2: TLabel;
    edContentType: TEdit;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCompressClick(Sender: TObject);
  private
    { Private declarations }
    FMessageCompressor: TsbxMessageCompressor;
  public
    { Public declarations }
  end;

var
  FormMessagecompressor: TFormMessagecompressor;

implementation

{$R *.dfm}

procedure TFormMessagecompressor.btnCompressClick(Sender: TObject);
begin
  FMessageCompressor.InputFile := edInputFile.Text;
  FMessageCompressor.OutputFile := edOutputFile.Text;

  FMessageCompressor.CompressionLevel := cbCompressionLevel.ItemIndex + 1;
  FMessageCompressor.Config('ContentType=' + edContentType.Text);

  try
    FMessageCompressor.Compress();

    MessageDlg('The file successfully compressed', mtInformation, [mbOk], 0);
  except
    on E: Exception do
    begin
      MessageDlg(e.message, mtError, [mbOk], 0);
    end;
  end;
end;

procedure TFormMessagecompressor.FormCreate(Sender: TObject);
begin
  FMessageCompressor := TsbxMessageCompressor.Create(nil);
end;

procedure TFormMessagecompressor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMessageCompressor);
end;

procedure TFormMessagecompressor.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpenFile.FileName := edInputFile.Text;
  if dlgOpenFile.Execute then
    edInputFile.Text := dlgOpenFile.FileName;
end;

procedure TFormMessagecompressor.sbOutputFileClick(Sender: TObject);
begin
  if dlgSaveFile.Execute then
    edOutputFile.Text := dlgSaveFile.FileName;
end;

end.








