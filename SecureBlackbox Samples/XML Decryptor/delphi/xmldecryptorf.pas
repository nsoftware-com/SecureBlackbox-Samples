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
unit xmldecryptorf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, xmlencpropsf,
  SBxXMLDecryptor;

type
  TFormXmldecryptor = class(TForm)
    lSourceFile: TLabel;
    editSource: TEdit;
    lDestFile: TLabel;
    editDest: TEdit;
    btnBrowseSource: TButton;
    btnBrowseDest: TButton;
    btnDecrypt: TButton;
    btnCancel: TButton;
    OpenDialogXML: TOpenDialog;
    SaveDialogXML: TSaveDialog;
    lDemoInfo: TLabel;
    procedure btnBrowseSourceClick(Sender: TObject);
    procedure btnBrowseDestClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    procedure SetFileContents(const FileName : string; const Content : TBytes);
    procedure HandleDecryptionInfoNeeded(Sender: TObject; var CancelDecryption: Boolean);
    procedure HandleSaveExternalData(Sender: TObject; ExternalData: TBytes);
  public
    { Public declarations }
  end;

var
  FormXmldecryptor: TFormXmldecryptor;

implementation

{$R *.DFM}

procedure TFormXmldecryptor.btnBrowseSourceClick(Sender: TObject);
begin
  OpenDialogXML.Filename := editSource.Text;
  OpenDialogXML.Filter := 'XML files (*.xml)|*.xml|All files (*.*)|*.*';
  if OpenDialogXML.Execute then
    editSource.Text := OpenDialogXML.Filename;
end;

procedure TFormXmldecryptor.btnBrowseDestClick(Sender: TObject);
begin
  SaveDialogXML.FileName := editDest.Text;
  SaveDialogXML.Filter := 'XML files (*.xml)|*.xml|All files (*.*)|*.*';
  if SaveDialogXML.Execute then
    editDest.Text := SaveDialogXML.FileName;
end;

procedure TFormXmldecryptor.SetFileContents(const FileName : string; const Content : TBytes);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
  try
    if Length(Content) > 0 then
      F.Write(Content[0], Length(Content));
  finally
    FreeAndNil(F);
  end;
end;

procedure TFormXmldecryptor.HandleDecryptionInfoNeeded(Sender: TObject; var CancelDecryption: Boolean);
begin
  FormXmlencprops.SetXMLEncryptionProps(Sender as TsbxXMLDecryptor);

  CancelDecryption := (FormXmlencprops.ShowModal = mrCancel);

  FormXmlencprops.GetXMLDecryptionInfo(Sender as TsbxXMLDecryptor);
end;

procedure TFormXmldecryptor.HandleSaveExternalData(Sender: TObject; ExternalData: TBytes);
begin
  SaveDialogXML.Title := 'Save external data as';
  SaveDialogXML.FileName := '';
  SaveDialogXML.Filter := 'All files (*.*)|*.*';
  if SaveDialogXML.Execute then
  begin
    SetFileContents(SaveDialogXML.FileName, ExternalData);
  end;
end;

procedure TFormXmldecryptor.btnDecryptClick(Sender: TObject);
var
  XMLDecryptor : TsbxXMLDecryptor;
begin
  XMLDecryptor := TsbxXMLDecryptor.Create(nil);
  try
    try
      XMLDecryptor.OnDecryptionInfoNeeded := HandleDecryptionInfoNeeded;
      XMLDecryptor.OnSaveExternalData := HandleSaveExternalData;

      XMLDecryptor.InputFile := editSource.Text;
      XMLDecryptor.OutputFile := editDest.Text;

      XMLDecryptor.Decrypt();

      MessageDlg('XML file successfully decrypted', mtInformation, [mbOk], 0);
      Close();
    except
      on E: Exception do
        MessageDlg(e.Message, mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(XMLDecryptor);
  end;
end;

procedure TFormXmldecryptor.btnCancelClick(Sender: TObject);
begin
  Close();
end;

end.


