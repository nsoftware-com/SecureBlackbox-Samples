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
unit messagedecryptorf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxMessageDecryptor;

type
  TFormMessagedecryptor = class(TForm)
    lbInputFile: TLabel;
    edInputFile: TEdit;
    sbBrowseInputFile: TSpeedButton;
    dlgOpenFile: TOpenDialog;
    lOutputFile: TLabel;
    edOutputFile: TEdit;
    sbOutputFile: TSpeedButton;
    btnDecrypt: TButton;
    gbSigningCertificates: TGroupBox;
    lvCertificates: TListView;
    btnRemove: TButton;
    btnAdd: TButton;
    dlgSaveFile: TSaveDialog;
    DlgOpen: TOpenDialog;
    Label10: TLabel;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
  private
    { Private declarations }
    FDecryptor: TsbxMessageDecryptor;

    procedure UpdateCertificates;
    function WriteCertificateInfo(Cert: TsbxCertificate): string;
  public
    { Public declarations }
  end;

var
  FormMessagedecryptor: TFormMessagedecryptor;

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

procedure TFormMessagedecryptor.UpdateCertificates;
var
  Item: TListItem;
  i: Integer;
  s: string;
  CertificateManager: TsbxCertificateManager;
begin
  lvCertificates.Items.BeginUpdate;
  lvCertificates.Items.Clear;

  CertificateManager := TsbxCertificateManager.Create(nil);
  try
    for i := 0 to FDecryptor.Certificates.Count - 1 do
    begin
      CertificateManager.Certificate := FDecryptor.Certificates.Item[i];

      s := CertificateManager.Certificate.Issuer;
      if s = '' then
        s := '<unknown>';

      Item := lvCertificates.Items.Add;
      Item.Caption := BinaryToString(CertificateManager.Certificate.SerialNumber);
      Item.SubItems.Add(s);
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  lvCertificates.Items.EndUpdate;
end;

function TFormMessagedecryptor.WriteCertificateInfo(Cert: TsbxCertificate) : string;
begin
  Result := Result + 'Certificate:'#13#10;
  Result := Result + 'Issuer: ' + Cert.Issuer + #13#10;
  Result := Result + 'Subject: ' + Cert.Subject + #13#10;
  if Cert.PrivateKeyExists then
    Result := Result + 'Private key available'#13#10#13#10
  else
    Result := Result + 'Private key is not available'#13#10#13#10;
end;

procedure TFormMessagedecryptor.btnAddClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  DlgOpen.Title := 'Select certificate file';
  DlgOpen.Filter := 'Certificates (*.pem, *.cer, *.crt, *.der, *.pfx, *.p12, *.pkcs12)|*.pem;*.cer;*.crt;*.der;*.pfx;*.p12;*.pkcs12|PEM-encoded certificates (*.pem)|*.pem|' +
    'DER-encoded certificates (*.cer, *.crt, *.der)|*.cer;*.crt;*.der|PKCS#12 encoded certificates (*.pfx, *.p12, *.pkcs12)|*.pfx;*.p12;*.pkcs12|All files (*.*)|*.*';
  if DlgOpen.Execute then
  begin
    CertificateManager := TsbxCertificateManager.Create(nil);
    try
      try
        CertificateManager.ImportFromFile(DlgOpen.Filename, InputBox('Please enter passphrase:', '',''));

        FDecryptor.Certificates.Add(CertificateManager.Certificate);

        UpdateCertificates;
      except
        on E: Exception do
          MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
      end;
    finally
      FreeAndNil(CertificateManager);
    end;
  end;
end;

procedure TFormMessagedecryptor.btnRemoveClick(Sender: TObject);
begin
  if Assigned(lvCertificates.Selected) then
  begin
    FDecryptor.Certificates.RemoveAt(lvCertificates.Selected.Index);
    UpdateCertificates;
  end;
end;

procedure TFormMessagedecryptor.btnDecryptClick(Sender: TObject);
var
  SigOK: boolean;
begin
  try
    FDecryptor.InputFile := edInputFile.Text;
    FDecryptor.OutputFile := edOutputFile.Text;

    FDecryptor.Decrypt;

    FormResults.mResults.Clear;
    FormResults.mResults.Lines.Add('Successfully decrypted!');
    FormResults.mResults.Lines.Add('');
    FormResults.mResults.Lines.Add('Symmetric Algorithm: ' + FDecryptor.EncryptionAlgorithm);
    FormResults.mResults.Lines.Add('');
    FormResults.mResults.Lines.Add('Certificates contained in message:');
    FormResults.mResults.Lines.Add(WriteCertificateInfo(FDecryptor.Certificates[FDecryptor.CertificateIndex]));

    FormResults.ShowModal;
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormMessagedecryptor.FormCreate(Sender: TObject);
begin
  FDecryptor := TsbxMessageDecryptor.Create(nil);
end;

procedure TFormMessagedecryptor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDecryptor);
end;

procedure TFormMessagedecryptor.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpenFile.FileName := edInputFile.Text;
  if dlgOpenFile.Execute then
    edInputFile.Text := dlgOpenFile.FileName;
end;

procedure TFormMessagedecryptor.sbOutputFileClick(Sender: TObject);
begin
  dlgSaveFile.FileName := edOutputFile.Text;
  if dlgSaveFile.Execute then
    edOutputFile.Text := dlgSaveFile.FileName;
end;

end.
