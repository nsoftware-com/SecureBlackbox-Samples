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
unit mailwriterf;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtDlgs, XPMan,
  sbxtypes, sbxconstants, SBxMailWriter, SBxCertificateManager,
  addressesf, inputf;

type
  TFormMailWriter = class(TForm)
    lblInfo: TLabel;
    grpOriginators: TGroupBox;
    lblFrom: TLabel;
    edtFrom: TEdit;
    lblSender: TLabel;
    edtSender: TEdit;
    lblSenderNotice: TLabel;
    btnFrom: TButton;
    grpAddressees: TGroupBox;
    lblTo: TLabel;
    edtTo: TEdit;
    lblCc: TLabel;
    lblBcc: TLabel;
    btnTo: TButton;
    edtCc: TEdit;
    btnCc: TButton;
    edtBcc: TEdit;
    btnBcc: TButton;
    grpOptions: TGroupBox;
    lblSubject: TLabel;
    edtSubject: TEdit;
    lblPriority: TLabel;
    cmbPriority: TComboBox;
    cbxDeliveryReceipt: TCheckBox;
    cbxReadReceipt: TCheckBox;
    grpSecurity: TGroupBox;
    lblSigningCertificate: TLabel;
    lvwEncryptionCertificates: TListView;
    lblHashAlgorithm: TLabel;
    cmbHashAlgorithm: TComboBox;
    lblSignatureFormat: TLabel;
    cmbSignatureFormat: TComboBox;
    btnLoadSigningCertificate: TButton;
    lblEncryptionCertificates: TLabel;
    btnLoadEncryptionCertificate: TButton;
    btnDeleteEncryptionCertificate: TButton;
    lblEncryptionAlgorithm: TLabel;
    cmbEncryptionAlgorithm: TComboBox;
    grpText: TGroupBox;
    grpAttachments: TGroupBox;
    lvwAttachments: TListView;
    btnAttach: TButton;
    btnDeleteAttachment: TButton;
    lblPlainText: TLabel;
    lblHtmlText: TLabel;
    memPlainText: TMemo;
    memHtmlText: TMemo;
    lblEmbeddedImagesNotice: TLabel;
    btnEmbed: TButton;
    lblHashAlgorithmNotice: TLabel;
    lblEncryptionCertificateNotice: TLabel;
    grpSave: TGroupBox;
    lblFilename: TLabel;
    edtFilename: TEdit;
    btnFilename: TButton;
    btnSave: TButton;
    dlgAttachFile: TOpenDialog;
    dlgSaveMessage: TSaveDialog;
    btnClearSigningCertificate: TButton;
    edtSigningCertificate: TEdit;
    dlgEmbedImage: TOpenDialog;
    dlgSigningCertificate: TOpenDialog;
    dlgEncryptionCertificate: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnFromClick(Sender: TObject);
    procedure btnToClick(Sender: TObject);
    procedure btnCcClick(Sender: TObject);
    procedure btnBccClick(Sender: TObject);
    procedure btnAttachClick(Sender: TObject);
    procedure btnDeleteAttachmentClick(Sender: TObject);
    procedure btnFilenameClick(Sender: TObject);
    procedure btnClearSigningCertificateClick(Sender: TObject);
    procedure btnEmbedClick(Sender: TObject);
    procedure btnLoadSigningCertificateClick(Sender: TObject);
    procedure btnLoadEncryptionCertificateClick(Sender: TObject);
    procedure btnDeleteEncryptionCertificateClick(Sender: TObject);
  private
    FWriter: TsbxMailWriter;
    procedure AskForPassword(Sender: TObject; var Password: string; var Cancel: Boolean);
  public
    procedure Clear();
  end;

var
  FormMailWriter: TFormMailWriter;

implementation

{$R *.dfm}

uses
  Registry;

const
  HashAlgorithms: array [0..9] of string = ( SB_HASH_ALGORITHM_MD5, SB_HASH_ALGORITHM_SHA1, SB_HASH_ALGORITHM_SHA224,
    SB_HASH_ALGORITHM_SHA256, SB_HASH_ALGORITHM_SHA384, SB_HASH_ALGORITHM_SHA512, SB_HASH_ALGORITHM_SHA3_224,
    SB_HASH_ALGORITHM_SHA3_256, SB_HASH_ALGORITHM_SHA3_384, SB_HASH_ALGORITHM_SHA3_512 );

  EncryptionAlgorithms: array [0..8] of string = ( SB_SYMMETRIC_ALGORITHM_DES, SB_SYMMETRIC_ALGORITHM_3DES,
    SB_SYMMETRIC_ALGORITHM_AES128, SB_SYMMETRIC_ALGORITHM_AES192, SB_SYMMETRIC_ALGORITHM_AES256,
    SB_SYMMETRIC_ALGORITHM_BLOWFISH, SB_SYMMETRIC_ALGORITHM_TWOFISH, SB_SYMMETRIC_ALGORITHM_CAMELLIA,
    SB_SYMMETRIC_ALGORITHM_SERPENT );

function LoadFile(const Filename: string): TBytes;
var
  F: TFileStream;
  Size: Integer;
begin
  F := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    Size := F.Size;
    if Size <> 0 then
    begin
      SetLength(Result, Size);
      F.ReadBuffer(Result[0], Size);
    end;
  finally
    FreeAndNil(F);
  end;
end;

procedure ApplyContentType(Attachment: TsbxMailAttachment);
var
  Ext, Value: string;
  Reg: TRegIniFile;
begin
  Ext := ExtractFileExt(Attachment.Filename);
  if Ext = '' then
    Exit;

  Reg := TRegIniFile.Create();
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    if not Reg.KeyExists(Ext) then
      Exit;

    Value := Reg.ReadString(Ext, 'Content Type', '');
    if Value = '' then
      Exit;

    Attachment.ContentType := Value;
  finally
    FreeAndNil(Reg);
  end;
end;

procedure TFormMailWriter.AskForPassword(Sender: TObject; var Password: string; var Cancel: Boolean);
begin
  Cancel := not TFormInput.Execute('Enter a password:', True, Password);
end;

procedure TFormMailWriter.btnAttachClick(Sender: TObject);
var
  Index: Integer;
  Item: TListItem;
  Attachment: TsbxMailAttachment;
begin
  if not dlgAttachFile.Execute(Self.Handle) then
    Exit;

  Index := FWriter.AttachFile(dlgAttachFile.FileName);
  Attachment := FWriter.Attachments[Index];
  ApplyContentType(Attachment);

  Item := lvwAttachments.Items.Add();
  Item.Caption := Attachment.Filename;
  if Attachment.ContentType = '' then
    Item.SubItems.Add('')
  else
    Item.SubItems.Add(Attachment.ContentType + '/' + Attachment.ContentSubtype);
  Item.SubItems.Add(FormatFloat(',0', Attachment.Size));
  Item.MakeVisible(False);
  Item.Selected := True;
end;

procedure TFormMailWriter.btnBccClick(Sender: TObject);
var
  Addresses: string;
begin
  Addresses := edtBcc.Text;
  if TFormAddresses.Execute('BCC', Addresses) then
    edtBcc.Text := Addresses;
end;

procedure TFormMailWriter.btnCcClick(Sender: TObject);
var
  Addresses: string;
begin
  Addresses := edtCc.Text;
  if TFormAddresses.Execute('CC', Addresses) then
    edtCc.Text := Addresses;
end;

procedure TFormMailWriter.btnClearSigningCertificateClick(Sender: TObject);
begin
  FWriter.SigningCertificate := nil;
  edtSigningCertificate.Text := '';
end;

procedure TFormMailWriter.btnDeleteAttachmentClick(Sender: TObject);
var
  Index: Integer;
begin
  if lvwAttachments.Selected = nil then
    Exit;

  Index := lvwAttachments.Selected.Index;

  FWriter.Attachments.RemoveAt(Index);
  lvwAttachments.Selected.Delete();

  if lvwAttachments.Items.Count = 0 then
    Exit;
  if Index < lvwAttachments.Items.Count then
    lvwAttachments.Items[Index].Selected := True
  else
    lvwAttachments.Items[Index - 1].Selected := True;
  lvwAttachments.Selected.MakeVisible(False);
end;

procedure TFormMailWriter.btnDeleteEncryptionCertificateClick(Sender: TObject);
var
  Index: Integer;
begin
  if lvwEncryptionCertificates.Selected = nil then
    Exit;

  Index := lvwEncryptionCertificates.Selected.Index;

  FWriter.EncryptionCertificates.RemoveAt(Index);
  lvwEncryptionCertificates.Selected.Delete();

  if lvwEncryptionCertificates.Items.Count = 0 then
    Exit;
  if Index < lvwEncryptionCertificates.Items.Count then
    lvwEncryptionCertificates.Items[Index].Selected := True
  else
    lvwEncryptionCertificates.Items[Index - 1].Selected := True;
  lvwEncryptionCertificates.Selected.MakeVisible(False);
end;

procedure TFormMailWriter.btnEmbedClick(Sender: TObject);
var
  Buffer: TBytes;
  ID, Ext: string;
  Index: Integer;
  Item: TListItem;
  Attachment: TsbxMailAttachment;
begin
  if not dlgEmbedImage.Execute(Self.Handle) then
    Exit;

  try
    Buffer := LoadFile(dlgEmbedImage.Filename);
  except
    on E: Exception do
    begin
      MessageDlg('Failed to load the file.'#13#10 + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  ID := '';
  if not TFormInput.Execute('Enter an ID for the image:', False, ID) then
    Exit;

  Index := FWriter.AttachImage(ID, Buffer);
  Attachment := FWriter.Attachments[Index];
  Ext := ExtractFileExt(dlgEmbedImage.FileName);
  if SameText(Ext, '.jpg') then
    Attachment.ContentSubtype := 'jpeg'
  else
    Attachment.ContentSubtype := LowerCase(Copy(Ext, 2, MaxInt));

  Item := lvwAttachments.Items.Add();
  Item.Caption := Attachment.ID;
  if Attachment.ContentType = '' then
    Item.SubItems.Add('')
  else
    Item.SubItems.Add(Attachment.ContentType + '/' + Attachment.ContentSubtype);
  Item.SubItems.Add(FormatFloat(',0', Attachment.Size));
  Item.MakeVisible(False);
  Item.Selected := True;
end;

procedure TFormMailWriter.btnFilenameClick(Sender: TObject);
begin
  if edtFilename.Text <> '' then
    dlgSaveMessage.InitialDir := ExtractFileDir(edtFilename.Text);

  if dlgSaveMessage.Execute(Self.Handle) then
    edtFilename.Text := dlgSaveMessage.FileName;
end;

procedure TFormMailWriter.btnFromClick(Sender: TObject);
var
  Addresses: string;
begin
  Addresses := edtFrom.Text;
  if TFormAddresses.Execute('FROM', Addresses) then
    edtFrom.Text := Addresses;
end;

procedure TFormMailWriter.btnLoadEncryptionCertificateClick(Sender: TObject);
var
  Manager: TsbxCertificateManager;
  Item: TListItem;
begin
  if not dlgEncryptionCertificate.Execute(Self.Handle) then
    Exit;

  Manager := TsbxCertificateManager.Create(nil);
  try
    Manager.OnPasswordNeeded := AskForPassword;

    try
      Manager.ImportFromFile(dlgEncryptionCertificate.FileName, '');
    except
      on E: Exception do
      begin
        MessageDlg(E.Message, mtError, [mbOk], 0);
        Exit;
      end;
    end;

    if Manager.Certificate = nil then // the user cancelled certificate loading
      Exit;

    FWriter.EncryptionCertificates.Add(Manager.Certificate);
    Item := lvwEncryptionCertificates.Items.Add();
    Item.Caption := Manager.Certificate.Subject;
    Item.SubItems.Add(Manager.Certificate.Issuer);
  finally
    FreeAndNil(Manager);
  end;
end;

procedure TFormMailWriter.btnLoadSigningCertificateClick(Sender: TObject);
var
  Manager: TsbxCertificateManager;
begin
  if not dlgSigningCertificate.Execute(Self.Handle) then
    Exit;

  Manager := TsbxCertificateManager.Create(nil);
  try
    Manager.OnPasswordNeeded := AskForPassword;

    try
      Manager.ImportFromFile(dlgSigningCertificate.FileName, '');
    except
      on E: Exception do
      begin
        MessageDlg(E.Message, mtError, [mbOk], 0);
        Exit;
      end;
    end;

    if Manager.Certificate = nil then // the user cancelled certificate loading
      Exit;

    if not Manager.Certificate.PrivateKeyExists then
    begin
      MessageDlg('The certificate does not contain a private key, and cannot be used for signing.', mtError, [mbOk], 0);
      Exit;
    end;

    FWriter.SigningCertificate := Manager.Certificate;
    edtSigningCertificate.Text := Format('%s (%s %d bits)',
      [FWriter.SigningCertificate.Subject, FWriter.SigningCertificate.KeyAlgorithm, FWriter.SigningCertificate.KeyBits]);
  finally
    FreeAndNil(Manager);
  end;
end;

procedure TFormMailWriter.btnSaveClick(Sender: TObject);
var
  Dir: string;
begin
  if edtFilename.Text = '' then
  begin
    MessageDlg('Please specify a name for the destination file.', mtWarning, [mbOk], 0);
    Exit;
  end;

  Dir := ExtractFileDir(edtFilename.Text);
  if Dir <> '' then
  begin
    if not DirectoryExists(Dir) then
    begin
      if MessageDlg('The destination directory does not exist. Would you like to create it?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        Exit;

      if not ForceDirectories(Dir) then
      begin
        MessageDlg('Failed to create the destination directory.', mtError, [mbOk], 0);
        Exit;
      end;
    end;
  end;

  // originator(s)
  FWriter.Message.From := edtFrom.Text;
  FWriter.Message.Sender := edtSender.Text;
  // addressee(s)
  FWriter.Message.SendTo := edtTo.Text;
  FWriter.Message.Cc := edtCc.Text;
  FWriter.Message.Bcc := edtBcc.Text;
  // options
  FWriter.Message.Subject := edtSubject.Text;
  case cmbPriority.ItemIndex of
    0: FWriter.Message.Priority := mpLowest;
    1: FWriter.Message.Priority := mpLow;
    3: FWriter.Message.Priority := mpHigh;
    4: FWriter.Message.Priority := mpHighest;
  else
    FWriter.Message.Priority := mpNormal;
  end;
  FWriter.Message.DeliveryReceipt := cbxDeliveryReceipt.Checked;
  FWriter.Message.ReadReceipt := cbxReadReceipt.Checked;
  // text
  FWriter.Message.PlainText := memPlainText.Lines.Text;
  FWriter.Message.HtmlText := memHtmlText.Lines.Text;
  // security
  FWriter.SecuritySettings.SignBeforeEncrypt := False;
  FWriter.SecuritySettings.SignMessageHeader := False;
  FWriter.SecuritySettings.Sign := (FWriter.SigningCertificate <> nil);
  if FWriter.SecuritySettings.Sign then
  begin
    if cmbHashAlgorithm.ItemIndex = -1 then
    begin
      MessageDlg('Please select a hash algorithm to sign the message.', mtWarning, [mbOk], 0);
      Exit;
    end;
    FWriter.SecuritySettings.HashAlgorithm := HashAlgorithms[cmbHashAlgorithm.ItemIndex];

    case cmbSignatureFormat.ItemIndex of
      0: FWriter.SecuritySettings.SignatureFormat := msMultipartSigned;
      1: FWriter.SecuritySettings.SignatureFormat := msSignedData;
    else
      MessageDlg('Please select a signature format to be used.', mtWarning, [mbOk], 0);
      Exit;
    end;
  end;
  FWriter.SecuritySettings.Encrypt := (FWriter.EncryptionCertificates.Count <> 0);
  if FWriter.SecuritySettings.Encrypt then
  begin
    if cmbEncryptionAlgorithm.ItemIndex = -1 then
    begin
      MessageDlg('Please select an encryption algorithm to encrypt the message.', mtWarning, [mbOk], 0);
      Exit;
    end;
    FWriter.SecuritySettings.EncryptionAlgorithm := EncryptionAlgorithms[cmbEncryptionAlgorithm.ItemIndex];
  end;

  try
    FWriter.SaveToFile(edtFilename.Text);
  except
    on E: Exception do
    begin
      MessageDlg('Failed to assemble and/or save a message.'#13#10 + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  MessageDlg('A message has been assembled and saved successfully.', mtInformation, [mbOk], 0);
  Clear();
end;

procedure TFormMailWriter.btnToClick(Sender: TObject);
var
  Addresses: string;
begin
  Addresses := edtTo.Text;
  if TFormAddresses.Execute('TO', Addresses) then
    edtTo.Text := Addresses;
end;

procedure TFormMailWriter.Clear();
var
  NewMessage: TsbxMailMessage;
begin
  NewMessage := TsbxMailMessage.Create();
  FWriter.Message := NewMessage;
  FreeAndNil(NewMessage);
end;

procedure TFormMailWriter.FormCreate(Sender: TObject);
begin
  Assert(Length(HashAlgorithms) = cmbHashAlgorithm.Items.Count,
    'Number of hash algorithms in the combobox is not equal to the length of the HashAlgorithms array');
  Assert(Length(EncryptionAlgorithms) = cmbEncryptionAlgorithm.Items.Count,
    'Number of encryption algorithms in the combobox is not equal to the length of the EncryptionAlgorithms array');

  FWriter := TsbxMailWriter.Create(nil);
end;

procedure TFormMailWriter.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWriter);
end;

end.


