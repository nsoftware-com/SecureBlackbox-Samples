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
unit mailreaderf;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, XPMan, StdCtrls, ComCtrls,
  sbxCore, sbxTypes, sbxMailReader, sbxCertificateManager, certloadf;

type
  TFormMailReader = class(TForm)
    grpLoad: TGroupBox;
    lblFilename: TLabel;
    edtFilename: TEdit;
    btnFilename: TButton;
    btnLoad: TButton;
    lblInfo: TLabel;
    dlgLoadMessage: TOpenDialog;
    grpText: TGroupBox;
    lblPlainText: TLabel;
    lblHtmlText: TLabel;
    memPlainText: TMemo;
    memHtmlText: TMemo;
    grpAttachments: TGroupBox;
    lvwAttachments: TListView;
    btnAttach: TButton;
    grpOriginators: TGroupBox;
    lblFrom: TLabel;
    lblSender: TLabel;
    edtSender: TEdit;
    grpRecipients: TGroupBox;
    lblTo: TLabel;
    lblCc: TLabel;
    edtFrom: TEdit;
    lvwTo: TListView;
    lvwCc: TListView;
    grpInfo: TGroupBox;
    lblSubject: TLabel;
    lblPriority: TLabel;
    edtSubject: TEdit;
    cbxDeliveryReceipt: TCheckBox;
    cbxReadReceipt: TCheckBox;
    txtPriority: TLabel;
    grpSecurity: TGroupBox;
    dlgSaveAttachment: TSaveDialog;
    lblDecryptionCertificate: TLabel;
    edtDecryptionCertificate: TEdit;
    lblEncryptionAlgorithm: TLabel;
    txtEncryptionAlgorithm: TStaticText;
    lblSigningCertificate: TLabel;
    edtSigningCertificate: TEdit;
    lblSignatureValidation: TLabel;
    txtSignatureValidation: TStaticText;
    lblHashAlgorithm: TLabel;
    txtHashAlgorithm: TStaticText;
    procedure btnFilenameClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAttachClick(Sender: TObject);
  private
    FCertManager: TsbxCertificateManager;
    FReader: TsbxMailReader;
    procedure Clear();
    procedure HandleDecryptionInfoNeeded(Sender: TObject; const IssuerRDN: string; SerialNumber, SubjectKeyID: TBytes);
    procedure PublishAddresses(Addresses: TsbxMailAddressList; View: TListView);
    procedure PublishAttachments(Attachments: TsbxMailAttachmentList);
    procedure PublishPriority(Priority: TsbxMailPriorities);
    procedure PublishSecurityInfo(Info: TsbxMailSecurityInfo;
      DecryptionCertificate, SigningCertificate: TsbxCertificate);
  public
    { Public declarations }
  end;

var
  FormMailReader: TFormMailReader;

implementation

{$R *.dfm}

procedure TFormMailReader.btnAttachClick(Sender: TObject);
var
  Stream: TFileStream;
  Data: TBytes;
begin
  if lvwAttachments.ItemIndex < 0 then
  begin
    MessageDlg('Please select an attachment to save to a file', mtWarning, [mbOk], 0);
    Exit;
  end;

  if not dlgSaveAttachment.Execute(Self.Handle) then
    Exit;

  Stream := TFileStream.Create(dlgSaveAttachment.FileName, fmCreate or fmShareDenyWrite);
  try
    Data := FReader.Attachments[lvwAttachments.ItemIndex].Data;
    if Length(Data) <> 0 then
      Stream.WriteBuffer(Data[0], Length(Data));
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TFormMailReader.btnFilenameClick(Sender: TObject);
begin
  if dlgLoadMessage.Execute(Self.Handle) then
    edtFilename.Text := dlgLoadMessage.FileName;
end;

procedure TFormMailReader.btnLoadClick(Sender: TObject);
begin
  if edtFilename.Text = '' then
  begin
    MessageDlg('Please specify a name for the source file.', mtWarning, [mbOk], 0);
    Exit;
  end;

  try
    FReader.LoadFromFile(edtFilename.Text);
  except
    on E: Exception do
    begin
      MessageDlg('Failed to load and/or parse a message.'#13#10 + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  Clear();

  edtSender.Text := FReader.Message.Sender;
  edtFrom.Text := FReader.Message.From;

  PublishAddresses(FReader.SendTo, lvwTo);
  PublishAddresses(FReader.Cc, lvwCc);

  PublishSecurityInfo(FReader.SecurityInfo, FReader.DecryptionCertificate, FReader.SigningCertificate);

  edtSubject.Text := FReader.Message.Subject;
  PublishPriority(FReader.Message.Priority);
  cbxDeliveryReceipt.Checked := FReader.Message.DeliveryReceipt;
  cbxReadReceipt.Checked := FReader.Message.ReadReceipt;

  memPlainText.Text := FReader.Message.PlainText;
  memHtmlText.Text := FReader.Message.HtmlText;

  PublishAttachments(FReader.Attachments);
end;

procedure TFormMailReader.Clear();
begin
  edtSender.Clear();
  edtFrom.Clear();

  lvwTo.Items.Clear();
  lvwCc.Items.Clear();

  edtDecryptionCertificate.Text := '[message not encrypted]';
  txtEncryptionAlgorithm.Caption := '';
  edtSigningCertificate.Text := '[message not signed]';
  txtSignatureValidation.Caption := '';
  txtHashAlgorithm.Caption := '';

  edtSubject.Clear();
  txtPriority.Caption := 'Normal';
  cbxDeliveryReceipt.Checked := False;
  cbxReadReceipt.Checked := False;

  memPlainText.Clear();
  memHtmlText.Clear();

  lvwAttachments.Items.Clear();
end;

procedure TFormMailReader.FormCreate(Sender: TObject);
begin
  FCertManager := TsbxCertificateManager.Create(nil);
  FReader := TsbxMailReader.Create(nil);
  FReader.OnDecryptionInfoNeeded := HandleDecryptionInfoNeeded;
  Clear();
end;

procedure TFormMailReader.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FReader);
  FreeAndNil(FCertManager);
end;

procedure TFormMailReader.HandleDecryptionInfoNeeded(Sender: TObject; const IssuerRDN: string;
  SerialNumber, SubjectKeyID: TBytes);
begin
  if TFormCertLoad.Execute(FCertManager, IssuerRDN, SerialNumber, SubjectKeyID) then
    FReader.DecryptionCertificate := FCertManager.Certificate
  else
    FReader.DecryptionCertificate := nil;
end;

procedure TFormMailReader.PublishAddresses(Addresses: TsbxMailAddressList; View: TListView);
var
  I: Integer;
  Addr: TsbxMailAddress;
  Item: TListItem;
begin
  View.Items.BeginUpdate();
  try
    for I := 0 to Addresses.Count - 1 do
    begin
      Addr := Addresses[I];
      Item := View.Items.Add();
      Item.Caption := Addr.DisplayName;
      Item.SubItems.Add(Addr.Address);
    end;
  finally
    View.Items.EndUpdate();
  end;
end;

procedure TFormMailReader.PublishAttachments(Attachments: TsbxMailAttachmentList);
var
  I: Integer;
  Attachment: TsbxMailAttachment;
  Item: TListItem;
begin
  lvwAttachments.Items.BeginUpdate();
  try
    for I := 0 to Attachments.Count - 1 do
    begin
      Attachment := Attachments[I];
      Item := lvwAttachments.Items.Add();
      if Attachment.ID = '' then
        Item.Caption := Attachment.Filename
      else
        Item.Caption := Attachment.ID;
      if Attachment.ContentType = '' then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(Attachment.ContentType + '/' + Attachment.ContentSubtype);
      Item.SubItems.Add(FormatFloat(',0', Attachment.Size));
    end;
  finally
    lvwAttachments.Items.EndUpdate();
  end;
end;

procedure TFormMailReader.PublishPriority(Priority: TsbxMailPriorities);
begin
  case Priority of
    mpLowest:
      txtPriority.Caption := 'Lowest';
    mpLow:
      txtPriority.Caption := 'Low';
    mpNormal:
      txtPriority.Caption := 'Normal';
    mpHigh:
      txtPriority.Caption := 'High';
    mpHighest:
      txtPriority.Caption := 'Highest';
  else
    txtPriority.Caption := 'Unknown';
  end;
end;

procedure TFormMailReader.PublishSecurityInfo(Info: TsbxMailSecurityInfo; DecryptionCertificate,
  SigningCertificate: TsbxCertificate);
begin
  if Info.Encrypted then
  begin
    if DecryptionCertificate = nil then
      edtDecryptionCertificate.Text := '[certificate not provided]'
    else
      edtDecryptionCertificate.Text := DecryptionCertificate.SubjectRDN;

    txtEncryptionAlgorithm.Caption := Info.EncryptionAlgorithm;
  end;

  if Info.Signed then
  begin
    if SigningCertificate = nil then
      edtSigningCertificate.Text := '[certificate not found]'
    else
      edtSigningCertificate.Text := SigningCertificate.SubjectRDN;

    case Info.SignatureValidationResult of
      svtValid:
        txtSignatureValidation.Caption := 'VALID';
      svtUnknown:
        txtSignatureValidation.Caption := 'NOT TRUSTED';
      svtCorrupted:
        txtSignatureValidation.Caption := 'CORRUPTED';
      svtSignerNotFound:
        txtSignatureValidation.Caption := 'SIGNER NOT FOUND';
      svtFailure:
        txtSignatureValidation.Caption := 'FAILED';
    end;

    txtHashAlgorithm.Caption := Info.HashAlgorithm;
  end;
end;

end.

