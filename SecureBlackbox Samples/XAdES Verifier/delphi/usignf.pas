unit usignf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxXAdESVerifier;

type
  TFormUsign = class(TForm)
    gbGeneralEnc: TGroupBox;
    lbHashAlgorithm: TLabel;
    lbCanonMethod: TLabel;
    edCanonMethod: TEdit;
    edHashAlgorithm: TEdit;
    gbKeyInfo: TGroupBox;
    lbKeyName: TLabel;
    edKeyName: TEdit;
    gbSigningCertificates: TGroupBox;
    lvKnownCertificates: TListView;
    btnRemove: TButton;
    btnAdd: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    DlgOpen: TOpenDialog;
    Label10: TLabel;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
  private
    { Private declarations }
    FVerifier: TsbxXAdESVerifier;

    procedure UpdateCertificates;
  public
    { Public declarations }
    procedure SetSignature(Value: TsbxXAdESVerifier; SigIndex : Integer);
  end;

var
  FormUsign: TFormUsign;

implementation

{$R *.dfm}

function BinaryToString(const Buffer : TBytes): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to Length(Buffer) - 1 do
    Result := Result + IntToHex(Buffer[i], 1);
end;

procedure TFormUsign.SetSignature(Value: TsbxXAdESVerifier; SigIndex : Integer);
begin
  FVerifier := Value;

  case FVerifier.Signatures[SigIndex].CanonicalizationMethod of
    cxcmCanon: edCanonMethod.Text := 'Canonical';
    cxcmCanonComment: edCanonMethod.Text := 'Canonical with comments';
    cxcmCanon_v1_1: edCanonMethod.Text := 'Canonical v1.1';
    cxcmCanonComment_v1_1: edCanonMethod.Text := 'Canonical with comments v1.1';
    cxcmExclCanon: edCanonMethod.Text := 'Exclusive canonical';
    cxcmExclCanonComment: edCanonMethod.Text := 'Exclusive canonical with comments';
    cxcmMinCanon: edCanonMethod.Text := 'Minimal canonical';
  else
    edCanonMethod.Text := 'Canonical';
  end;

  edHashAlgorithm.Text := FVerifier.Signatures[SigIndex].HashAlgorithm;

  edKeyName.Text := FVerifier.Config('KeyName');

  UpdateCertificates;
end;

procedure TFormUsign.UpdateCertificates;
var
  Item: TListItem;
  i: Integer;
  s: string;
  CertificateManager: TsbxCertificateManager;
begin
  lvKnownCertificates.Items.BeginUpdate;
  lvKnownCertificates.Items.Clear;

  CertificateManager := TsbxCertificateManager.Create(nil);
  try
    for i := 0 to FVerifier.KnownCertificates.Count - 1 do
    begin
      CertificateManager.Certificate := FVerifier.KnownCertificates.Item[i];

      s := CertificateManager.Certificate.Issuer;
      if s = '' then
        s := '<unknown>';

      Item := lvKnownCertificates.Items.Add;
      Item.Caption := BinaryToString(CertificateManager.Certificate.SerialNumber);
      Item.SubItems.Add(s);
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  lvKnownCertificates.Items.EndUpdate;
end;

procedure TFormUsign.btnAddClick(Sender: TObject);
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

        FVerifier.KnownCertificates.Add(CertificateManager.Certificate);

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

procedure TFormUsign.btnRemoveClick(Sender: TObject);
begin
  if Assigned(lvKnownCertificates.Selected) then
  begin
    FVerifier.KnownCertificates.RemoveAt(lvKnownCertificates.Selected.Index);
    UpdateCertificates;
  end;
end;

end.