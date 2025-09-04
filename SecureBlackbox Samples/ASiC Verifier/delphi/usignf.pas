unit usignf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificatemanager, SBxASiCVerifier;

type
  TFormUsign = class(TForm)
    DlgOpen: TOpenDialog;
    btnOK: TButton;
    btnCancel: TButton;
    lIssuerRDN: TLabel;
    edIssuerRDN: TEdit;
    gbSigningCertificates: TGroupBox;
    lvSigningCertificates: TListView;
    btnRemove: TButton;
    btnAdd: TButton;
    lSerialNumber: TLabel;
    edSerialNumber: TEdit;
    lSubjectKeyID: TLabel;
    edSubjectKeyID: TEdit;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
  private
    { Private declarations }
    FVerifier: TsbxASiCVerifier;

    procedure UpdateCertificates;
  public
    { Public declarations }
    procedure Init(Verifier: TsbxASiCVerifier; const IssuerRDN: string;
      const SerialNumber: TBytes; const SubjectKeyID: TBytes);
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

procedure TFormUsign.Init(Verifier: TsbxASiCVerifier; const IssuerRDN: string;
  const SerialNumber: TBytes; const SubjectKeyID: TBytes);
begin
  FVerifier := Verifier;

  edIssuerRDN.Text := IssuerRDN;
  edSerialNumber.Text := BinaryToString(SerialNumber);
  edSubjectKeyID.Text := BinaryToString(SubjectKeyID);

  UpdateCertificates;
end;

procedure TFormUsign.UpdateCertificates;
var
  Item: TListItem;
  i: Integer;
  s: string;
  CertificateManager: TsbxCertificateManager;
begin
  lvSigningCertificates.Items.BeginUpdate;
  lvSigningCertificates.Items.Clear;

  CertificateManager := TsbxCertificateManager.Create(nil);
  try
    for i := 0 to FVerifier.KnownCertificates.Count - 1 do
    begin
      CertificateManager.Certificate := FVerifier.KnownCertificates.Item[i];

      s := CertificateManager.Certificate.Issuer;
      if s = '' then
        s := '<unknown>';

      Item := lvSigningCertificates.Items.Add;
      Item.Caption := BinaryToString(CertificateManager.Certificate.SerialNumber);
      Item.SubItems.Add(s);
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  lvSigningCertificates.Items.EndUpdate;
end;

procedure TFormUsign.btnAddClick(Sender: TObject);
var
  Certificatemanager: TsbxCertificatemanager;
begin
  DlgOpen.Title := 'Select certificate file';
  DlgOpen.Filter := 'Certificates (*.pem, *.cer, *.crt, *.der, *.pfx, *.p12, *.pkcs12)|*.pem;*.cer;*.crt;*.der;*.pfx;*.p12;*.pkcs12|PEM-encoded certificates (*.pem)|*.pem|' +
    'DER-encoded certificates (*.cer, *.crt, *.der)|*.cer;*.crt;*.der|PKCS#12 encoded certificates (*.pfx, *.p12, *.pkcs12)|*.pfx;*.p12;*.pkcs12|All files (*.*)|*.*';
  if DlgOpen.Execute then
  begin
    Certificatemanager := TsbxCertificatemanager.Create(nil);
    try
      try
        Certificatemanager.ImportFromFile(DlgOpen.Filename, InputBox('Please enter passphrase:', '',''));

        FVerifier.KnownCertificates.Add(Certificatemanager.Certificate);

        UpdateCertificates;
      except
        on E: Exception do
          MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
      end;
    finally
      FreeAndNil(Certificatemanager);
    end;
  end;
end;

procedure TFormUsign.btnRemoveClick(Sender: TObject);
begin
  if Assigned(lvSigningCertificates.Selected) then
  begin
    FVerifier.KnownCertificates.RemoveAt(lvSigningCertificates.Selected.Index);
    UpdateCertificates;
  end;
end;

end.
