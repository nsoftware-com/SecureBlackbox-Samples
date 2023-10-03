unit usignf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificatemanager, SBxArchiveReader;

type
  TFormUsign = class(TForm)
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
    DlgOpen: TOpenDialog;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
  private
    { Private declarations }
    FArchive: TSBxArchiveReader;

    procedure UpdateCertificates;
  public
    { Public declarations }
    procedure Init(Archive: TSBxArchiveReader; const IssuerRDN: string;
      const SerialNumber: TBytes);
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

procedure TFormUsign.Init(Archive: TSBxArchiveReader; const IssuerRDN: string; const SerialNumber: TBytes);
begin
  FArchive := Archive;

  edIssuerRDN.Text := IssuerRDN;
  edSerialNumber.Text := BinaryToString(SerialNumber);

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
    for i := 0 to FArchive.KnownCertificates.Count - 1 do
    begin
      CertificateManager.Certificate := FArchive.KnownCertificates.Item[i];

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
  DlgOpen.Filter := 'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX';
  if DlgOpen.Execute then
  begin
    Certificatemanager := TsbxCertificatemanager.Create(nil);
    try
      try
        Certificatemanager.ImportFromFile(DlgOpen.Filename, InputBox('Please enter passphrase:', '',''));

        FArchive.KnownCertificates.Add(Certificatemanager.Certificate);

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
    FArchive.KnownCertificates.RemoveAt(lvSigningCertificates.Selected.Index);
    UpdateCertificates;
  end;
end;

end.
