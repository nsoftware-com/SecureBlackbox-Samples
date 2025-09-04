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
unit pdfsignerpkiproxyf;

interface

uses
  SysUtils, Variants, Classes, Graphics, StrUtils,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificateStorage, SBxCertificateManager, SBxPDFSigner;

type
  TFormPdfsignerpkiproxy = class(TForm)
    Label10: TLabel;
    lbInputFile: TLabel;
    sbBrowseInputFile: TSpeedButton;
    Label1: TLabel;
    sbOutputFile: TSpeedButton;
    edInputFile: TEdit;
    edOutputFile: TEdit;
    GroupBox6: TGroupBox;
    Label3: TLabel;
    btnLoadCerts: TSpeedButton;
    Label4: TLabel;
    edStoragePIN: TEdit;
    cbLevel: TComboBox;
    cbVisible: TCheckBox;
    btnSign: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    lvCerts: TListView;
    dlgOpenDLL: TOpenDialog;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure btnLoadCertsClick(Sender: TObject);
  private
    { Private declarations }
    FSigner: TsbxPDFSigner;
    FCertStorage: TsbxCertificateStorage;

    procedure CloseStorage;
    procedure LoadCertificates(const StoreFilename: string);
  public
    { Public declarations }
  end;

var
  FormPdfsignerpkiproxy: TFormPdfsignerpkiproxy;

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

procedure TFormPdfsignerpkiproxy.btnSignClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  if lvCerts.Selected = nil then
  begin
    MessageDlg('Select certificate for signing', mtWarning, [mbOk], 0);
    exit;
  end;

  FSigner.InputFile := edInputFile.Text;
  FSigner.OutputFile := edOutputFile.Text;

  FSigner.SigningCertificate := FCertStorage.Certificates[lvCerts.Selected.Index];

  if ContainsText(FSigner.SigningCertificate.KeyAlgorithm, 'id-dsa') then
  begin
    MessageDlg('The certificate was found to contain a DSA key. The hash algorithm has been switched to SHA1.', mtInformation, [mbOk], 0);
    FSigner.NewSignature.HashAlgorithm := 'SHA1';
  end;

  FSigner.NewSignature.SignatureType := pstPAdES;

  case cbLevel.ItemIndex of
    0 : FSigner.NewSignature.Level := paslGeneric;
    1 : FSigner.NewSignature.Level := paslBaselineB;
    2 : FSigner.NewSignature.Level := paslBaselineT;
    3 : FSigner.NewSignature.Level := paslBaselineLT;
    4 : FSigner.NewSignature.Level := paslBaselineLTA;
    5 : FSigner.NewSignature.Level := paslBES;
    6 : FSigner.NewSignature.Level := paslEPES;
    7 : FSigner.NewSignature.Level := paslLTV;
  else
    FSigner.NewSignature.Level := paslBaselineB;
  end;

  FSigner.Widget.Invisible := not cbVisible.Checked;

  FSigner.Config('IgnoreChainValidationErrors=true');

  try
    FSigner.Sign();

    MessageDlg('PDF file successfully signed', mtInformation, [mbOk], 0);
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormPdfsignerpkiproxy.FormCreate(Sender: TObject);
begin
  FSigner := TsbxPDFSigner.Create(nil);
  FCertStorage := TsbxCertificateStorage.Create(nil);
end;

procedure TFormPdfsignerpkiproxy.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCertStorage);
  FreeAndNil(FSigner);
end;

procedure TFormPdfsignerpkiproxy.sbBrowseInputFileClick(Sender: TObject);
begin
  //dlgOpen.FileName := edInputFile.Text;
  if dlgOpen.Execute then
    edInputFile.Text := dlgOpen.FileName;
end;

procedure TFormPdfsignerpkiproxy.sbOutputFileClick(Sender: TObject);
begin
  dlgSave.InitialDir := ExtractFilePath(Application.ExeName);
  if dlgSave.Execute then
    edOutputFile.Text := dlgSave.FileName;
end;

procedure TFormPdfsignerpkiproxy.btnLoadCertsClick(Sender: TObject);
begin
  if dlgOpenDLL.Execute then
    LoadCertificates(dlgOpenDLL.FileName);
end;

procedure TFormPdfsignerpkiproxy.CloseStorage();
begin
  if FCertStorage.Opened then
  begin
    if StrToInt(FCertStorage.Config('PKCS11ActiveSlot')) <> -1 then
      FCertStorage.Logout(true);

    FCertStorage.Close(false);
  end;

  lvCerts.Items.Clear;
end;

procedure TFormPdfsignerpkiproxy.LoadCertificates(const StoreFilename: string);
var
  i, SlotCount, slotNum: integer;
  SlotList: TStringList;
  Cert: TsbxCertificate;
  Item: TListItem;
  KeyAlgorithm: string;
begin
  CloseStorage();

  try
    FCertStorage.Open('pkcs11:///' + StoreFilename + '?slot=-1');

    SlotList := TStringList.Create;
    try
      SlotList.StrictDelimiter := true;
      SlotList.Text := FCertStorage.ListStores;
      SlotCount := SlotList.Count;

      slotNum := -1;
      for i := 0 to SlotCount - 1 do
      begin
        if Uppercase(FCertStorage.Config('PKCS11SlotTokenPresent[' + IntToStr(i) + ']')) = 'TRUE' then
        begin
          slotNum := i;
          break;
        end;
      end;
    finally
      FreeAndNil(SlotList);
    end;

    if (slotNum <> -1) then
    begin
      { Opening new session }
      FCertStorage.Config('PKCS11Slot=' + IntToStr(slotNum));
      FCertStorage.Login(1{stUser}, edStoragePIN.Text, true);

      for i := 0 to FCertStorage.Certificates.Count - 1 do
      begin
        if FCertStorage.Certificates[i].PrivateKeyExists then
        begin
          Cert := FCertStorage.Certificates[i];
          Item := lvCerts.Items.Add;
          Item.ImageIndex := 3;
          { Subject }
          Item.Caption := Cert.Subject;
          { Issuer }
          Item.SubItems.Add(Cert.Issuer);
          { Validity period }
          Item.SubItems.Add(Cert.ValidFrom);
          Item.SubItems.Add(Cert.ValidTo);
          { Algorithm }
          KeyAlgorithm := Cert.KeyAlgorithm;
          KeyAlgorithm := KeyAlgorithm + ' (' + IntToStr(Cert.KeyBits) + ' bits)';
          Item.SubItems.Add(KeyAlgorithm);
        end;
      end;
    end
    else
    begin
      CloseStorage();
      MessageDlg('Active slot not found',  mtWarning, [mbOk], 0);
      Exit;
    end;
  except
    on E : Exception do
    begin
      CloseStorage();
      MessageDlg('Error opening storage: ' + StoreFilename + #13#10 + E.Message,  mtError, [mbOk], 0);
      Exit;
    end;
  end;
end;

end.

