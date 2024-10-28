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
unit pkcs11certificatestoragef;

interface

uses
  Windows, Messages, SysUtils, StdCtrls, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ImgList, ComCtrls, ToolWin, ExtCtrls, Buttons,
  SBxTypes, SBxCertificateStorage, System.ImageList;

type
  TFormPKCS11CertificateStorage = class(TForm)
    Panel2: TPanel;
    Panel1: TPanel;
    Label10: TLabel;
    pClient: TPanel;
    lvCerts: TListView;
    pToolbar: TPanel;
    cbToolbar: TCoolBar;
    pToolbarButtons: TPanel;
    btnAddCert: TSpeedButton;
    btnRemoveCert: TSpeedButton;
    Bevel: TBevel;
    lSlots: TLabel;
    btnFiles: TSpeedButton;
    cbSlots: TComboBox;
    bOpenSlot: TBitBtn;
    MainMenu: TMainMenu;
    mnuFile: TMenuItem;
    mnuOpenStorage: TMenuItem;
    mnuSaveStorage: TMenuItem;
    mnuCloseStorage: TMenuItem;
    mnuN: TMenuItem;
    mnuExit: TMenuItem;
    ImageList: TImageList;
    OpenDialog: TOpenDialog;
    OpenDialogCert: TOpenDialog;
    procedure btnAddCertClick(Sender: TObject);
    procedure btnRemoveCertClick(Sender: TObject);
    procedure btnFilesClick(Sender: TObject);
    procedure bOpenSlotClick(Sender: TObject);
    procedure mnuOpenStorageClick(Sender: TObject);
    procedure mnuSaveStorageClick(Sender: TObject);
    procedure mnuCloseStorageClick(Sender: TObject);
    procedure cbSlotsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure cbSlotsChange(Sender: TObject);
    procedure lvCertsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    StorageFilename: string;
    CertPassword: string;
    CertStorage: TsbxCertificateStorage;

    procedure OpenStorage;
    procedure CloseStorage(Save: boolean);
    procedure OpenSlot;
    procedure ExitApp;
    procedure AddCertificate;
    procedure RemoveCertificate;
    procedure RefreshCertificates;
    procedure ProcessFiles;
    procedure SetupButtons;
    function RequestPassword(const Caption: string; const Prompt: string;
      var Pass: string): boolean;

    procedure DoPasswordNeeded(Sender: TObject; const NeededFor: String; var Password: String; var Cancel: Boolean);
  public
    { Public declarations }
  end;

var
  FormPKCS11CertificateStorage: TFormPKCS11CertificateStorage;

implementation

uses pdfprocessorf, pinf;

{$R *.DFM}

procedure TFormPKCS11CertificateStorage.DoPasswordNeeded(Sender: TObject; const NeededFor: String;
  var Password: String; var Cancel: Boolean);
begin
  Password := CertPassword;
end;

procedure TFormPKCS11CertificateStorage.FormCreate(Sender: TObject);
begin
  CertStorage := TsbxCertificateStorage.Create(nil);
  CertStorage.OnPasswordNeeded := DoPasswordNeeded;

  StorageFilename := '';
  CertPassword := '';
end;

procedure TFormPKCS11CertificateStorage.FormDestroy(Sender: TObject);
begin
  FreeAndNil(CertStorage);
end;

procedure TFormPKCS11CertificateStorage.mnuOpenStorageClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    StorageFilename := OpenDialog.Filename;
    OpenStorage;
  end;
end;

procedure TFormPKCS11CertificateStorage.mnuSaveStorageClick(Sender: TObject);
begin
  CloseStorage(true);
end;

procedure TFormPKCS11CertificateStorage.mnuCloseStorageClick(Sender: TObject);
begin
  CloseStorage(false);
end;

procedure TFormPKCS11CertificateStorage.mnuExitClick(Sender: TObject);
begin
  ExitApp;
end;
(*
procedure TFormPKCS11CertificateStorage.OpenStorage;
var
  i, SlotCount, TokenPresent: integer;
begin
  CloseStorage(false);
  cbSlots.Items.Add('<Please select a slot>');

  try
    CertStorage.Open('pkcs11:///' + StorageFilename + '?slot=-1');

    SlotCount := StrToIntDef(CertStorage.Config('PKCS11SlotCount'), 0);

    for i := 0 to SlotCount - 1 do
    begin
      if Uppercase(CertStorage.Config('PKCS11SlotTokenPresent[' + IntToStr(i) + ']')) = 'TRUE' then
        TokenPresent := 1
      else
        TokenPresent := 0;

      cbSlots.Items.AddObject(CertStorage.Config('PKCS11SlotDescription[' + IntToStr(i) + ']'), TObject(TokenPresent));
    end;

    cbSlots.ItemIndex := 0;
  except
    on E : Exception do
    begin
      CloseStorage(false);
      MessageDlg('Error opening storage: ' + StorageFilename + #13#10 + E.Message,  mtError, [mbOk], 0);
      Exit;
    end;
  end;

  SetupButtons;
end;

procedure TFormPKCS11CertificateStorage.CloseStorage(Save: boolean);
begin
  if CertStorage.Opened then
    CertStorage.Close(Save);

  lvCerts.Items.Clear;
  cbSlots.Items.Clear;

  SetupButtons;
end;

procedure TFormPKCS11CertificateStorage.OpenSlot;
var
  Pin: string;
  RO: string;
begin
  { Checking whether we can establish a new session }
  if cbSlots.ItemIndex <= 0 then
    Exit;

  if Integer(cbSlots.Items.Objects[cbSlots.ItemIndex]) = 0 then // not TokenPresent
  begin
    MessageDlg('Token not found in specified slot', mtError, [mbOk], 0);
    Exit;
  end;

  if Uppercase(CertStorage.Config('PKCS11SlotReadOnly[' + IntToStr(cbSlots.ItemIndex - 1) + ']')) = 'TRUE' then
    RO := '&readonly=1'
  else
    RO := '&readonly=0';

  if RequestPassword('PIN request', 'Please enter your PIN:', Pin) then
  begin
    if CertStorage.Opened then
      CertStorage.Close(false);

    { Opening new session }
    try
      CertStorage.Open('pkcs11://user:' + Pin + '@/' + StorageFilename + '?slot=' + IntToStr(cbSlots.ItemIndex - 1) + RO);

      RefreshCertificates;
      SetupButtons;
    except
      on E : Exception do
      begin
        MessageDlg('Error opening slot: ' + E.Message, mtError, [mbOk], 0);
        OpenStorage;
        Exit;
      end;
    end;
  end;
end;
*)

procedure TFormPKCS11CertificateStorage.OpenStorage;
var
  i, SlotCount, TokenPresent: integer;
  SlotList: TStringList;
begin
  CloseStorage(false);
  cbSlots.Items.Add('<Please select a slot>');

  try
    CertStorage.Open('pkcs11:///' + StorageFilename + '?slot=-1');

    SlotList := TStringList.Create;
    try
      SlotList.StrictDelimiter := true;
      SlotList.Text := CertStorage.ListStores;
      SlotCount := SlotList.Count;

      for i := 0 to SlotCount - 1 do
      begin
        if Uppercase(CertStorage.Config('PKCS11SlotTokenPresent[' + IntToStr(i) + ']')) = 'TRUE' then
          TokenPresent := 1
        else
          TokenPresent := 0;

        cbSlots.Items.AddObject(SlotList[i], TObject(TokenPresent));
      end;
    finally
      FreeAndNil(SlotList);
    end;

    cbSlots.ItemIndex := 0;
  except
    on E : Exception do
    begin
      CloseStorage(false);
      MessageDlg('Error opening storage: ' + StorageFilename + #13#10 + E.Message,  mtError, [mbOk], 0);
      Exit;
    end;
  end;

  SetupButtons;
end;

procedure TFormPKCS11CertificateStorage.CloseStorage(Save: boolean);
begin
  if CertStorage.Opened then
  begin
    if StrToInt(CertStorage.Config('PKCS11ActiveSlot')) <> -1 then
      CertStorage.Logout(true);

    CertStorage.Close(Save);
  end;

  lvCerts.Items.Clear;
  cbSlots.Items.Clear;

  SetupButtons;
end;

procedure TFormPKCS11CertificateStorage.OpenSlot;
var
  Pin: string;
  RO: boolean;
begin
  { Checking whether we can establish a new session }
  if cbSlots.ItemIndex <= 0 then
    Exit;

  if Integer(cbSlots.Items.Objects[cbSlots.ItemIndex]) = 0 then // not TokenPresent
  begin
    MessageDlg('Token not found in specified slot', mtError, [mbOk], 0);
    Exit;
  end;

  if Uppercase(CertStorage.Config('PKCS11SlotReadOnly[' + IntToStr(cbSlots.ItemIndex - 1) + ']')) = 'TRUE' then
    RO := true
  else
    RO := false;

  if RequestPassword('PIN request', 'Please enter your PIN:', Pin) then
  begin
    if StrToInt(CertStorage.Config('PKCS11ActiveSlot')) <> -1 then
      CertStorage.Logout(true);

    { Opening new session }
    try
      CertStorage.Config('PKCS11Slot=' + IntToStr(cbSlots.ItemIndex - 1));
      CertStorage.Login(1{stUser}, Pin, RO);

      RefreshCertificates;
      SetupButtons;
    except
      on E : Exception do
      begin
        MessageDlg('Error opening slot: ' + E.Message, mtError, [mbOk], 0);
        OpenStorage;
        Exit;
      end;
    end;
  end;
end;

procedure TFormPKCS11CertificateStorage.ExitApp;
begin
  CloseStorage(false);
  Close;
end;

procedure TFormPKCS11CertificateStorage.AddCertificate;
begin
  if OpenDialogCert.Execute then
  begin
    if RequestPassword('Password request', 'Please enter password for certificate:', CertPassword) then
    begin
      try
        CertStorage.ImportFromFile(OpenDialogCert.FileName, CertPassword, false);

        RefreshCertificates;
      except
        on E : Exception do
        begin
          MessageDlg('Failed to load certificate: ' + E.Message, mtError, [mbOk], 0);
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TFormPKCS11CertificateStorage.RemoveCertificate;
var
  S : string;
  Cert : TSBxCertificate;
begin
  if (lvCerts.Selected <> nil) and (lvCerts.Selected.Index < CertStorage.Certificates.Count) then
  begin
    Cert := CertStorage.Certificates[lvCerts.Selected.Index];
    S := 'Subject: ' + Cert.Subject + #13#10 +
      'Issuer: ' + Cert.Issuer;
    if MessageDlg('The following certificate will be deleted:'#13#10 + S,
      mtWarning, [mbYes, mbNo], 0) = mrYes then
    begin
      CertStorage.Remove(lvCerts.Selected.Index);
      RefreshCertificates;
    end;
  end;
end;

procedure TFormPKCS11CertificateStorage.RefreshCertificates;
var
  i: integer;
  Cert: TsbxCertificate;
  Item: TListItem;
  S: string;
begin
  lvCerts.Items.Clear;

  for i := 0 to CertStorage.Certificates.Count - 1 do
  begin
    Cert := CertStorage.Certificates[i];
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
    S := Cert.KeyAlgorithm;
    S := S + ' (' + IntToStr(Cert.KeyBits) + ' bits)';
    if Cert.PrivateKeyExists then
      S := S + ' (priv)';
    Item.SubItems.Add(S);
  end;
end;

procedure TFormPKCS11CertificateStorage.SetupButtons;
begin
  btnAddCert.Enabled := CertStorage.Opened and (StrToIntDef(CertStorage.Config('PKCS11ActiveSlot'), -1) >= 0);
  btnRemoveCert.Enabled := CertStorage.Opened and (StrToIntDef(CertStorage.Config('PKCS11ActiveSlot'), -1) >= 0);
  mnuSaveStorage.Enabled := CertStorage.Opened;
  mnuCloseStorage.Enabled := CertStorage.Opened;
  bOpenSlot.Enabled := CertStorage.Opened;
end;

function TFormPKCS11CertificateStorage.RequestPassword(const Caption: string; const Prompt: string;
  var Pass: string): boolean;
begin
  FormPin.lPrompt.Caption := Prompt;
  FormPin.Caption := Caption;
  if FormPin.ShowModal = mrOk then
  begin
    Pass := FormPin.EditPin.Text;
    Result := true;
  end
  else
    Result := false;
end;

procedure TFormPKCS11CertificateStorage.cbSlotsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  Img : integer;
begin
  cbSlots.Canvas.FillRect(Rect);
  if (Index <> 0) then
  begin
    if Integer(cbSlots.Items.Objects[Index]) = 1 then
      Img := 1
    else
      Img := 2;
    ImageList.Draw(cbSlots.Canvas, Rect.Left, Rect.Top, Img);
  end;
  cbSlots.Canvas.TextOut(Rect.Left + ImageList.Width + 2, Rect.Top,
    cbSlots.Items[Index]);
end;

procedure TFormPKCS11CertificateStorage.cbSlotsChange(Sender: TObject);
begin
  bOpenSlot.Enabled := cbSlots.ItemIndex > 0;
end;

procedure TFormPKCS11CertificateStorage.lvCertsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  btnRemoveCert.Enabled := Item <> nil;
  btnFiles.Enabled := Item <> nil;
end;

procedure TFormPKCS11CertificateStorage.bOpenSlotClick(Sender: TObject);
begin
  OpenSlot;
end;

procedure TFormPKCS11CertificateStorage.btnAddCertClick(Sender: TObject);
begin
  AddCertificate;
end;

procedure TFormPKCS11CertificateStorage.btnRemoveCertClick(Sender: TObject);
begin
  if lvCerts.Selected <> nil then
    RemoveCertificate;
end;

procedure TFormPKCS11CertificateStorage.btnFilesClick(Sender: TObject);
begin
  if lvCerts.Selected <> nil then
    ProcessFiles;
end;

procedure TFormPKCS11CertificateStorage.ProcessFiles;
begin
  if (lvCerts.Selected <> nil) and (lvCerts.Selected.Index < CertStorage.Certificates.Count) then
  begin
    FormPDFProcessor.Cert := CertStorage.Certificates[lvCerts.Selected.Index];
    FormPDFProcessor.ShowModal;
  end;
end;

end.
