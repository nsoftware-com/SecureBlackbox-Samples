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
unit kmipclientf;

{$ifdef UNICODE}
  {$define SB_UNICODE_VCL}
{$endif}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StrUtils,
  Menus, ToolWin, ComCtrls, ExtCtrls, StdCtrls, ImgList,
  SBxTypes, SBxKMIPClient, SBxCertificateManager, SBxCryptoKeyManager;

type
  TFormKMIPclient = class(TForm)
    Panel1: TPanel;
    Label10: TLabel;
    lvObjects: TListView;
    bRefresh: TButton;
    bEncrypt: TButton;
    bDecrypt: TButton;
    bSign: TButton;
    bVerify: TButton;
    bSettings: TButton;
    bAddKey: TButton;
    bAddCert: TButton;
    bRemove: TButton;
    Panel2: TPanel;
    bCreateCert: TButton;
    bCreateKey: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bSettingsClick(Sender: TObject);
    procedure bRefreshClick(Sender: TObject);
    procedure lvObjectsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure bAddKeyClick(Sender: TObject);
    procedure bAddCertClick(Sender: TObject);
    procedure bRemoveClick(Sender: TObject);
    procedure bEncryptClick(Sender: TObject);
    procedure bDecryptClick(Sender: TObject);
    procedure bSignClick(Sender: TObject);
    procedure bVerifyClick(Sender: TObject);
    procedure bCreateCertClick(Sender: TObject);
    procedure bCreateKeyClick(Sender: TObject);
  private
    FClient: TsbxKMIPClient;

    procedure DoTLSCertValidate(Sender: TObject; const ServerHost: String;
      const ServerIP: String; var Accept: Boolean);

    procedure ClearObjectList;
    procedure RefreshObjectList;
    procedure SelectItem(Item: TListItem);
  public
    { Public declarations }
  end;

var
  FormKMIPclient: TFormKMIPclient;

implementation


{$R *.DFM}

uses connpropsf, addcertf, addkeyf, operationf, createcertf, CountryList,
  createkeyf;

procedure TFormKMIPclient.ClearObjectList;
begin
  lvObjects.Items.Clear;
end;

procedure TFormKMIPclient.RefreshObjectList;
var
  i: integer;
  Item: TListItem;
begin
  try
    ClearObjectList;

    FClient.List(0, '', 0, 0, false);

    for i := 0 to FClient.Objects.Count - 1 do
    begin
      Item := lvObjects.Items.Add;
      Item.Caption := TsbxKMIPObject(FClient.Objects.Item[i]).ObjectId;
      case TsbxKMIPObject(FClient.Objects.Item[i]).ObjectType of
        otCertificate: Item.SubItems.Add('Certificate');
        otSymmetricKey: Item.SubItems.Add('Symmetric Key');
        otPublicKey: Item.SubItems.Add('Public Key');
        otPrivateKey: Item.SubItems.Add('Private Key');
        else Item.SubItems.Add('Unknown');
      end;

      Item.SubItems.Add(TsbxKMIPObject(FClient.Objects.Item[i]).KeyAlgorithm);
      Item.SubItems.Add(IntToStr(TsbxKMIPObject(FClient.Objects.Item[i]).KeyBits));
      Item.SubItems.Add(TsbxKMIPObject(FClient.Objects.Item[i]).ObjectGroup);
      Item.Data := FClient.Objects.Item[i];
    end;

    SelectItem(nil);
  except
    on E : Exception do
    begin
      MessageDlg(E.Message, mtError, [mbOk], 0);
    end;
  end;
end;

procedure TFormKMIPclient.SelectItem(Item: TListItem);
begin
  if Assigned(Item) and Assigned(Item.Data) then
  begin
    bRemove.Enabled := true;

    case TsbxKMIPObject(Item.Data).ObjectType of
      otCertificate:
        begin
          bEncrypt.Enabled := true;
          bDecrypt.Enabled := true;
          bSign.Enabled := true;
          bVerify.Enabled := true;
        end;
      otSymmetricKey:
        begin
          bEncrypt.Enabled := true;
          bDecrypt.Enabled := true;
          bSign.Enabled := false;
          bVerify.Enabled := false;
        end;
      otPublicKey:
        begin
          if ContainsText(TsbxKMIPObject(Item.Data).KeyAlgorithm, 'rsa') then
            bEncrypt.Enabled := true
          else
            bEncrypt.Enabled := false;
          bDecrypt.Enabled := false;
          bSign.Enabled := false;
          bVerify.Enabled := true;
        end;
      otPrivateKey:
        begin
          bEncrypt.Enabled := false;
          if ContainsText(TsbxKMIPObject(Item.Data).KeyAlgorithm, 'rsa') then
            bDecrypt.Enabled := true
          else
            bDecrypt.Enabled := false;
          bSign.Enabled := true;
          bVerify.Enabled := false;
        end;
      else
        begin
          bEncrypt.Enabled := false;
          bDecrypt.Enabled := false;
          bSign.Enabled := false;
          bVerify.Enabled := false;
        end;
    end;
  end
  else
  begin
    bRemove.Enabled := false;
    bEncrypt.Enabled := false;
    bDecrypt.Enabled := false;
    bSign.Enabled := false;
    bVerify.Enabled := false;
  end;
end;

procedure TFormKMIPclient.bAddCertClick(Sender: TObject);
var
  frmAddCert: TFormAddCert;
  Manager: TsbxCertificateManager;
begin
  frmAddCert := TFormAddCert.Create(nil);
  try
    if frmAddCert.ShowModal = mrOk then
    begin
      Manager := TsbxCertificateManager.Create(nil);
      try
        try
          Manager.ImportFromFile(frmAddCert.edCertFile.Text, frmAddCert.edCertPassword.Text);

          FClient.Certificate := Manager.Certificate;

          FClient.Add(true, frmAddCert.edGroup.Text, true);

          RefreshObjectList;
        except
          on E : Exception do
          begin
            MessageDlg(E.Message, mtError, [mbOk], 0);
          end;
        end;
      finally
        FreeAndNil(Manager);
      end;
    end;
  finally
    FreeAndNil(frmAddCert);
  end;
end;

procedure TFormKMIPclient.bAddKeyClick(Sender: TObject);
var
  frmAddKey: TFormAddKey;
  Manager: TsbxCryptoKeyManager;
begin
  frmAddKey := TFormAddKey.Create(nil);
  try
    if frmAddKey.ShowModal = mrOk then
    begin
      Manager := TsbxCryptoKeyManager.Create(nil);
      try
        try
          Manager.ImportFromFile(frmAddKey.edKeyFile.Text, 1, '', frmAddKey.cbCurve.Text, '', 0, '');

          FClient.Key := Manager.Key;

          FClient.AddKey(frmAddKey.edGroup.Text, true);

          RefreshObjectList;
        except
          on E : Exception do
          begin
            MessageDlg(E.Message, mtError, [mbOk], 0);
          end;
        end;
      finally
        FreeAndNil(Manager);
      end;
    end;
  finally
    FreeAndNil(frmAddKey);
  end;
end;

procedure TFormKMIPclient.bCreateCertClick(Sender: TObject);
var
  frmCreateCert: TFormCreateCert;
  i: integer;
  KeyIds, SubjectRDN: string;
  TempCert: TsbxCertificate;
begin
  frmCreateCert := TFormCreateCert.Create(nil);
  try
    if frmCreateCert.ShowModal = mrOk then
    begin
      try
        KeyIds := FClient.GenerateKey(frmCreateCert.cbPublicAlgorithm.Text, frmCreateCert.Curve, '',
          StrToIntDef(frmCreateCert.edKeyLength.Text, 0), frmCreateCert.edGroup.Text, true);

        // Remove private key id
        Delete(KeyIds, 1, pos(FClient.Config('ListDelimiter'), KeyIds));

        TempCert := TsbxCertificate.Create();
        try
          TempCert.SubjectRDN := '/C=' + GetCountryAbbr(frmCreateCert.cbCountryS.Text) +
            '/ST=' + frmCreateCert.edStateS.Text +
            '/L=' + frmCreateCert.edLocalityS.Text +
            '/O=' + frmCreateCert.edOrganizationS.Text +
            '/OU=' + frmCreateCert.edOrganizationUnitS.Text +
            '/CN=' + frmCreateCert.edCommonNameS.Text;

          FClient.Certificate := TempCert;

          FClient.Generate(KeyIds, true);
        finally
          FreeAndNil(TempCert);
        end;

        RefreshObjectList;
      except
        on E : Exception do
        begin
          MessageDlg(E.Message, mtError, [mbOk], 0);
        end;
      end;
    end;
  finally
    FreeAndNil(frmCreateCert);
  end;
end;

procedure TFormKMIPclient.bCreateKeyClick(Sender: TObject);
var
  frmCreateKey: TFormCreateKey;
begin
  frmCreateKey := TFormCreateKey.Create(nil);
  try
    if frmCreateKey.ShowModal = mrOk then
    begin
      try
        FClient.GenerateKey(frmCreateKey.cbPublicAlgorithm.Text, frmCreateKey.Curve, '',
          StrToIntDef(frmCreateKey.edKeyLength.Text, 0), frmCreateKey.edGroup.Text, true);

        RefreshObjectList;
      except
        on E : Exception do
        begin
          MessageDlg(E.Message, mtError, [mbOk], 0);
        end;
      end;
    end;
  finally
    FreeAndNil(frmCreateKey);
  end;
end;

procedure TFormKMIPclient.bDecryptClick(Sender: TObject);
var
  OperationForm: TFormOperation;
begin
  if Assigned(lvObjects.Selected) and Assigned(lvObjects.Selected.Data) then
  begin
    OperationForm := TFormOperation.Create(nil);
    try
      OperationForm.Init(FClient, TsbxKMIPObject(lvObjects.Selected.Data).ObjectId, Op_Decrypt);

      OperationForm.ShowModal;
    finally
      FreeAndNil(OperationForm);
    end;
  end;
end;

procedure TFormKMIPclient.bEncryptClick(Sender: TObject);
var
  OperationForm: TFormOperation;
begin
  if Assigned(lvObjects.Selected) and Assigned(lvObjects.Selected.Data) then
  begin
    OperationForm := TFormOperation.Create(nil);
    try
      OperationForm.Init(FClient, TsbxKMIPObject(lvObjects.Selected.Data).ObjectId, Op_Encrypt);

      OperationForm.ShowModal;
    finally
      FreeAndNil(OperationForm);
    end;
  end;
end;

procedure TFormKMIPclient.bRefreshClick(Sender: TObject);
begin
  RefreshObjectList;
end;

procedure TFormKMIPclient.bRemoveClick(Sender: TObject);
var
  ObjType: string;
begin
  if Assigned(lvObjects.Selected) and Assigned(lvObjects.Selected.Data) then
  begin
    case TsbxKMIPObject(lvObjects.Selected.Data).ObjectType of
        otCertificate: ObjType := 'Certificate';
        otSymmetricKey: ObjType := 'Symmetric Key';
        otPublicKey: ObjType := 'Public Key';
        otPrivateKey: ObjType := 'Private Key';
        else ObjType := 'Object';
      end;

    if MessageDlg('Remove ' + ObjType + ' "' + TsbxKMIPObject(lvObjects.Selected.Data).ObjectId + '"?', mtConfirmation, mbOKCancel, 0) = mrOK then
    begin
      try
        FClient.Remove(TsbxKMIPObject(lvObjects.Selected.Data).ObjectId);

        RefreshObjectList;
      except
        on E : Exception do
        begin
          MessageDlg(E.Message, mtError, [mbOk], 0);
        end;
      end;
    end;
  end;
end;

procedure TFormKMIPclient.bSettingsClick(Sender: TObject);
begin
  if FormConnprops.ShowModal = mrOk then
  begin
    FClient.BaseURL := FormConnprops.editURL.Text;

    case FormConnprops.rgEncoder.ItemIndex of
      1 : FClient.Encoding := etXML;
      2 : FClient.Encoding := etJSON;
      else
        FClient.Encoding := etTTLV;
    end;

    FClient.Username := FormConnprops.editUsername.Text;
    FClient.Password := FormConnprops.editPassword.Text;

    RefreshObjectList;
  end;
end;

procedure TFormKMIPclient.bSignClick(Sender: TObject);
var
  OperationForm: TFormOperation;
begin
  if Assigned(lvObjects.Selected) and Assigned(lvObjects.Selected.Data) then
  begin
    OperationForm := TFormOperation.Create(nil);
    try
      OperationForm.Init(FClient, TsbxKMIPObject(lvObjects.Selected.Data).ObjectId, Op_Sign);

      OperationForm.ShowModal;
    finally
      FreeAndNil(OperationForm);
    end;
  end;
end;

procedure TFormKMIPclient.bVerifyClick(Sender: TObject);
var
  OperationForm: TFormOperation;
begin
  if Assigned(lvObjects.Selected) and Assigned(lvObjects.Selected.Data) then
  begin
    OperationForm := TFormOperation.Create(nil);
    try
      OperationForm.Init(FClient, TsbxKMIPObject(lvObjects.Selected.Data).ObjectId, Op_Verify);

      OperationForm.ShowModal;
    finally
      FreeAndNil(OperationForm);
    end;
  end;
end;

procedure TFormKMIPclient.lvObjectsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  SelectItem(Item);
end;

procedure TFormKMIPclient.FormCreate(Sender: TObject);
begin
  FClient := TsbxKMIPClient.Create(nil);
  FClient.Config('BlockSize=16000');
  FClient.OnTLSCertValidate := DoTLSCertValidate;
end;

procedure TFormKMIPclient.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FClient);
end;

procedure TFormKMIPclient.DoTLSCertValidate(Sender: TObject;
  const ServerHost: String; const ServerIP: String; var Accept: Boolean);
begin
  Accept := true;
end;

end.




