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
unit kmipclientf;

{$ifdef UNICODE}
  {$define SB_UNICODE_VCL}
{$endif}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StrUtils,
  Menus, ToolWin, ComCtrls, ExtCtrls, StdCtrls, ImgList,
  SBxTypes, SBxKMIPClient;

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

    FClient.GetList(0);

    for i := 0 to FClient.Objects.Count - 1 do
    begin
      Item := lvObjects.Items.Add;
      Item.Caption := TsbxKMIPObject(FClient.Objects.Item[i]).UniqueIdentifier;
      case TsbxKMIPObject(FClient.Objects.Item[i]).ObjectType of
        otCertificate: Item.SubItems.Add('Certificate');
        otSymmetricKey: Item.SubItems.Add('Symmetric Key');
        otPublicKey: Item.SubItems.Add('Public Key');
        otPrivateKey: Item.SubItems.Add('Private Key');
        else Item.SubItems.Add('Unknown');
      end;

      Item.SubItems.Add(TsbxKMIPObject(FClient.Objects.Item[i]).KeyAlgorithm);
      Item.SubItems.Add(IntToStr(TsbxKMIPObject(FClient.Objects.Item[i]).KeyLength));
      Item.SubItems.Add(TsbxKMIPObject(FClient.Objects.Item[i]).ID);
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
begin
  frmAddCert := TFormAddCert.Create(nil);
  try
    if frmAddCert.ShowModal = mrOk then
    begin
      FClient.InputFile := frmAddCert.edCertFile.Text;

      try
        FClient.AddCertificate(frmAddCert.edCertPassword.Text, true, frmAddCert.edCertId.Text);

        RefreshObjectList;
      except
        on E : Exception do
        begin
          MessageDlg(E.Message, mtError, [mbOk], 0);
        end;
      end;
    end;
  finally
    FreeAndNil(frmAddCert);
  end;
end;

procedure TFormKMIPclient.bAddKeyClick(Sender: TObject);
var
  frmAddKey: TFormAddKey;
begin
  frmAddKey := TFormAddKey.Create(nil);
  try
    if frmAddKey.ShowModal = mrOk then
    begin
      FClient.Config('Curve=' + frmAddKey.cbCurve.Text);
      FClient.InputFile := frmAddKey.edKeyFile.Text;

      try
        FClient.AddKey(frmAddKey.edKeyId.Text);

        RefreshObjectList;
      except
        on E : Exception do
        begin
          MessageDlg(E.Message, mtError, [mbOk], 0);
        end;
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
  SubjectRDN: string;
begin
  frmCreateCert := TFormCreateCert.Create(nil);
  try
    if frmCreateCert.ShowModal = mrOk then
    begin
      SubjectRDN := '/C=' + GetCountryAbbr(frmCreateCert.cbCountryS.Text) +
                    '/ST=' + frmCreateCert.edStateS.Text +
                    '/L=' + frmCreateCert.edLocalityS.Text +
                    '/O=' + frmCreateCert.edOrganizationS.Text +
                    '/OU=' + frmCreateCert.edOrganizationUnitS.Text +
                    '/CN=' + frmCreateCert.edCommonNameS.Text;

      FClient.Config('Curve=' + frmCreateCert.Curve);

      try
        FClient.GenerateCert('', frmCreateCert.cbPublicAlgorithm.Text, frmCreateCert.cbHashAlgorithm.Text,
          StrToIntDef(frmCreateCert.edKeyLength.Text, 0), SubjectRDN, frmCreateCert.edCertId.Text);

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
      FClient.Config('Curve=' + frmCreateKey.Curve);

      try
        FClient.GenerateKey(frmCreateKey.cbPublicAlgorithm.Text,
          StrToIntDef(frmCreateKey.edKeyLength.Text, 0), frmCreateKey.edCertId.Text);

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
      OperationForm.Init(FClient, TsbxKMIPObject(lvObjects.Selected.Data).UniqueIdentifier, Op_Decrypt);

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
      OperationForm.Init(FClient, TsbxKMIPObject(lvObjects.Selected.Data).UniqueIdentifier, Op_Encrypt);

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

    if MessageDlg('Remove ' + ObjType + ' "' + TsbxKMIPObject(lvObjects.Selected.Data).UniqueIdentifier + '"?', mtConfirmation, mbOKCancel, 0) = mrOK then
    begin
      try
        FClient.Remove(TsbxKMIPObject(lvObjects.Selected.Data).UniqueIdentifier);

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
    FClient.Host := FormConnprops.editHost.Text;
    FClient.Port := StrToInt(FormConnprops.editPort.Text);

    case FormConnprops.rgEncoder.ItemIndex of
      1 : FClient.EncoderType := etXML;
      2 : FClient.EncoderType := etJSON;
      else
        FClient.EncoderType := etTTLV;
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
      OperationForm.Init(FClient, TsbxKMIPObject(lvObjects.Selected.Data).UniqueIdentifier, Op_Sign);

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
      OperationForm.Init(FClient, TsbxKMIPObject(lvObjects.Selected.Data).UniqueIdentifier, Op_Verify);

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
end;

procedure TFormKMIPclient.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FClient);
end;

end.

