unit referencef;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  SBxTypes, SBxCore;

type
  TFormReference = class(TForm)
    lbHashAlgorithm: TLabel;
    lbReferenceId: TLabel;
    cmbHashAlgorithm: TComboBox;
    edReferenceId: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    edReferenceType: TEdit;
    lbReferenceType: TLabel;
    lbReferenceURI: TLabel;
    edReferenceURI: TEdit;
    GroupBox1: TGroupBox;
    rbTargetXMLElement: TRadioButton;
    edTargetXMLElement: TEdit;
    cbAutoGenerateElementId: TCheckBox;
    lbCustomId: TLabel;
    edCustomId: TEdit;
    Label10: TLabel;
    rbTargetData: TRadioButton;
    mmData: TMemo;
    GroupBox2: TGroupBox;
    lbCanonMethod: TLabel;
    cmbCanonMethod: TComboBox;
    cbUseEnvelopedSignatureTransform: TCheckBox;
    procedure mmDataChange(Sender: TObject);
    procedure edTargetXMLElementChange(Sender: TObject);
  private
  public
    procedure Initialize(Value: TsbxXMLReference);
    procedure UpdateReference(Value: TsbxXMLReference);
  end;

var
  FormReference: TFormReference;

implementation

{$R *.dfm}

{ TfrmReference }

procedure TFormReference.Initialize(Value: TsbxXMLReference);
var
  {$ifndef UNICODE}
  s : string;
  {$endif}
  I: Integer;
begin
  if not Assigned(Value) then
    Exit;

  edReferenceId.Text := Value.ID;
  edReferenceType.Text := Value.ReferenceType;
  edReferenceURI.Text := Value.URI;

  cmbHashAlgorithm.ItemIndex := 0;
  for i := 0 to cmbHashAlgorithm.Items.Count - 1 do
    if cmbHashAlgorithm.Items[i] = Value.HashAlgorithm then
    begin
      cmbHashAlgorithm.ItemIndex := i;
      Break;
    end;

  edTargetXMLElement.Text := Value.TargetXMLElement;
  edCustomId.Text := Value.CustomElementId;
  cbAutoGenerateElementId.Checked := Value.AutoGenerateElementId;

  {$ifdef UNICODE}
  mmData.Text := TEncoding.UTF8.GetString(Value.TargetData);
  {$else}
  SetLength(s, Length(Value.TargetData));
  for i := 1 to Length(s) do
    s[i] := Chr(Value.TargetData[i - 1]);

  mmData.Text := s;
  {$endif}

  if (Length(Value.TargetXMLElement) > 0) or (Length(Value.TargetData) = 0) then
    rbTargetXMLElement.Checked := true
  else
    rbTargetData.Checked := true;

  case (Value.CanonicalizationMethod) of
    cxcmCanon:
      cmbCanonMethod.ItemIndex := 0;
    cxcmCanonComment:
      cmbCanonMethod.ItemIndex := 1;
    cxcmCanon_v1_1:
      cmbCanonMethod.ItemIndex := 2;
    cxcmCanonComment_v1_1:
      cmbCanonMethod.ItemIndex := 3;
    cxcmExclCanon:
      cmbCanonMethod.ItemIndex := 4;
    cxcmExclCanonComment:
      cmbCanonMethod.ItemIndex := 5;
    cxcmMinCanon:
      cmbCanonMethod.ItemIndex := 6;
    cxcmNone:
      cmbCanonMethod.ItemIndex := 7;
    else
      cmbCanonMethod.ItemIndex := 0;
  end;

  cbUseEnvelopedSignatureTransform.Checked := Value.UseEnvelopedSignatureTransform;
end;

procedure TFormReference.UpdateReference(Value: TsbxXMLReference);
{$ifndef UNICODE}
var
  Buf : TBytes;
  I: Integer;
{$endif}
begin
  if not Assigned(Value) then
    Exit;

  Value.ID := edReferenceId.Text;
  Value.ReferenceType := edReferenceType.Text;
  Value.URI := edReferenceURI.Text;
  Value.HashAlgorithm := cmbHashAlgorithm.Text;

  if rbTargetXMLElement.Checked then
  begin
    Value.TargetXMLElement := edTargetXMLElement.Text;
    Value.CustomElementId := edCustomId.Text;
    Value.AutoGenerateElementId := cbAutoGenerateElementId.Checked;
  end;

  if rbTargetData.Checked then
  begin
    {$ifdef UNICODE}
    Value.TargetData := TEncoding.UTF8.GetBytes(mmData.Text);
    {$else}
    SetLength(Buf, Length(mmData.Text));
    for i := 0 to Length(Buf) - 1 do
      Buf[i] := Ord(mmData.Text[i + 1]);

    Value.TargetData := Buf;
    {$endif}
  end;

  case (cmbCanonMethod.ItemIndex) of
    0:
      Value.CanonicalizationMethod := cxcmCanon;
    1:
      Value.CanonicalizationMethod := cxcmCanonComment;
    2:
      Value.CanonicalizationMethod := cxcmCanon_v1_1;
    3:
      Value.CanonicalizationMethod := cxcmCanonComment_v1_1;
    4:
      Value.CanonicalizationMethod := cxcmExclCanon;
    5:
      Value.CanonicalizationMethod := cxcmExclCanonComment;
    6:
      Value.CanonicalizationMethod := cxcmMinCanon;
    7:
      Value.CanonicalizationMethod := cxcmNone;
    else
      Value.CanonicalizationMethod := cxcmCanon;
  end;

  Value.UseEnvelopedSignatureTransform := cbUseEnvelopedSignatureTransform.Checked;
end;

procedure TFormReference.mmDataChange(Sender: TObject);
begin
  rbTargetData.Checked := true;
end;

procedure TFormReference.edTargetXMLElementChange(Sender: TObject);
begin
  rbTargetXMLElement.Checked := true;
end;

end.
