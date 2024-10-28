unit referencef;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  SBxTypes;

type
  TFormReference = class(TForm)
    lbTargetXMLElement: TLabel;
    lbHashAlgorithm: TLabel;
    lbID: TLabel;
    edTargetXMLElement: TEdit;
    cmbHashAlgorithm: TComboBox;
    mmData: TMemo;
    edID: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    edRefType: TEdit;
    lbRefType: TLabel;
    lbTargetData: TLabel;
    cbAutoGenerateElementId: TCheckBox;
    cmbCanonMethod: TComboBox;
    lbCanonMethod: TLabel;
  private
  public
    procedure Initialize(const Value: TsbxXMLReference);
    procedure Update(Value: TsbxXMLReference);
  end;

var
  FormReference: TFormReference;

implementation

{$R *.dfm}

{ TfrmReference }

procedure TFormReference.Initialize(const Value: TsbxXMLReference);
var
  i: integer;
begin
  edID.Text := Value.ID;
  edRefType.Text := Value.ReferenceType;

  for i := 0 to cmbHashAlgorithm.Items.Count - 1 do
    if cmbHashAlgorithm.Items[i] = Value.HashAlgorithm then
    begin
      cmbHashAlgorithm.ItemIndex := i;
      Break;
    end;

  edTargetXMLElement.Text := Value.TargetXMLElement;
  cbAutoGenerateElementId.Checked := Value.AutoGenerateElementId;

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

  mmData.Text := TEncoding.UTF8.GetString(Value.TargetData);
end;

procedure TFormReference.Update(Value: TsbxXMLReference);
begin
  Value.ID := edID.Text;
  Value.ReferenceType := edRefType.Text;
  Value.HashAlgorithm := cmbHashAlgorithm.Text;

  Value.TargetXMLElement := edTargetXMLElement.Text;
  Value.AutoGenerateElementId := cbAutoGenerateElementId.Checked;

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

  Value.TargetData := TEncoding.UTF8.GetBytes(mmData.Text);
end;

end.
