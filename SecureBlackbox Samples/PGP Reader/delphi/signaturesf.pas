unit signaturesf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls,
  SBxTypes, SBxCore, SBxPGPKeyring;

type
  TFormSignatures = class(TForm)
    lvSignatures: TListView;
  private
    { Private declarations }
  public
    procedure Init(Signatures: TsbxPGPSignatureList; pgpKeyring: TsbxPGPKeyring);
  end;

var
  FormSignatures: TFormSignatures;

implementation

{$R *.DFM}

{ TfrmSignatures }

procedure TFormSignatures.Init(Signatures: TsbxPGPSignatureList; pgpKeyring: TsbxPGPKeyring);
var
  i, j: Integer;
  Item: TListItem;
  Key: TsbxPGPKey;
  Signature: TsbxPGPSignature;
  userID, sigVal: string;
begin
  Key := nil;

  lvSignatures.Items.Clear();
  for i := 0 to Signatures.Count - 1 do
  begin
    Signature := TsbxPGPSignature(Signatures.Item[i]);

    item := lvSignatures.Items.Add();

    userID := 'Unknown Key';
    for j := 0 to pgpKeyring.Keys.Count - 1 do
    begin
      Key := TsbxPGPKey(pgpKeyring.Keys.Item[j]);
      if not Key.IsSubkey and (UpperCase(Key.KeyID) = UpperCase(Signature.SignerKeyID)) then
      begin
        if Key.Username <> '' then
          userID := Key.Username
        else
          userID := 'No name';

        break;
      end;
    end;
    item.Caption := userID;

    case Signature.Validity of
      svtCorrupted:
        sigVal := 'Corrupted';
      svtSignerNotFound:
        sigVal := 'Signing key not found, unable to verify';
      svtReferenceCorrupted:
        sigVal := 'Reference Corrupted';
      svtFailure:
        sigVal := 'Failure';
      svtValid:
        sigVal := 'Valid';
    else
      sigVal := 'Unknown reason';
    end;

    item.SubItems.Add(sigVal);
  end;
end;

end.
