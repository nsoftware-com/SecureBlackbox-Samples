unit officeencpropsf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  SBxTypes, SBxCore, SBxOfficeDecryptor;

type
  TFormOfficeencprops = class(TForm)
    gbEncryptionProps: TGroupBox;
    editPassword: TEdit;
    lProvidePassword: TLabel;
    lEncryptionInfo: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    lInfo: TLabel;
    lDetails: TLabel;
  public
    FCount : Integer;

    procedure SetOfficeEncryptionProps(OfficeDecryptor : TsbxOfficeDecryptor);
    procedure GetOfficeDecryptionInfo(OfficeDecryptor : TsbxOfficeDecryptor);
  end;

var
  FormOfficeencprops: TFormOfficeencprops;

implementation

{$R *.dfm}

{ TFormOfficeencprops }

procedure TFormOfficeencprops.SetOfficeEncryptionProps(OfficeDecryptor : TsbxOfficeDecryptor);
var
  s : string;
begin
  if FCount > 0 then
    MessageDlg('Invalid password', mtWarning, [mbOk], 0);

  s := 'Document format: ';
  case OfficeDecryptor.DocumentFormat of
    odfBinary : s := s + 'Binary';
    odfOpenXML : s := s + 'OpenXML';
    odfOpenXPS : s := s + 'OpenXPS';
    odfOpenDocument : s := s + 'OpenDocument';
  else
    s := s + 'Unknown';
  end;

  s := s + #13#10 + 'Encryption type: ';
  case OfficeDecryptor.EncryptionType of
    oetBinaryRC4 : s := s + 'Binary RC4';
    oetBinaryRC4CryptoAPI : s := s + 'Binary RC4 CryptoAPI';
    oetOpenXMLStandard : s := s + 'OpenXML Standard';
    oetOpenXMLAgile : s := s + 'OpenXML Agile';
    oetOpenDocument : s := s + 'OpenOffice Standard';
  else
    s := s + 'Unknown';
  end;

  s := s + #13#10 + 'Encryption algorithm: ' + OfficeDecryptor.EncryptionAlgorithm + #13#10;

  if OfficeDecryptor.EncryptionType = oetBinaryRC4CryptoAPI then
  begin
    s := s + #13#10 + 'Key length in bits: ' + OfficeDecryptor.Config('RC4KeyBits');
    s := s + #13#10 + 'Encrypt Document Properties: ' + OfficeDecryptor.Config('EncryptDocumentProperties');
    s := s + #13#10 + 'Use Hardened Key Generation: ' + OfficeDecryptor.Config('HardenedKeyGeneration');
  end
  else if OfficeDecryptor.EncryptionType = oetOpenXMLAgile then
  begin
    s := s + #13#10 + 'Hash Algorithm: ' + OfficeDecryptor.Config('HashAlgorithm');
    s := s + #13#10 + 'Salt Size: ' + OfficeDecryptor.Config('SaltSize');
    s := s + #13#10 + 'Spin Count: ' + OfficeDecryptor.Config('SpinCount');
  end
  else if OfficeDecryptor.EncryptionType = oetOpenDocument then
  begin
    s := s + #13#10 + 'Checksum Algorithm: ' + OfficeDecryptor.Config('ChecksumAlgorithm');
    s := s + #13#10 + 'Start Key Generation Algorithm: ' + OfficeDecryptor.Config('StartKeyGenerationAlgorithm');
  end;

  lDetails.Caption := s;
end;

procedure TFormOfficeencprops.GetOfficeDecryptionInfo(OfficeDecryptor : TsbxOfficeDecryptor);
begin
  OfficeDecryptor.Password := editPassword.Text;

  Inc(FCount);
end;

end.
