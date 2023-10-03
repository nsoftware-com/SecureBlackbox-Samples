unit newarchivef;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormNewarchive = class(TForm)
    lblCompressionLevel: TLabel;
    lblArchiveName: TLabel;
    edtArchiveFile: TEdit;
    btnChoose: TButton;
    gbCompressionOptions: TGroupBox;
    cbCompressionLevel: TComboBox;
    sdSaveArchive: TSaveDialog;
    btnCancel: TButton;
    btnOk: TButton;
    cbArchiveType: TComboBox;
    lblArchiveType: TLabel;
    gbSecurityOptions: TGroupBox;
    lblPassword: TLabel;
    lblPasswordConfirmation: TLabel;
    lblEncryption: TLabel;
    lblEncryptionAlgorithm: TLabel;
    cbEncryptionAlgorithm: TComboBox;
    edtPassword: TEdit;
    edtPasswordConfirmation: TEdit;
    cbShowPassword: TCheckBox;
    cbEncryptionType: TComboBox;
    procedure btnChooseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbArchiveTypeChange(Sender: TObject);
    procedure cbEncryptionTypeChange(Sender: TObject);
    procedure cbShowPasswordClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
    procedure RefreshControlsVisibility;
  public
    { Public declarations }
  end;

var
  FormNewarchive: TFormNewarchive;

implementation

{$R *.dfm}

procedure TFormNewarchive.btnChooseClick(Sender: TObject);
begin
  sdSaveArchive.FileName := edtArchiveFile.Text;
  if sdSaveArchive.Execute then
    edtArchiveFile.Text := sdSaveArchive.FileName;
end;

procedure TFormNewarchive.btnOkClick(Sender: TObject);
begin
  if (cbEncryptionType.ItemIndex > 0) and (not cbShowPassword.Checked) and
    (edtPassword.Text <> edtPasswordConfirmation.Text) then
  begin
    Application.MessageBox('Password and it''s confirmation don''t match.', 'Warning!', MB_OK);
    ActiveControl := edtPasswordConfirmation;
  end
  else
    ModalResult := IDOK;
end;

procedure TFormNewarchive.cbArchiveTypeChange(Sender: TObject);
begin
  RefreshControlsVisibility;
end;

procedure TFormNewarchive.cbEncryptionTypeChange(Sender: TObject);
begin
  RefreshControlsVisibility;
end;

procedure TFormNewarchive.cbShowPasswordClick(Sender: TObject);
begin
  RefreshControlsVisibility;
end;

procedure TFormNewarchive.FormShow(Sender: TObject);
begin
  edtArchiveFile.Text := 'archive.zip';
  cbArchiveType.ItemIndex := 0;
  cbEncryptionType.ItemIndex := 0;

  RefreshControlsVisibility;
end;

procedure TFormNewarchive.RefreshControlsVisibility;
var
  i : integer;
begin
  if cbArchiveType.ItemIndex = 0 then
    gbSecurityOptions.Enabled := true
  else
  begin
    gbSecurityOptions.Enabled := false;
    cbEncryptionType.ItemIndex := 0;
  end;

  if cbEncryptionType.ItemIndex = 0 then
  begin
    { no encryption }
    lblEncryptionAlgorithm.Visible := false;
    cbEncryptionAlgorithm.Visible := false;
    lblPassword.Visible := false;
    edtPassword.Visible := false;
    lblPasswordConfirmation.Visible := false;
    edtPasswordConfirmation.Visible := false;
    cbShowPassword.Visible := false;
  end
  else
  begin
    lblPassword.Visible := true;
    edtPassword.Visible := true;
    if cbShowPassword.Checked then
      edtPassword.PasswordChar := #0
    else
      edtPassword.PasswordChar := '*';
    lblPasswordConfirmation.Visible := not cbShowPassword.Checked;
    edtPasswordConfirmation.Visible := not cbShowPassword.Checked;
    cbShowPassword.Visible := true;

    if cbEncryptionType.ItemIndex = 1 then
    begin
      { Generic encryption }
      lblEncryptionAlgorithm.Visible := false;
      cbEncryptionAlgorithm.Visible := false;
    end
    else if cbEncryptionType.ItemIndex = 2 then
    begin
      { WinZip encryption }
      lblEncryptionAlgorithm.Visible := true;
      cbEncryptionAlgorithm.Visible := true;
      cbEncryptionAlgorithm.Items.CommaText := 'AES128,AES192,AES256';
      cbEncryptionAlgorithm.ItemIndex := 0;
    end
    else if cbEncryptionType.ItemIndex = 3 then
    begin
      { Strong encryption }
      lblEncryptionAlgorithm.Visible := true;
      cbEncryptionAlgorithm.Visible := true;
      cbEncryptionAlgorithm.Items.CommaText := 'RC2,RC4,3DES168,AES128,AES192,AES256,Blowfish,Twofish';
      cbEncryptionAlgorithm.ItemIndex := 3;
    end;
  end;
end;

end.
