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
unit archivereaderf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, FileCtrl,
  Dialogs, Menus, ComCtrls, ImgList, ImageList, ToolWin, StdCtrls, ExtCtrls,
  SBxTypes, SBxCore, SBxArchiveReader;

type
  TFormArchiveReader = class(TForm)
    mainMenu: TMainMenu;
    mniFile: TMenuItem;
    mniOpenArchive: TMenuItem;
    mniCloseArchive: TMenuItem;
    mniExit: TMenuItem;
    lvFiles: TListView;
    imgListFileIcons: TImageList;
    pnlPath: TPanel;
    Image1: TImage;
    lblPath: TLabel;
    pmFileListPopup: TPopupMenu;
    miflExtract: TMenuItem;
    miflExtractTo: TMenuItem;
    sdChooseDirectory: TSaveDialog;
    sbStatusBar: TStatusBar;
    miflExtractAll: TMenuItem;
    miflExtractAllTo: TMenuItem;
    Panel1: TPanel;
    Label10: TLabel;
    procedure mniOpenArchiveClick(Sender: TObject);
    procedure lvFilesCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure lvFilesDblClick(Sender: TObject);
    procedure mniCloseArchiveClick(Sender: TObject);
    procedure mniExitClick(Sender: TObject);
    procedure pmFileListPopupPopup(Sender: TObject);
    procedure miflExtractClick(Sender: TObject);
    procedure miflExtractToClick(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miflExtractAllClick(Sender: TObject);
    procedure miflExtractAllToClick(Sender: TObject);
  private
    { Private declarations }
    FArchive: TSBxArchiveReader;
    FArchiveName : string;
    CurrentPath : string;

    procedure RefreshFilesView;
    procedure DoExtractSelectedFile(const Destination: string);
    procedure DoExtractAllFiles(const Destination: string);

    procedure DoBeforeExtractFile(Sender: TObject;
      const FileName: String; FileSize: Int64; var Destination: Integer;
      var ExtractionPath: String; var Skip: Boolean);
    procedure DoAfterExtractFile(Sender: TObject;
      const FileName: String; FileSize: Int64;  Destination: Integer);
    procedure DoProgress(Sender: TObject; Processed: Int64; Total: Int64;
      OverallProcessed: Int64; OverallTotal: Int64; var Cancel: Boolean);
    procedure DoRecipientFound(Sender: TObject; RecipientHash: TBytes; CertFound: Boolean);
    procedure DoDecryptionPasswordNeeded(Sender: TObject; const PasswordTarget: string; var Cancel: Boolean);
    procedure DoSignatureFound(Sender: TObject; const IssuerRDN: String;
      SerialNumber: TBytes; SubjectKeyID: TBytes; CertFound: Boolean;
      var ValidateSignature: Boolean; var ValidateChain: Boolean);
  public
    { Public declarations }
  end;

var
  FormArchiveReader: TFormArchiveReader;

implementation

{$R *.dfm}

uses openarchivef, progressf, certificatef, passwordf, usignf;

//{$R 'SecureBlackbox.ZIPSFX.Console.res'}

function PathCutLastComponent(const Path : string) : string;
var
  i : integer;
begin
  Result := '';

  if Length(Path) = 0 then
    Exit;

  i := Length(Path);

  if (Path[i] = '/') or (Path[i] = '\') then
    Dec(i);

  while (i >= 1) and (Path[i] <> '/') and (Path[i] <> '\') do
    Dec(i);

  if i >= 1 then
    Result := Copy(Path, 1, i - 1);
end;

procedure TFormArchiveReader.RefreshFilesView;
var
  Entry: TSBxArchivedFile;
  Item: TListItem;
  i: integer;
  St: string;
begin
  if not FArchive.Opened then
    Exit;

  lblPath.Caption := '\' + CurrentPath;

  lvFiles.Items.BeginUpdate;
  lvFiles.Items.Clear;

  if Length(CurrentPath) > 0 then
  begin
    Item := lvFiles.Items.Add;
    Item.Caption := '..';
    Item.Data := nil;
    Item.ImageIndex := 1;
  end;

  for i := 0 to FArchive.Files.Count - 1 do
  begin
    Entry := FArchive.Files[i];

    if CurrentPath = Entry.Folder then
    begin
      Item := lvFiles.Items.Add;
      Item.Caption := Entry.FileName;
      Item.Data := Entry;

      if Entry.Directory then
      begin
        Item.ImageIndex := 1;
        Item.SubItems.Add('');
        Item.SubItems.Add('');
        Item.SubItems.Add('');
        Item.SubItems.Add('');
      end
      else
      begin
        Item.ImageIndex := 0;
        Item.SubItems.Add(IntToStr(Entry.CompressedSize));
        Item.SubItems.Add(IntToStr(Entry.Size));
        Item.SubItems.Add(Entry.MTime);

        // Security
        St := '';
        case Entry.EncryptionType of
          aetGeneric:
          begin
            St := 'Encrypted (generic)';
          end;

          aetWinZip:
          begin
            St := 'Encrypted (WinZip ' + IntToStr(Entry.EncryptionKeyLength) + ')';
          end;

          aetStrong:
          begin
            St := 'Encrypted (Strong-' + Entry.EncryptionAlgorithm + ',' + IntToStr(Entry.EncryptionKeyLength) + ' bits)';
          end;
        end;

        if Entry.Signed then
          St := St + ', signed';

        Item.SubItems.Add(St);
      end;
    end;
  end;

  lvFiles.Items.EndUpdate;
end;

procedure TFormArchiveReader.DoExtractSelectedFile(const Destination: string);
var
  Entry: TSBxArchivedFile;
begin
  if lvFiles.Selected.Data = nil then
    exit;

  Entry := TSBxArchivedFile(lvFiles.Selected.Data);

  FormProgress.btnOk.Enabled := false;
  FormProgress.btnCancel.Enabled := true;
  FormProgress.CancelOperation := false;
  FormProgress.lvLog.Items.Clear;
  FormProgress.Caption := 'Extracting...';
  FormProgress.Show;

  try
    FArchive.Extract(Entry.Path, Destination, false);
  except
    on E : Exception do
      FormProgress.AddToLog('Error : ' + E.Message);
  end;

  FormProgress.btnCancel.Enabled := false;
  FormProgress.btnOk.Enabled := true;
  sbStatusBar.Panels[1].Text := 'Extraction finished.';
end;

procedure TFormArchiveReader.DoExtractAllFiles(const Destination: string);
begin
  FormProgress.btnOk.Enabled := false;
  FormProgress.btnCancel.Enabled := true;
  FormProgress.CancelOperation := false;
  FormProgress.lvLog.Items.Clear;
  FormProgress.Caption := 'Extracting...';
  FormProgress.Show;

  try
    FArchive.ExtractAll(Destination, true);
  except
    on E : Exception do
      FormProgress.AddToLog('Error : ' + E.Message);
  end;

  FormProgress.btnCancel.Enabled := false;
  FormProgress.btnOk.Enabled := true;
  sbStatusBar.Panels[1].Text := 'Extraction finished.';
end;

procedure TFormArchiveReader.mniOpenArchiveClick(Sender: TObject);
begin
  if FArchive.Opened then
    FArchive.Close;

  FormOpenarchive.edtArchiveFile.Text := '';
  FormOpenarchive.cbArchiveType.ItemIndex := 0;
  if FormOpenarchive.ShowModal = mrOk then
  begin
    FArchiveName := FormOpenarchive.edtArchiveFile.Text;

    try
      case FormOpenarchive.cbArchiveType.ItemIndex of
        1: FArchive.Open(Integer(aftTarGzip), FArchiveName);
        2: FArchive.Open(Integer(aftTarBzip2), FArchiveName);
        3: FArchive.Open(Integer(aftGzip), FArchiveName);
        4: FArchive.Open(Integer(aftBzip2), FArchiveName);
        else FArchive.Open(Integer(aftZip), FArchiveName);
      end;
    except
      on E : Exception do
        Application.MessageBox(PChar(E.Message), 'Error opening archive:', MB_OK);
    end;

    if FArchive.Opened then
    begin
      mniCloseArchive.Enabled := true;
      CurrentPath := '';
      RefreshFilesView;
    end;
  end;
end;

procedure TFormArchiveReader.lvFilesCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  Entry1, Entry2: TSBxArchivedFile;
begin
  if Item1.Data = nil then
  begin
    Compare := -1;
    Exit;
  end
  else if Item2.Data = nil then
  begin
    Compare := 1;
    Exit;
  end;

  Entry1 := TSBxArchivedFile(Item1.Data);
  Entry2 := TSBxArchivedFile(Item2.Data);

  if (Entry1.Directory and (not Entry2.Directory)) then
    Compare := -1
  else if ((not Entry1.Directory) and Entry2.Directory) then
    Compare := 1
  else
    Compare := CompareStr(Entry1.FileName, Entry2.FileName);
end;

procedure TFormArchiveReader.lvFilesDblClick(Sender: TObject);
var
  Entry: TSBxArchivedFile;
begin
  if Assigned(lvFiles.Selected) then
  begin
    if lvFiles.Selected.Data = nil then
      CurrentPath := PathCutLastComponent(CurrentPath)
    else
    begin
      Entry := TSBxArchivedFile(lvFiles.Selected.Data);

      if Entry.Directory then
        CurrentPath := Entry.Path
      else
        exit;
    end;

    RefreshFilesView;
  end;
end;

procedure TFormArchiveReader.mniCloseArchiveClick(Sender: TObject);
begin
  if FArchive.Opened then
  begin
    FArchive.Close;
    lvFiles.Items.Clear;
  end;  
end;

procedure TFormArchiveReader.mniExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormArchiveReader.pmFileListPopupPopup(Sender: TObject);
begin
  miflExtract.Enabled := (lvFiles.SelCount > 0) and FArchive.Opened;
  miflExtractTo.Enabled := (lvFiles.SelCount > 0) and FArchive.Opened;
  miflExtractAll.Enabled := FArchive.Opened;
  miflExtractAllTo.Enabled := FArchive.Opened;
end;

procedure TFormArchiveReader.miflExtractClick(Sender: TObject);
begin
  DoExtractSelectedFile(PathCutLastComponent(FArchiveName));
end;

procedure TFormArchiveReader.miflExtractToClick(Sender: TObject);
var
  Dir : string;
begin
  Dir := PathCutLastComponent(FArchiveName);
  if SelectDirectory(Dir, [], 0) then
    DoExtractSelectedFile(Dir);
end;

procedure TFormArchiveReader.FormCreate(Sender: TObject);
begin
  FArchive := TSBxArchiveReader.Create(nil);

  FArchive.OnBeforeExtractFile := DoBeforeExtractFile;
  FArchive.OnAfterExtractFile := DoAfterExtractFile;
  FArchive.OnProgress := DoProgress;
  FArchive.OnRecipientFound := DoRecipientFound;
  FArchive.OnDecryptionPasswordNeeded := DoDecryptionPasswordNeeded;
  FArchive.OnSignatureFound := DoSignatureFound;

  FArchiveName := '';
end;

procedure TFormArchiveReader.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FArchive);
end;

procedure TFormArchiveReader.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Resize := (NewHeight >= 500) and (NewWidth >= 750);
end;

procedure TFormArchiveReader.DoBeforeExtractFile(Sender: TObject;
  const FileName: String; FileSize: Int64; var Destination: Integer;
  var ExtractionPath: String; var Skip: Boolean);
begin
  FormProgress.pbCurrentFileProgress.Max := 100000;
  FormProgress.pbTotalProgress.Max := 100000;
  FormProgress.lblCurrentFileName.Caption := FileName;
  Application.ProcessMessages;
end;

procedure TFormArchiveReader.DoAfterExtractFile(Sender: TObject;
  const FileName: String; FileSize: Int64;  Destination: Integer);
begin
  FormProgress.AddToLog('File extracted : ' + FileName);
  Application.ProcessMessages;
end;

procedure TFormArchiveReader.DoProgress(Sender: TObject; Processed: Int64; Total: Int64;
  OverallProcessed: Int64; OverallTotal: Int64; var Cancel: Boolean);
var
  ProcessedNormalized : Int64;
begin
  if Total > 0 then
    ProcessedNormalized := (Processed * 100000) div Total
  else
    ProcessedNormalized := 100000;
  FormProgress.pbCurrentFileProgress.Position := ProcessedNormalized;
  if OverallTotal > 0 then
    ProcessedNormalized := (OverallProcessed * 100000 div OverallTotal)
  else
    ProcessedNormalized := 100000;

  FormProgress.pbTotalProgress.Position := ProcessedNormalized;
  Application.ProcessMessages;
  Cancel := FormProgress.CancelOperation;
end;

procedure TFormArchiveReader.DoRecipientFound(Sender: TObject;
  RecipientHash: TBytes; CertFound: Boolean);
begin
  if not CertFound and (FormCertificate.ShowModal = IDOK) then
    FArchive.DecryptionCertificates.Add(FormCertificate.Certificate);
end;

procedure TFormArchiveReader.DoDecryptionPasswordNeeded(Sender: TObject; const PasswordTarget: string; var Cancel: Boolean);
begin
  if FormPassword.ShowModal = IDOK then
  begin
    FArchive.DecryptionPassword := FormPassword.edtPassword.Text;
    Cancel := false;
  end
  else
  begin
    FArchive.DecryptionPassword := '';
    Cancel := true;
  end;
  FormPassword.edtPassword.Text := '';
end;

procedure TFormArchiveReader.DoSignatureFound(Sender: TObject; const IssuerRDN: String;
  SerialNumber: TBytes; SubjectKeyID: TBytes; CertFound: Boolean;
  var ValidateSignature: Boolean; var ValidateChain: Boolean);
var
  frmSign: TFormUsign;
begin
  if not CertFound then
  begin
    frmSign := TFormUsign.Create(nil);
    try
      frmSign.Init(FArchive, IssuerRDN, SerialNumber);

      frmSign.ShowModal;
    finally
      FreeAndNil(frmSign);
    end;
  end;
end;

procedure TFormArchiveReader.miflExtractAllClick(Sender: TObject);
begin
  DoExtractAllFiles(PathCutLastComponent(FArchiveName));
end;

procedure TFormArchiveReader.miflExtractAllToClick(Sender: TObject);
var
  Dir : string;
begin
  Dir := PathCutLastComponent(FArchiveName);
  if SelectDirectory(Dir, [], 0) then
    DoExtractAllFiles(Dir);
end;

end.















