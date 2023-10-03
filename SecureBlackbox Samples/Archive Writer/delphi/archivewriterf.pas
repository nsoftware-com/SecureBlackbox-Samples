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
unit archivewriterf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, FileCtrl,
  Dialogs, Menus, ComCtrls, ImgList, ImageList, ToolWin, StdCtrls, ExtCtrls,
  SBxTypes, SBxCore, SBxArchiveWriter;

type
  TFormArchiveWriter = class(TForm)
    lvFiles: TListView;
    imgListFileIcons: TImageList;
    pnlPath: TPanel;
    Image1: TImage;
    lblPath: TLabel;
    sdChooseDirectory: TSaveDialog;
    sbStatusBar: TStatusBar;
    pmFileListPopup: TPopupMenu;
    miflDelete: TMenuItem;
    miflAddFiles: TMenuItem;
    miflAddDirectory: TMenuItem;
    mainMenu: TMainMenu;
    mniFile: TMenuItem;
    mniOpenArchive: TMenuItem;
    mniNewArchive: TMenuItem;
    mniCloseArchive: TMenuItem;
    mniSaveArchive: TMenuItem;
    mniSaveArchiveAs: TMenuItem;
    mniExit: TMenuItem;
    sdArchiveFile: TSaveDialog;
    odAddFiles: TOpenDialog;
    Panel1: TPanel;
    Label10: TLabel;
    procedure mniOpenArchiveClick(Sender: TObject);
    procedure lvFilesCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure lvFilesDblClick(Sender: TObject);
    procedure mniCloseArchiveClick(Sender: TObject);
    procedure mniExitClick(Sender: TObject);
    procedure pmFileListPopupPopup(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mniNewArchiveClick(Sender: TObject);
    procedure mniSaveArchiveClick(Sender: TObject);
    procedure mniSaveArchiveAsClick(Sender: TObject);
    procedure miflAddFilesClick(Sender: TObject);
    procedure miflAddDirectoryClick(Sender: TObject);
    procedure miflDeleteClick(Sender: TObject);
  private
    { Private declarations }
    FArchive: TSBxArchiveWriter;
    CurrentPath : string;

    procedure RefreshFilesView;

    procedure DoBeforeCompressFile(Sender: TObject;
      const Path: String; FileSize: Int64; Source: Integer);
    procedure DoAfterCompressFile(Sender: TObject;
      const Path: String; FileSize: Int64; Source: Integer);
    procedure DoProgress(Sender: TObject; Processed: Int64; Total: Int64;
      OverallProcessed: Int64; OverallTotal: Int64; var Cancel: Boolean);
    procedure DoRecipientFound(Sender: TObject; RecipientHash: TBytes; CertFound: Boolean);
    procedure DoDecryptionPasswordNeeded(Sender: TObject; const PasswordTarget: string; var Cancel: Boolean);
  public
    { Public declarations }
  end;

var
  FormArchiveWriter: TFormArchiveWriter;

implementation

{$R *.dfm}

uses openarchivef, progressf, newarchivef, certificatef, passwordf;

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

procedure TFormArchiveWriter.RefreshFilesView;
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

      case Entry.Action of
        atAdd: Item.SubItems.Add('Add');
        atKeep: Item.SubItems.Add('Keep');
        atUpdate: Item.SubItems.Add('Update');
        atDelete: Item.SubItems.Add('Delete');
      end;
    end;
  end;

  lvFiles.Items.EndUpdate;
end;

procedure TFormArchiveWriter.mniOpenArchiveClick(Sender: TObject);
begin
  if FArchive.Opened then
    FArchive.Close;

  FormOpenarchive.edtArchiveFile.Text := '';
  FormOpenarchive.cbArchiveType.ItemIndex := 0;
  if FormOpenarchive.ShowModal = mrOk then
  begin
    try
      case FormOpenarchive.cbArchiveType.ItemIndex of
        1: FArchive.Open(Integer(aftTarGzip), FormOpenarchive.edtArchiveFile.Text);
        2: FArchive.Open(Integer(aftTarBzip2), FormOpenarchive.edtArchiveFile.Text);
        3: FArchive.Open(Integer(aftGzip), FormOpenarchive.edtArchiveFile.Text);
        4: FArchive.Open(Integer(aftBzip2), FormOpenarchive.edtArchiveFile.Text);
        else FArchive.Open(Integer(aftZip), FormOpenarchive.edtArchiveFile.Text);
      end;
    except
      on E : Exception do
        Application.MessageBox(PChar(E.Message), 'Error opening archive:', MB_OK);
    end;

    if FArchive.Opened then
    begin
      mniCloseArchive.Enabled := true;
      mniSaveArchive.Enabled := true;
      mniSaveArchiveAs.Enabled := true;
      CurrentPath := '';
      RefreshFilesView;
    end;
  end;
end;

procedure TFormArchiveWriter.mniNewArchiveClick(Sender: TObject);
begin
  if FArchive.Opened then
  begin
    FArchive.Close;
    lvFiles.Items.Clear;
  end;

  if FormNewarchive.ShowModal = IDOK then
  begin
    case FormNewarchive.cbCompressionLevel.ItemIndex of
      0 : FArchive.CompressionLevel := 1;
      1 : FArchive.CompressionLevel := 3;
      2 : FArchive.CompressionLevel := 6;
    else
      FArchive.CompressionLevel := 9;
    end;

    case FormNewarchive.cbEncryptionType.ItemIndex of
      1 :
      begin
        FArchive.EncryptionType := aetGeneric;
        FArchive.EncryptionPassword := FormNewarchive.edtPassword.Text;
      end;
      2 :
      begin
        FArchive.EncryptionType := aetWinZip;

        case FormNewarchive.cbEncryptionAlgorithm.ItemIndex of
          0 : FArchive.Config('EncryptionKeyLength=128');
          1 : FArchive.Config('EncryptionKeyLength=192');
          2 : FArchive.Config('EncryptionKeyLength=256')
        else
          FArchive.Config('EncryptionKeyLength=128');
        end;

        FArchive.EncryptionPassword := FormNewarchive.edtPassword.Text;
      end;
      3 :
      begin
        FArchive.EncryptionType := aetStrong;
        FArchive.EncryptionAlgorithm := FormNewarchive.cbEncryptionAlgorithm.Text;
        FArchive.EncryptionPassword := FormNewarchive.edtPassword.Text;
      end;
    else
      FArchive.EncryptionType := aetNoEncryption;
    end;

    case FormNewarchive.cbArchiveType.ItemIndex of
      1: FArchive.CreateNew(Integer(aftTarGzip));
      2: FArchive.CreateNew(Integer(aftTarBzip2));
      3: FArchive.CreateNew(Integer(aftGzip));
      4: FArchive.CreateNew(Integer(aftBzip2));
      else FArchive.CreateNew(Integer(aftZip));
    end;

    mniCloseArchive.Enabled := true;
    mniSaveArchive.Enabled := true;
    mniSaveArchiveAs.Enabled := true;
  end;

  CurrentPath := '';
  RefreshFilesView;
end;

procedure TFormArchiveWriter.mniSaveArchiveAsClick(Sender: TObject);
begin
  if sdArchiveFile.Execute then
  begin
    FormProgress.Caption := 'Compressing...';
    FormProgress.Show;
    FormProgress.btnOk.Enabled := false;
    FormProgress.btnCancel.Enabled := true;
    FormProgress.CancelOperation := false;

    try
      FArchive.Save(sdArchiveFile.FileName);
    except
      on E : Exception do
        FormProgress.AddToLog('Erorr : ' + E.Message);
    end;

    FormProgress.btnCancel.Enabled := false;
    FormProgress.btnOk.Enabled := true;

    RefreshFilesView;
  end;
end;

procedure TFormArchiveWriter.mniSaveArchiveClick(Sender: TObject);
begin
  FormProgress.Caption := 'Compressing...';
  FormProgress.Show;
  FormProgress.btnOk.Enabled := false;
  FormProgress.btnCancel.Enabled := true;
  FormProgress.CancelOperation := false;

  try
    if FArchive.NewArchive then
      FArchive.Save(FormNewarchive.edtArchiveFile.Text)
    else
      FArchive.Save(FormOpenarchive.edtArchiveFile.Text);
  except
    on E : Exception do
      FormProgress.AddToLog('Erorr : ' + E.Message);
  end;

  FormProgress.btnCancel.Enabled := false;
  FormProgress.btnOk.Enabled := true;

  RefreshFilesView;
end;

procedure TFormArchiveWriter.lvFilesCompare(Sender: TObject; Item1,
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

procedure TFormArchiveWriter.lvFilesDblClick(Sender: TObject);
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
        CurrentPath := Entry.Path;
    end;

    RefreshFilesView;
  end;
end;

procedure TFormArchiveWriter.miflDeleteClick(Sender: TObject);
var
  i, j, k : integer;
  Entry: TSBxArchivedFile;
begin
  i := 0;

  while (i < lvFiles.Items.Count) do
    if (lvFiles.Items[i].Selected) then
    begin
      if (lvFiles.Items[i].Data = nil) then
      begin
        lvFiles.Items[i].Selected := false;
        Inc(i);
      end
      else
      begin
        Entry := TSBxArchivedFile(lvFiles.Items[i].Data);

        if Entry.NewFile then
        begin
          FArchive.Files.Remove(lvFiles.Items[i].Data);
          lvFiles.Items.Delete(i);
        end
        else
        begin
          Entry.Action := atDelete;
          lvFiles.Items[i].SubItems[4] := 'Delete';
          Inc(i);
        end;
      end;
    end
    else
      Inc(i);
end;

procedure TFormArchiveWriter.miflAddDirectoryClick(Sender: TObject);
var
  Dir : string;
begin
  Dir := '';
  if SelectDirectory(Dir, [], 0) then
  begin
    FArchive.AddFiles(CurrentPath, Dir, true);
    RefreshFilesView;
  end;
end;

procedure TFormArchiveWriter.miflAddFilesClick(Sender: TObject);
var
  i: integer;
begin
  if odAddFiles.Execute then
  begin
    for i := 0 to odAddFiles.Files.Count - 1 do
      FArchive.AddFile(CurrentPath + '/' + ExtractFileName(odAddFiles.Files[i]), odAddFiles.Files[i]);

    RefreshFilesView;
  end;
end;

procedure TFormArchiveWriter.mniCloseArchiveClick(Sender: TObject);
begin
  if FArchive.Opened then
  begin
    FArchive.Close;
    lvFiles.Items.Clear;

    mniCloseArchive.Enabled := false;
    mniSaveArchive.Enabled := false;
    mniSaveArchiveAs.Enabled := false;
  end;
end;

procedure TFormArchiveWriter.mniExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormArchiveWriter.pmFileListPopupPopup(Sender: TObject);
begin
  miflDelete.Enabled := (lvFiles.SelCount > 0) and (FArchive.NewArchive or FArchive.Opened);
  miflAddFiles.Enabled := FArchive.NewArchive or FArchive.Opened;
  miflAddDirectory.Enabled := FArchive.NewArchive or FArchive.Opened;
end;

procedure TFormArchiveWriter.FormCreate(Sender: TObject);
begin
  FArchive := TSBxArchiveWriter.Create(nil);
  FArchive.OnBeforeCompressFile := DoBeforeCompressFile;
  FArchive.OnAfterCompressFile := DoAfterCompressFile;
  FArchive.OnProgress := DoProgress;
  FArchive.OnRecipientFound := DoRecipientFound;
  FArchive.OnDecryptionPasswordNeeded := DoDecryptionPasswordNeeded;
end;

procedure TFormArchiveWriter.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FArchive);
end;

procedure TFormArchiveWriter.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Resize := (NewHeight >= 500) and (NewWidth >= 750);
end;

procedure TFormArchiveWriter.DoBeforeCompressFile(Sender: TObject;
  const Path: String; FileSize: Int64; Source: Integer);
begin
  FormProgress.pbCurrentFileProgress.Max := 100000;
  FormProgress.pbTotalProgress.Max := 100000;
  FormProgress.lblCurrentFileName.Caption := Path;
  Application.ProcessMessages;
end;

procedure TFormArchiveWriter.DoAfterCompressFile(Sender: TObject;
  const Path: String; FileSize: Int64; Source: Integer);
begin
  FormProgress.AddToLog('File compressed : ' + Path);
  Application.ProcessMessages;
end;

procedure TFormArchiveWriter.DoProgress(Sender: TObject; Processed: Int64; Total: Int64;
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

procedure TFormArchiveWriter.DoRecipientFound(Sender: TObject;
  RecipientHash: TBytes; CertFound: Boolean);
begin
  if not CertFound and (FormCertificate.ShowModal = IDOK) then
    FArchive.DecryptionCertificates.Add(FormCertificate.Certificate);
end;

procedure TFormArchiveWriter.DoDecryptionPasswordNeeded(Sender: TObject; const PasswordTarget: string; var Cancel: Boolean);
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

end.
















