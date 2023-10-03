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
unit ftpclientf;

{$ifdef UNICODE}
  {$define SB_UNICODE_VCL}
{$endif}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ToolWin, ComCtrls, ExtCtrls, StdCtrls, ImgList,
  SBxTypes, SBxCore, SBxFTPClient, System.ImageList;

type
  TFormFtpclient = class(TForm)
    tbToolbar: TToolBar;
    lvLog: TListView;
    lvFiles: TListView;
    tbConnect: TToolButton;
    tbDisconnect: TToolButton;
    tbDelim1: TToolButton;
    tbMakeDir: TToolButton;
    tbDelete: TToolButton;
    tbDelim2: TToolButton;
    tbDownload: TToolButton;
    tbUpload: TToolButton;
    tbDelim3: TToolButton;
    tbRefresh: TToolButton;
    spLog: TSplitter;
    pPath: TPanel;
    lPath: TLabel;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    imgListViews: TImageList;
    Image1: TImage;
    Panel1: TPanel;
    Label10: TLabel;
    procedure tbConnectClick(Sender: TObject);
    procedure tbDisconnectClick(Sender: TObject);
    procedure tbMakeDirClick(Sender: TObject);
    procedure tbDeleteClick(Sender: TObject);
    procedure tbDownloadClick(Sender: TObject);
    procedure tbUploadClick(Sender: TObject);
    procedure tbRefreshClick(Sender: TObject);
    procedure lvFilesDblClick(Sender: TObject);
    procedure mnuConnectClick(Sender: TObject);
    procedure mnuDisconnectClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FtpClientError(Sender: TObject; ErrorCode: Integer; const Description: string);
    procedure FtpClientProgress(Sender: TObject; Total, Current: Int64;
      var Cancel: Boolean);
    procedure FtpClientFileOperation(Sender: TObject; Operation: integer;
      const RemotePath, LocalPath: string; var Skip, Cancel: Boolean);
    procedure FtpClientFileOperationResult(Sender: TObject; Operation: integer;
      const RemotePath, LocalPath: string; ErrorCode: integer; const Comment: string; var Cancel: Boolean);
    procedure FtpClientListEntry(Sender: TObject; const Filename: string);
    procedure FtpClientControlReceive(Sender: TObject; const TextLine: string);
    procedure FtpClientControlSend(Sender: TObject; const TextLine: string);
    procedure FtpClientCertificateValidate(Sender: TObject; const ServerHost : string; const ServerIP : string; var Accept: Boolean);
  private
    FClient: TsbxFTPClient;
    FDirList: TList;
    procedure Connect;
    procedure Disconnect;
    procedure MakeDir;
    procedure Delete;
    procedure Download;
    procedure Upload;
    procedure ChangeDir;
    procedure Refresh;
    procedure Log(const S : string; Error : boolean = false);
    procedure ClearFileList;
  public
    { Public declarations }
  end;

var
  FormFtpclient: TFormFtpclient;

implementation

uses ConnPropsF, ProgressF, PromptF;

{$R *.DFM}

procedure TFormFtpclient.tbConnectClick(Sender: TObject);
begin
  Connect;
end;

procedure TFormFtpclient.tbDisconnectClick(Sender: TObject);
begin
  Disconnect;
end;

procedure TFormFtpclient.tbMakeDirClick(Sender: TObject);
begin
  MakeDir;
end;

procedure TFormFtpclient.tbDeleteClick(Sender: TObject);
begin
  Delete;
end;

procedure TFormFtpclient.tbDownloadClick(Sender: TObject);
begin
  Download;
end;

procedure TFormFtpclient.tbUploadClick(Sender: TObject);
begin
  Upload;
end;

procedure TFormFtpclient.tbRefreshClick(Sender: TObject);
begin
  Refresh;
end;

procedure TFormFtpclient.Connect;
var
  Err: integer;
  Address: string;
  Port: integer;
begin
  if FClient.Connected then
  begin
    MessageDlg('Already connected', mtInformation, [mbOk], 0);
    Exit;
  end;

  if FormConnprops.ShowModal = mrOk then
  begin
    if Pos(':', FormConnprops.editHost.Text) > 0 then
    begin
      Address := Copy(FormConnprops.editHost.Text, 1, Pos(':', FormConnprops.editHost.Text) - 1);
      Port := StrToIntDef(Copy(FormConnprops.editHost.Text, Pos(':', FormConnprops.editHost.Text) + 1, Length(FormConnprops.editHost.Text)), 22);
    end
    else
    begin
      Address := FormConnprops.editHost.Text;
      Port := 21;
    end;

    FClient.Username := FormConnprops.editUsername.Text;
    FClient.Password := FormConnprops.editPassword.Text;

    if FormConnprops.rbExplicitTLS.Checked then
      FClient.TLSSettings.TLSMode := smExplicitTLS
    else
    if FormConnprops.rbImplicitTLS.Checked then
      FClient.TLSSettings.TLSMode := smImplicitTLS
    else
      FClient.TLSSettings.TLSMode := smNoTLS;

    FClient.PassiveMode := FormConnprops.cbPassive.Checked;
    try
      FClient.Connect(Address, Port);
    except
      on E: Exception do
      begin
        Log('Ftp connection failed with message [' + E.Message + ']', true);
        FClient.Disconnect;

        Exit;
      end;
    end;
    Log('Ftp connection established');
    Refresh;
  end;
end;

procedure TFormFtpclient.Disconnect;
begin
  Log('Disconnecting');
  FClient.Disconnect;
  ClearFileList;
  lPath.Caption := '';
end;

procedure TFormFtpclient.MakeDir;
var
  DirName : string;
begin
  if FClient.Connected then
  begin
    DirName := InputBox('Make directory', 'Please enter the name for new directory', '');
    if DirName = '' then Exit;
    Log('Creating directory ' + DirName);
    try
      FClient.MakeDir(DirName);
    except
      on E: Exception do
      begin
        Log('Failed to create directory "' + DirName + '", ' + E.Message, true);
      end;
    end;
    Refresh;
  end;
end;

procedure TFormFtpclient.Delete;
var FName : string;
begin
  if FClient.Connected and Assigned(lvFiles.Selected) and Assigned(lvFiles.Selected.Data) then
  begin
    if MessageDlg('Please confirm that you want to delete "' +
      TsbxFTPListEntry(lvFiles.Selected.Data).Name + '"', mtConfirmation,
      [mbYes, mbNo], 0) = mrYes then
    begin
      Log('Removing item ' + TsbxFTPListEntry(lvFiles.Selected.Data).Name);
      FName := TsbxFTPListEntry(lvFiles.Selected.Data).Name;
      try
        if TsbxFTPListEntry(lvFiles.Selected.Data).FileType = cfetDirectory then
          FClient.DeleteDir(FName)
        else
          FClient.DeleteFile(FName);
      except
        on E: Exception do
        begin
          Log('Failed to delete "' + TsbxFTPListEntry(lvFiles.Selected.Data).Name + '", ' +
            E.Message, true);
        end;
      end;
      Refresh;
    end;
  end;
end;

procedure TFormFtpclient.Download;
var
  Size : integer;
  FName : string;
begin
  if FClient.Connected and Assigned(lvFiles.Selected) and Assigned(lvFiles.Selected.Data) and
    (TsbxFTPListEntry(lvFiles.Selected.Data).FileType <> cfetDirectory) then
  begin
    SaveDialog.FileName := TsbxFTPListEntry(lvFiles.Selected.Data).Name;
    if SaveDialog.Execute then
    begin
      Log('Downloading file ' + TsbxFTPListEntry(lvFiles.Selected.Data).Name);
      FName := TsbxFTPListEntry(lvFiles.Selected.Data).Name;
      Size := TsbxFTPListEntry(lvFiles.Selected.Data).Size;
      FormProgress.lSourceFilename.Caption := FName;
      FormProgress.lDestFilename.Caption := SaveDialog.Filename;
      FormProgress.lProgress.Caption := '0 / ' + IntToStr(Size);
      FormProgress.pbProgress.Position := 0;
      FormProgress.Canceled := false;
      FormProgress.Caption := 'Download';
      FormProgress.Show;
      try
        try
          FClient.DownloadFile(FName, SaveDialog.Filename);
        finally
          FormProgress.Hide;
          Log('Download finished');
        end;
      except
        on E : Exception do
        begin
          Log('Error during download: ' + E.Message, true);
        end;
      end;
    end;
  end;
end;

procedure TFormFtpclient.Upload;
var
  Size : integer;
  FName : string;

  function GetFileSize(const Name : string): Int64;
  var
    F : TFileStream;
  begin
    try
      F := TFileStream.Create(Name, fmOpenRead or fmShareDenyNone);
      try
        Result := F.Size;
      finally
        FreeAndNil(F);
      end;
    except
      Result := 0;
    end;
  end;

begin
  if FClient.Connected then
  begin
    if OpenDialog.Execute then
    begin
      Log('Uploading file ' + OpenDialog.Filename);
      FName := ExtractFileName(OpenDialog.Filename);
      Size := GetFileSize(OpenDialog.Filename);
      FormProgress.lDestFilename.Caption := FName;
      FormProgress.lSourceFilename.Caption := OpenDialog.Filename;
      FormProgress.lProgress.Caption := '0 / ' + IntToStr(Size);
      FormProgress.pbProgress.Position := 0;
      FormProgress.Canceled := false;
      FormProgress.Caption := 'Upload';
      FormProgress.Show;
      try
        try
          FClient.UploadFile(OpenDialog.Filename, FName);
        finally
          FormProgress.Hide;
          Log('Upload finished');
        end;
      except
        on E : Exception do
        begin
          Log('Error during upload: ' + E.Message, true);
        end;
      end;
      Refresh;
    end;
  end;
end;

procedure TFormFtpclient.ChangeDir;
begin
  if Assigned(lvFiles.Selected) then
  begin
    if not Assigned(lvFiles.Selected.Data) then
    begin
      Log('Changing directory to ..');
      try
        FClient.ChangeDir('..');
        Refresh;
      except
        on E : Exception do
          Log('Unable to change directory: [' + E.Message + ']', true);
      end;
    end
    else
    if TsbxFTPListEntry(lvFiles.Selected.Data).FileType = cfetDirectory then
    begin
      Log('Changing directory to ' + TsbxFTPListEntry(lvFiles.Selected.Data).Name);
      try
        FClient.ChangeDir(TsbxFTPListEntry(lvFiles.Selected.Data).Name);
        Refresh;
      except
        on E : Exception do
          Log('Unable to change directory: [' + E.Message + ']', true);
      end;
    end;
  end;
end;

function FileListSort(Item1, Item2: Pointer): Integer;
var
  Info1, Info2 : TsbxFTPListEntry;
begin
  Info1 := Item1;
  Info2 := Item2;
  if ((Info1.FileType = cfetDirectory) and (Info2.FileType = cfetDirectory)) or
     ((Info1.FileType <> cfetDirectory) and (Info2.FileType <> cfetDirectory))
  then
    Result := CompareText(Info1.Name, Info2.Name)
  else
  begin
    if Info1.FileType = cfetDirectory then
      Result := -1
    else
      Result := 1;
  end;
end;

procedure TFormFtpclient.Refresh;
var
  I: integer;
  Item: TListItem;
begin
  ClearFileList;
  if not FClient.Connected then Exit;

  lPath.Caption := FClient.GetCurrentDir();
  Log('Retrieving file list');
  try
    FDirList.Clear;

    if FClient.GetCurrentDir() <> '/' then
    begin
      Item := lvFiles.Items.Add;
      Item.Caption := '..';
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      Item.ImageIndex := 8;
    end;

    FClient.ListDir(true, true);

    for I := 0 to FDirList.Count - 1 do
    begin
      TsbxFTPListEntry(FDirList.Items[I]).Name := TsbxFTPListEntry(FDirList.Items[I]).Name;
      TsbxFTPListEntry(FDirList.Items[I]).Path := TsbxFTPListEntry(FDirList.Items[I]).Path;
    end;

    FDirList.Sort(FileListSort);
    for I := 0 to FDirList.Count - 1 do
    begin
      Item := lvFiles.Items.Add;
      Item.Caption := TsbxFTPListEntry(FDirList.Items[I]).Name;
      Item.SubItems.Add(IntToStr(TsbxFTPListEntry(FDirList.Items[I]).Size));
      if (TsbxFTPListEntry(FDirList.Items[I]).FileType <> cfetDirectory) then
      begin
        Item.SubItems.Add(TsbxFTPListEntry(FDirList.Items[I]).FileDate);
        Item.ImageIndex := 9;
      end
      else
      begin
        Item.SubItems.Add('');
        Item.ImageIndex := 8;
      end;
      Item.Data := FDirList.Items[I];
    end;
  except
    on E : Exception do
    begin
      Log('Failed to retrieve file list');
      Exit;
    end;
  end;
end;

procedure TFormFtpclient.Log(const S : string; Error : boolean = false);
var
  Item : TListItem;
begin
  Item := lvLog.Items.Add;
  Item.Caption := TimeToStr(Now);
  Item.SubItems.Add(S);
  if Error then
    Item.ImageIndex := 11
  else
    Item.ImageIndex := 10;
end;

procedure TFormFtpclient.ClearFileList;
var
  I : integer;
begin
  try
    for I := 0 to lvFiles.Items.Count - 1 do
      TsbxFTPListEntry(lvFiles.Items[I].Data).Free;
  finally
    lvFiles.Items.Clear;
  end;
end;

procedure TFormFtpclient.lvFilesDblClick(Sender: TObject);
begin
  ChangeDir;
end;

procedure TFormFtpclient.mnuConnectClick(Sender: TObject);
begin
  Connect;
end;

procedure TFormFtpclient.mnuDisconnectClick(Sender: TObject);
begin
  Disconnect;
end;

procedure TFormFtpclient.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormFtpclient.FormCreate(Sender: TObject);
begin
  FClient := TsbxFTPClient.Create(nil);
  FClient.OnError := FtpClientError;
  FClient.OnProgress := FtpClientProgress;
  FClient.OnFileOperation := FtpClientFileOperation;
  FClient.OnFileOperationResult := FtpClientFileOperationResult;
  FClient.OnListEntry := FtpClientListEntry;
  FClient.OnControlReceive := FtpClientControlReceive;
  FClient.OnControlSend := FtpClientControlSend;
  FClient.OnTLSCertValidate := FtpClientCertificateValidate;

  FDirList := TList.Create;
end;

procedure TFormFtpclient.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FClient);
  FreeAndNil(FDirList);
end;


procedure TFormFtpclient.FtpClientError(Sender: TObject; ErrorCode: Integer; const Description: string);
begin
  Log('Error ' + IntToStr(ErrorCode) + ' (' + Description + ')', true);
end;


procedure TFormFtpclient.FtpClientProgress(Sender: TObject; Total,
  Current: Int64; var Cancel: Boolean);
begin
  FormProgress.lProgress.Caption := IntToStr(Current) + ' / ' + IntToStr(Total);
  FormProgress.pbProgress.Position := Current * 100 div Total;
  FormProgress.Refresh;
  Cancel := FormProgress.Canceled;
end;

function OperationName(Operation: integer): string;
begin
  case (operation) of
     0: Result := 'Download file';
     1: Result := 'Upload file';
     2: Result := 'Delete file';
     3: Result := 'Make directory';
     else  Result := 'Unknown';
  end;
end;

procedure TFormFtpclient.FtpClientFileOperation(Sender: TObject; Operation: integer;
  const RemotePath, LocalPath: string; var Skip, Cancel: Boolean);
var
  S: string;
begin
  S := OperationName(Operation) + '.';
  if Length(LocalPath) > 0 then
    S := S + ' Local path: ' + LocalPath;
  if Length(RemotePath) > 0 then
    S := S + ' Remote path: ' + RemotePath;
  Log(S);
end;

procedure TFormFtpclient.FtpClientFileOperationResult(Sender: TObject; Operation: integer;
  const RemotePath, LocalPath: string; ErrorCode: integer; const Comment: string; var Cancel: Boolean);
var
  S: string;
begin
  S := '';
  if ErrorCode <> 0 then
    S := S + 'Error ' + IntToStr(ErrorCode) + '.';

  S := S + 'Result of' + OperationName(Operation) + '.';

  if Length(LocalPath) > 0 then
    S := S + ' Local path: ' + LocalPath;
  if Length(RemotePath) > 0 then
    S := S + ' Remote path: ' + RemotePath;
  if Length(Comment) > 0 then
    S := S + ' Comment: ' + Comment;
  Log(S);
end;

procedure TFormFtpclient.FtpClientListEntry(Sender: TObject; const Filename: string);
var
  Item: TsbxFTPListEntry;
begin
  Item := TsbxFTPListEntry.Create;
  Item.FileDate := FClient.CurrentListEntry.FileDate;
  Item.FileType := FClient.CurrentListEntry.FileType;
  Item.Name := FClient.CurrentListEntry.Name;
  Item.Path := FClient.CurrentListEntry.Path;
  Item.Size := FClient.CurrentListEntry.Size;
  Item.EntryFormat := FClient.CurrentListEntry.EntryFormat;
  Item.RawData := FClient.CurrentListEntry.RawData;

  FDirList.Add(Item);
end;

procedure TFormFtpclient.FtpClientControlReceive(Sender: TObject; const TextLine: string);
begin
  Log('<<<' + TextLine);
end;

procedure TFormFtpclient.FtpClientControlSend(Sender: TObject; const TextLine: string);
begin
  Log('>>>' + TextLine);
end;

procedure TFormFtpclient.FtpClientCertificateValidate(Sender: TObject; const ServerHost : string; const ServerIP : string; var Accept: Boolean);
begin
  Accept := true;
end;

end.






