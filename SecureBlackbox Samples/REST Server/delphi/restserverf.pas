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
unit restserverf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls,
  SBxRestServer, SBxCertificateManager, SBxTypes, Winapi.ShellAPI;

type
  TFormRestserver = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    SpeedButton1: TSpeedButton;
    Label4: TLabel;
    edPort: TEdit;
    cbUseTLS: TCheckBox;
    edCertFile: TEdit;
    edCertPassword: TEdit;
    bbStart: TButton;
    bbStop: TButton;
    mmLog: TMemo;
    OpenFileDlg: TOpenDialog;
    lvItems: TListView;
    Label2: TLabel;
    Label10: TLabel;
    Label5: TLabel;
    procedure SpeedButton1Click(Sender: TObject);
    procedure bbStartClick(Sender: TObject);
    procedure bbStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FServer: TsbxRESTServer;
    FItems: TStringList;

    procedure ViewItems(Sender: TObject);
    function ItemsToJson: string;

    function ReadFileAsString(const FileName: string): string;

    procedure DoAccept(Sender: TObject; const RemoteAddress: String;
      RemotePort: Integer; var Accept: Boolean);
    procedure DoGet(Sender: TObject; ConnectionId: Int64;
      const URI: String; var Handled: Boolean);
    procedure DoDelete(Sender: TObject; ConnectionId: Int64;
      const URI: String; var Handled: Boolean);
    procedure DoPost(Sender: TObject; ConnectionId: Int64;
      const URI: String; var Handled: Boolean);
  public
    { Public declarations }
  end;

var
  FormRestserver: TFormRestserver;

implementation

{$R *.dfm}

function TFormRestserver.ReadFileAsString(const FileName: string): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure TFormRestserver.ViewItems(Sender: TObject);
var
  i: integer;
  Li: TListItem;
begin
  lvItems.Items.BeginUpdate;
  try
    lvItems.Items.Clear;

    for i := 0 to FItems.Count - 1 do
    begin
      Li := lvItems.Items.Add;

      Li.Caption := IntToStr(i);
      Li.SubItems.Add(FItems[i]);
    end;
  finally
    lvItems.Items.EndUpdate;
  end;
end;

function TFormRestserver.ItemsToJson: string;
var
  i: integer;
begin
  Result := '[';

  for i := 0 to FItems.Count - 1 do
  begin
    Result := Result + '{"id": ' + IntToStr(i) + ', "value": "' + FItems[i] + '"},';
  end;

  Delete(Result, Length(Result), 1);
  Result := Result + ']';
end;

procedure TFormRestserver.DoAccept(Sender: TObject; const RemoteAddress: String;
  RemotePort: Integer; var Accept: Boolean);
begin
  mmLog.Lines.Add('New connection from ' + RemoteAddress);
  Accept := true;
end;

procedure TFormRestserver.DoGet(Sender: TObject; ConnectionId: Int64;
  const URI: String; var Handled: Boolean);
var
  Content, ContentType: string;
begin
  Handled := true;

  if URI = '/items' then
  begin
    Content := ItemsToJson;
    ContentType := 'application/json';
  end
  else if (URI = '/') or (URI = '/index.html') then
  begin
    Content := ReadFileAsString('..\\..\\index.html');
    ContentType := 'text/html';
  end
  else if URI = '/client.js' then
  begin
    Content := ReadFileAsString('..\\..\\client.js');
    ContentType := 'text/javascript';
  end
  else if URI = '/main.css' then
  begin
    Content := ReadFileAsString('..\\..\\main.css');
    ContentType := 'text/css';
  end
  else
  begin
    Content := '404 Not Found';
    ContentType := 'text/plain';
    FServer.SetResponseStatus(ConnectionId, 404);
  end;

  FServer.SetResponseString(ConnectionId, Content, ContentType, '');
end;

function DeleteCompare(List: TStringList; Index1, Index2: integer) : integer;
var
  A, B : string;
  Ai, Bi: integer;
begin
  A := string(List[Index1]);
  B := string(List[Index2]);

  Ai := StrToInt(A);
  Bi := StrToInt(B);

  if Ai > Bi then
    Result := -1
  else if Ai < Bi then
    Result := 1
  else
    Result := 0;
end;

procedure TFormRestserver.DoDelete(Sender: TObject; ConnectionId: Int64;
  const URI: String; var Handled: Boolean);
var
  Req: string;
  SL, SubSL, ToDelete: TStringList;
  i: integer;
begin
  Req := FServer.GetRequestString(ConnectionId, '');

  if URI = '/items/delete' then
  begin
    // request body has next format:
    // delete[]=x&delete[]=y&...

    SL := TStringList.Create;
    SubSL := TStringList.Create;
    ToDelete := TStringList.Create;
    try
      SL.Delimiter := '&';
      SL.StrictDelimiter := true;
      SL.DelimitedText := Req;

      for i := 0 to SL.Count - 1 do
      begin
        SubSL.Clear;
        SubSL.Delimiter := '=';
        SubSL.StrictDelimiter := true;
        SubSL.DelimitedText := SL[i];

        if SubSL.Count <> 2 then
        begin
          FServer.SetResponseStatus(ConnectionId, 400); // bad request
          Exit;
        end;

        try
          StrToInt(SubSL[1]); // check if value is integer
        except
          FServer.SetResponseStatus(ConnectionId, 400); // bad request
          Exit;
        end;

        ToDelete.Add(SubSL[1]);
      end;

      ToDelete.CustomSort(DeleteCompare);

      FItems.BeginUpdate;
      try
        for i := 0 to ToDelete.Count - 1 do
          FItems.Delete(StrToInt(ToDelete[i]));
      finally
        FItems.EndUpdate;
      end;
    finally
      ToDelete.Free;
      SubSL.Free;
      SL.Free;
    end;

    Handled := true;
  end;
end;

procedure TFormRestserver.DoPost(Sender: TObject; ConnectionId: Int64;
  const URI: String; var Handled: Boolean);
var
  Req: string;
begin
  // simplify here to eliminate JSON parsing in Delphi
  if URI = '/items/add' then
  begin
    Req := FServer.GetRequestString(ConnectionId, '');
    FItems.Add(Req);

    FServer.SetResponseStatus(ConnectionId, 200);
    Handled := true;
  end;
end;

procedure TFormRestserver.bbStartClick(Sender: TObject);
var
  Port: integer;
  CertificateManager: TsbxCertificateManager;
begin
  if FServer.Active then
    Exit;

  if TryStrToInt(edPort.Text, Port) then
    FServer.Port := Port;

  if cbUseTLS.Checked then
  begin
    FServer.TLSSettings.TLSMode := TsbxRESTServerTLSTLSModes.smImplicitTLS;
    CertificateManager := TsbxCertificateManager.Create(nil);
    try
      try
        CertificateManager.ImportFromFile(edCertFile.Text, edCertPassword.Text);

        FServer.TLSServerChain.Add(CertificateManager.Certificate);
      except
        on E: Exception do
          MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
      end;
    finally
      FreeAndNil(CertificateManager);
    end;
  end;

  FServer.OnAccept := DoAccept;
  FServer.OnGetRequest := DoGet;
  FServer.OnDeleteRequest := DoDelete;
  FServer.OnPostRequest := DoPost;

  FServer.Start;

  ShellExecute(0, 'open', PChar('http://localhost:' + edPort.Text), nil, nil, SW_SHOWNORMAL);
end;

procedure TFormRestserver.bbStopClick(Sender: TObject);
begin
  if FServer.Active then
    FServer.Stop;
end;

procedure TFormRestserver.FormCreate(Sender: TObject);
begin
  FServer := TSBxRESTServer.Create(nil);
  FItems := TStringList.Create;

  FItems.OnChange := ViewItems;

  FItems.BeginUpdate;
  try
    FItems.Add('Item 1');
    FItems.Add('Item 2');
    FItems.Add('Item 3');
  finally
    FItems.EndUpdate;
  end;
end;

procedure TFormRestserver.FormDestroy(Sender: TObject);
begin
  if FServer.Active then
    FServer.Stop;

  FServer.Free;
  FItems.Free;
end;

procedure TFormRestserver.SpeedButton1Click(Sender: TObject);
begin
  if OpenFileDlg.Execute then
    edCertFile.Text := OpenFileDlg.FileName;
end;

end.
