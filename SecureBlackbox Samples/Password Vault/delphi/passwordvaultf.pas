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
unit passwordvaultf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, Types, StrUtils, SBxPasswordVault, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TFormPasswordVault = class(TForm)
    dlgSave: TSaveDialog;
    pTitle: TPanel;
    Label10: TLabel;
    pMain: TPanel;
    pLeft: TPanel;
    SplitterVert: TSplitter;
    Panel1: TPanel;
    lvValues: TListView;
    lvEntries: TListView;
    Panel2: TPanel;
    MainMenu1: TMainMenu;
    pmEntries: TPopupMenu;
    miVault: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miClose: TMenuItem;
    dlgOpen: TOpenDialog;
    miAdd: TMenuItem;
    miRemove: TMenuItem;
    miChangekey: TMenuItem;
    pmValues: TPopupMenu;
    miAdd1: TMenuItem;
    miRemove1: TMenuItem;
    miModify1: TMenuItem;
    edTitle: TEdit;
    Label1: TLabel;
    edDescription: TEdit;
    Label2: TLabel;
    N1: TMenuItem;
    miExit: TMenuItem;
    N2: TMenuItem;
    miRefresh1: TMenuItem;
    miRemoveAll: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miAddClick(Sender: TObject);
    procedure miRemoveClick(Sender: TObject);
    procedure miChangekeyClick(Sender: TObject);
    procedure lvEntriesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure miAdd1Click(Sender: TObject);
    procedure miModify1Click(Sender: TObject);
    procedure miRemove1Click(Sender: TObject);
    procedure pmEntriesPopup(Sender: TObject);
    procedure pmValuesPopup(Sender: TObject);
    procedure miRefresh1Click(Sender: TObject);
    procedure miRemoveAllClick(Sender: TObject);
  private
    FVault: TSBxPasswordVault;
    FCurEntry: string;

    procedure ClearEntries;
    procedure ClearValues;
    procedure RefreshEntryList;
    procedure RefreshValueList(EntryName: string);

    procedure DoKeyNeeded(Sender: TObject; var Cancel: Boolean);
    procedure DoEntryKeyNeeded(Sender: TObject; const EntryName: String; var Cancel: Boolean);
  public
    { Public declarations }
  end;

var
  FormPasswordVault: TFormPasswordVault;
  

implementation

uses editvaluef, passwordf, newentryf, changekeyf;

{$R *.DFM}

procedure TFormPasswordVault.ClearEntries;
begin
  lvEntries.Items.Clear;
  FCurEntry := '';
  ClearValues;
end;

procedure TFormPasswordVault.ClearValues;
begin
  lvValues.Items.Clear;
end;

procedure TFormPasswordVault.RefreshEntryList;
var
  i: integer;
  Entries: string;
  EntryList: TStringDynArray;
  Item: TListItem;
begin
  try
    ClearEntries;

    Entries := FVault.ListEntries;

    EntryList := SplitString(Entries, ';');

    for i := 0 to Length(EntryList) - 1 do
    begin
      Item := lvEntries.Items.Add();
      Item.Caption := EntryList[i];
    end;
  except
    on E : Exception do
    begin
      MessageDlg(E.Message, mtError, [mbOk], 0);
    end;
  end;
end;

procedure TFormPasswordVault.RefreshValueList(EntryName: string);
var
  i: integer;
  Fields, Value: string;
  FieldList: TStringDynArray;
  Item: TListItem;
begin
  try
    ClearValues;
    FCurEntry := EntryName;
    FVault.EntryKey := nil;

    Fields := FVault.ListFields(EntryName, true);

    FieldList := SplitString(Fields, ';');

    for i := 0 to Length(FieldList) - 1 do
    begin
      Item := lvValues.Items.Add();
      Item.Caption := FieldList[i];

      try
        Value := FVault.GetEntryValueStr(EntryName, FieldList[i]);
      except
        // Error on decryption
        Value := '';
      end;
      Item.SubItems.Add(Value);
    end;
  except
    on E : Exception do
    begin
      MessageDlg(E.Message, mtError, [mbOk], 0);
    end;
  end;
end;

procedure TFormPasswordVault.DoKeyNeeded(Sender: TObject; var Cancel: Boolean);
begin
  if FormPassword.ShowModal = IDOK then
  begin
    FVault.Password := FormPassword.edPassword.Text;
    Cancel := false;
  end
  else
    Cancel := true;
end;

procedure TFormPasswordVault.DoEntryKeyNeeded(Sender: TObject; const EntryName: String; var Cancel: Boolean);
begin
  if FormPassword.ShowModal = IDOK then
  begin
    FVault.EntryPassword := FormPassword.edPassword.Text;
    Cancel := false;
  end
  else
    Cancel := true;
end;

procedure TFormPasswordVault.FormCreate(Sender: TObject);
begin
  FVault := TSBxPasswordVault.Create(nil);
  FVault.OnKeyNeeded := DoKeyNeeded;
  FVault.OnEntryKeyNeeded := DoEntryKeyNeeded;

  FCurEntry := '';
end;

procedure TFormPasswordVault.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FVault);
end;

procedure TFormPasswordVault.pmEntriesPopup(Sender: TObject);
begin
  miRemove.Enabled := lvEntries.SelCount > 0;
  miRemoveAll.Enabled := lvEntries.Items.Count > 0;
  miChangekey.Enabled := lvEntries.SelCount > 0;
end;

procedure TFormPasswordVault.pmValuesPopup(Sender: TObject);
begin
  miAdd1.Enabled := FCurEntry <> '';
  miModify1.Enabled := lvValues.SelCount > 0;
  miRemove1.Enabled := lvValues.SelCount > 0;
end;

procedure TFormPasswordVault.miOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute() then
  begin
    try
      FVault.OpenFile(dlgOpen.FileName);

      edTitle.Text := FVault.Title;
      edDescription.Text := FVault.Description;

      RefreshEntryList;
    except
      on E : Exception do
      begin
        MessageDlg(E.Message, mtError, [mbOk], 0);
      end;
    end;
  end;
end;

procedure TFormPasswordVault.miSaveClick(Sender: TObject);
begin
  if dlgSave.Execute() then
  begin
    try
      FVault.Title := edTitle.Text;
      FVault.Description := edDescription.Text;

      FVault.SaveFile(dlgSave.FileName);
    except
      on E : Exception do
      begin
        MessageDlg(E.Message, mtError, [mbOk], 0);
      end;
    end;
  end;
end;

procedure TFormPasswordVault.miCloseClick(Sender: TObject);
begin
  FVault.Close;

  edTitle.Text := '';
  edDescription.Text := '';

  ClearEntries;
end;

procedure TFormPasswordVault.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormPasswordVault.miAddClick(Sender: TObject);
var
  Item: TListItem;
begin
  if FormNewEntry.ShowModal = IDOK then
  begin
    try
      FVault.AddEntry(FormNewEntry.edEntryName.Text);

      // add new entry to list
      Item := lvEntries.Items.Add();
      Item.Caption := FormNewEntry.edEntryName.Text;
    except
      on E : Exception do
      begin
        MessageDlg(E.Message, mtError, [mbOk], 0);
      end;
    end;
  end;
end;

procedure TFormPasswordVault.miRemoveClick(Sender: TObject);
begin
  if Assigned(lvEntries.Selected) then
  begin
    if MessageDlg('Remove entry "' + lvEntries.Selected.Caption + '" ?', mtConfirmation, mbOKCancel, 0) = mrOK then
    begin
      try
        FVault.RemoveEntry(lvEntries.Selected.Caption);

        lvEntries.DeleteSelected;
      except
        on E : Exception do
        begin
          MessageDlg(E.Message, mtError, [mbOk], 0);
        end;
      end;
    end;
  end;
end;

procedure TFormPasswordVault.miChangekeyClick(Sender: TObject);
begin
  if Assigned(lvEntries.Selected) then
  begin
    FormChangeKey.Caption := 'Change password of entry "' + lvEntries.Selected.Caption + '"';
    FormChangeKey.edOldPassword.Text := FVault.EntryPassword;

    if FormChangeKey.ShowModal = IDOK then
    begin
      try
        FVault.EntryPassword := FormChangeKey.edOldPassword.Text;
        FVault.ChangeEntryPassword(lvEntries.Selected.Caption, FormChangeKey.edNewPassword.Text);
      except
        on E : Exception do
        begin
          MessageDlg(E.Message, mtError, [mbOk], 0);
        end;
      end;
    end;
  end;
end;

procedure TFormPasswordVault.miAdd1Click(Sender: TObject);
var
  Item: TListItem;
begin
  FormEditValue.Init('', '');

  if FormEditValue.ShowModal = IDOK then
  begin
    try
      FVault.SetEntryValueStr(FCurEntry, FormEditValue.edFieldName.Text, FormEditValue.edValue.Text, FormEditValue.cbEncrypted.Checked);

      // add new value to list
      Item := lvValues.Items.Add();
      Item.Caption := FormEditValue.edFieldName.Text;
      Item.SubItems.Add(FormEditValue.edValue.Text);
    except
      on E : Exception do
      begin
        MessageDlg(E.Message, mtError, [mbOk], 0);
      end;
    end;
  end;
end;

procedure TFormPasswordVault.miModify1Click(Sender: TObject);
begin
  if Assigned(lvValues.Selected) then
  begin
    FormEditValue.Init(lvValues.Selected.Caption, lvValues.Selected.SubItems[0]);

    if FormEditValue.ShowModal = IDOK then
    begin
      try
        FVault.SetEntryValueStr(FCurEntry, FormEditValue.edFieldName.Text, FormEditValue.edValue.Text, FormEditValue.cbEncrypted.Checked);

        // modify value in list
        lvValues.Selected.SubItems[0] := FormEditValue.edValue.Text;
      except
        on E : Exception do
        begin
          MessageDlg(E.Message, mtError, [mbOk], 0);
        end;
      end;
    end;
  end;
end;

procedure TFormPasswordVault.miRemove1Click(Sender: TObject);
begin
  if Assigned(lvValues.Selected) then
  begin
    if MessageDlg('Remove value of "' + lvValues.Selected.Caption + '" ?', mtConfirmation, mbOKCancel, 0) = mrOK then
    begin
      try
        FVault.RemoveField(FCurEntry, lvValues.Selected.Caption);

        lvValues.DeleteSelected;
      except
        on E : Exception do
        begin
          MessageDlg(E.Message, mtError, [mbOk], 0);
        end;
      end;
    end;
  end;
end;

procedure TFormPasswordVault.miRemoveAllClick(Sender: TObject);
begin
  if MessageDlg('Remove all entries?', mtConfirmation, mbOKCancel, 0) = mrOK then
  begin
    try
      FVault.RemoveAllEntries;
      ClearEntries;
    except
      on E : Exception do
      begin
        MessageDlg(E.Message, mtError, [mbOk], 0);
      end;
    end;
  end;
end;

procedure TFormPasswordVault.miRefresh1Click(Sender: TObject);
begin
  if Assigned(lvValues.Selected) then
  begin
    try
      lvValues.Selected.SubItems[0] := FVault.GetEntryValueStr(FCurEntry, lvValues.Selected.Caption);
    except
      // Error on decryption
    end;
  end;
end;

procedure TFormPasswordVault.lvEntriesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
    RefreshValueList(Item.Caption);
end;

end.



