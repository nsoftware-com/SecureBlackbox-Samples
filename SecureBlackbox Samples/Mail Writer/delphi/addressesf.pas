unit addressesf;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  sbxtypes, SBxMailWriter;

type
  TFormAddresses = class(TForm)
    lblName: TLabel;
    edtName: TEdit;
    lblAddress: TLabel;
    lblAddresses: TLabel;
    lvwAddresses: TListView;
    edtAddress: TEdit;
    btnAdd: TButton;
    btnDelete: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    function GetAddresses(): string;
    procedure SetAddresses(const Value: string);
  public
    class function Execute(Caption: string; var Addresses: string): Boolean;
    property Addresses: string read GetAddresses write SetAddresses;
  end;

var
  FormAddresses: TFormAddresses;

implementation

{$R *.dfm}

{ TFormAddresses }

procedure TFormAddresses.btnAddClick(Sender: TObject);
var
  Item: TListItem;
begin
  if edtAddress.Text = '' then
  begin
    MessageDlg('E-mail Address is required in order to add an item.', mtError, [mbOk], 0);
    Exit;
  end;

  Item := lvwAddresses.Items.Add();
  Item.Caption := edtName.Text;
  Item.SubItems.Add(edtAddress.Text);
  Item.MakeVisible(False);
  Item.Selected := True;

  edtName.Text := '';
  edtAddress.Text := '';
end;

procedure TFormAddresses.btnDeleteClick(Sender: TObject);
var
  Current: Integer;
begin
  if lvwAddresses.Selected = nil then
    Exit;

  Current := lvwAddresses.Selected.Index;
  lvwAddresses.Selected.Delete();
  if lvwAddresses.Items.Count = 0 then
    Exit;

  if Current < lvwAddresses.Items.Count then
    lvwAddresses.Items[Current].Selected := True
  else
    lvwAddresses.Items[Current - 1].Selected := True;
end;

class function TFormAddresses.Execute(Caption: string; var Addresses: string): Boolean;
var
  Dialog: TFormAddresses;
begin
  Application.CreateForm(TFormAddresses, Dialog);
  Dialog.Caption := Format(Dialog.Caption, [Caption]);
  Dialog.Addresses := Addresses;
  Result := (Dialog.ShowModal() = mrOk);
  if Result then
    Addresses := Dialog.Addresses;
  FreeAndNil(Dialog);
end;

function TFormAddresses.GetAddresses(): string;
var
  I: Integer;
  Writer: TsbxMailWriter;
  Address: TsbxMailAddress;
  Item: TListItem;
begin
  if edtAddress.Text <> '' then
    btnAdd.Click();

  if lvwAddresses.Items.Count = 0 then
  begin
    Result := '';
    Exit;
  end;

  Writer := TsbxMailWriter.Create(nil);
  try
    for I := 0 to lvwAddresses.Items.Count - 1 do
    begin
      Item := lvwAddresses.Items[I];

      Address := TsbxMailAddress.Create(Item.Caption, Item.SubItems[0]);
      Writer.From.Add(Address); // all address fields in TsbxMailWriter behave the same way, so there is
                                // no difference which one to use to compose a string from an address list
    end;
    try
      Result := Writer.Message.From;
    except
      Result := '';
    end;
  finally
    FreeAndNil(Writer);
  end;
end;

procedure TFormAddresses.SetAddresses(const Value: string);
var
  Writer: TsbxMailWriter;
  I: Integer;
  Item: TListItem;
begin
  Writer := TsbxMailWriter.Create(nil);
  try
    try
      Writer.Message.From := Value;   // all address fields in TsbxMailWriter behave the same way, so there is
                                      // no difference which one to use to split a string into an address list
    except
      Exit;
    end;

    for I := 0 to Writer.From.Count - 1 do
    begin
      Item := lvwAddresses.Items.Add();
      Item.Caption := Writer.From[I].DisplayName;
      Item.SubItems.Add(Writer.From[I].Address);
    end;
  finally
    FreeAndNil(Writer);
  end;
end;

end.
