unit referencesf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,
  SBxTypes;

type
  TFormReferences = class(TForm)
    lbReferences: TListBox;
    btnClose: TButton;
    btnAdd: TButton;
    btnDelete: TButton;
    btnEdit: TButton;
    procedure btnEditClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    FReferences: TsbxXMLReferenceList;

    procedure SetReferences(const Value: TsbxXMLReferenceList);

    procedure UpdateReferences;
  public
    property References: TsbxXMLReferenceList read FReferences write SetReferences;
  end;

var
  FormReferences: TFormReferences;

implementation

{$R *.dfm}

uses referencef;

{ TfrmReferences }

procedure TFormReferences.btnAddClick(Sender: TObject);
var
  Ref: TsbxXMLReference;
begin
  Ref := TsbxXMLReference.Create;
  try
    FormReference.Initialize(Ref);
    if FormReference.ShowModal = mrOK then
    begin
      FormReference.UpdateReference(Ref);
      FReferences.Add(Ref);

      UpdateReferences;
    end
  finally
    FreeAndNil(Ref);
  end;
end;

procedure TFormReferences.btnDeleteClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to lbReferences.Items.Count - 1 do
    if lbReferences.Selected[i] then
    begin
      FReferences.RemoveAt(i);
      Break;
    end;

  UpdateReferences;
end;

procedure TFormReferences.btnEditClick(Sender: TObject);
var
  i: Integer;
  Ref: TsbxXMLReference;
begin
  for i := 0 to lbReferences.Items.Count - 1 do
    if lbReferences.Selected[i] then
    begin
      Ref := TsbxXMLReference(FReferences.Item[i]);

      FormReference.Initialize(Ref);
      if FormReference.ShowModal = mrOK then
      begin
        // recreating a reference, to completely clear it
        FReferences.RemoveAt(i);
        Ref := TsbxXMLReference.Create();
        try
          if FReferences.Count < i then
            FReferences.Insert(i, Ref)
          else
            FReferences.Add(Ref);
        finally
          FreeAndNil(Ref);
        end;

        FormReference.UpdateReference(FReferences.Item[i]);

        UpdateReferences;
      end;

      Break;
    end;
end;

procedure TFormReferences.SetReferences(const Value: TsbxXMLReferenceList);
begin
  FReferences := Value;
  UpdateReferences;
end;

procedure TFormReferences.UpdateReferences;
var
  i: Integer;
  Id, s: string;
begin
  lbReferences.Clear;
  for i := 0 to FReferences.Count - 1 do
  begin
    Id := TsbxXMLReference(FReferences.Item[i]).ID;
    s := TsbxXMLReference(FReferences.Item[i]).URI;
    if Length(s) = 0 then
      s := TsbxXMLReference(FReferences.Item[i]).TargetXMLElement;

    if Length(s) = 0 then
    begin
      if Length(TsbxXMLReference(FReferences.Item[i]).TargetData) = 0 then
        s := '#document'
      else
        s := '#data';
    end;

    if Length(Id) > 0 then
      s := Id + ' - ' + s;

    lbReferences.Items.AddObject(s, FReferences.Item[i]);
  end;
end;

end.
