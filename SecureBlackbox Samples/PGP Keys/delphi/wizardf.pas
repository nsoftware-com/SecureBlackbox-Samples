unit wizardf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Math,
  SBxTypes, SBxCore, SBxPGPKeyManager;

type
  TFormWizard = class(TForm)
    pnlButtons: TPanel;
    bvlButtonsTop: TBevel;
    btnBack: TButton;
    btnNext: TButton;
    btnCancel: TButton;
    pnlStep1: TPanel;
    pnlStep1Top: TPanel;
    lblStep1Caption: TLabel;
    lblStep1Description: TLabel;
    bvlStep1Top: TBevel;

    pnlStep2: TPanel;
    pnlStep2Top: TPanel;
    lblStep2Caption: TLabel;
    lblStep2Description: TLabel;
    bvlStep2Top: TBevel;

    pnlStep3: TPanel;
    pnlStep3Top: TPanel;
    lblStep3Caption: TLabel;
    lblStep3Description: TLabel;
    bvlStep3Top: TBevel;

    pnlStep4: TPanel;
    pnlStep4Top: TPanel;
    lblStep4Caption: TLabel;
    lblStep4Description: TLabel;
    bvlStep4Top: TBevel;

    pnlStep5: TPanel;
    pnlStep5Top: TPanel;
    lblStep5Caption: TLabel;
    lblStep5Description: TLabel;
    bvlStep5Top: TBevel;

    pnlStep6: TPanel;
    pnlStep6Top: TPanel;
    lblStep6Caption: TLabel;
    lblStep6Description: TLabel;
    bvlStep6Top: TBevel;
    lblStep1Prompt: TLabel;
    btnFinish: TButton;
    lblStep2Prompt: TLabel;
    edtPassword: TEdit;
    lblPassword: TLabel;
    lblConfirmation: TLabel;
    edtConfirmation: TEdit;
    lblName: TLabel;
    edtName: TEdit;
    edtEMail: TEdit;
    lblEMail: TLabel;
    cmbKeyType: TComboBox;
    lblKeyType: TLabel;
    lblKeyExpiration: TLabel;
    rbtNever: TRadioButton;
    dtpExpirationDate: TDateTimePicker;
    rbtDate: TRadioButton;
    lblKeySize: TLabel;
    cmbKeySize: TComboBox;
    pbrProgress: TProgressBar;
    lblStep3Prompt: TLabel;
    tmrProgress: TTimer;
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pnlStep2Exit(Sender: TObject);
    procedure pnlStep2Enter(Sender: TObject);
    procedure pnlStep1Enter(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pnlStep3Enter(Sender: TObject);
    procedure rbtDateClick(Sender: TObject);
    procedure edtNameChange(Sender: TObject);
    procedure tmrProgressTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cmbKeyTypeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FStep: Integer;
    FCount: Integer;
    FSteps: array [1..6] of TPanel;
  public
    FCompleted: Boolean;
    FKeyManager: TsbxPGPKeyManager;
  end;

  TKeyGenerationThread = class(TThread)
  private
    FException: Exception;
    frmWizard: TFormWizard;
    procedure RaiseException;
  protected
    procedure DoTerminate; override;
    constructor Create(AWizard: TFormWizard);
    procedure Execute; override;
  end;

var
  FormWizard: TFormWizard;

implementation

{$R *.DFM}

procedure TFormWizard.FormCreate(Sender: TObject);
begin
  FStep := 1;
  FCount := 3;
  FSteps[1] := pnlStep1;
  FSteps[2] := pnlStep2;
  FSteps[3] := pnlStep3;
  FSteps[4] := pnlStep4;
  FSteps[5] := pnlStep5;
  FSteps[6] := pnlStep6;
  pnlStep1.Show;

  FKeyManager := TsbxPGPKeyManager.Create(nil);
end;

procedure TFormWizard.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FKeyManager);
end;

procedure TFormWizard.FormShow(Sender: TObject);
begin
  cmbKeyType.ItemIndex := 1;
  cmbKeySize.ItemIndex := 1;
  dtpExpirationDate.Date := Date + 365;
  edtName.SetFocus;
end;

procedure TFormWizard.btnBackClick(Sender: TObject);
begin
  if FStep = FCount then
    begin
      btnFinish.Hide;
      btnNext.Show;
    end;
  FSteps[FStep].Hide;
  Dec(FStep);
  FSteps[FStep].Show;
  if @FSteps[FStep].OnEnter <> nil then
    FSteps[FStep].OnEnter(nil);
  btnBack.Enabled := FStep > 1;
end;

procedure TFormWizard.btnNextClick(Sender: TObject);
begin
  if @FSteps[FStep].OnExit <> nil then
    FSteps[FStep].OnExit(nil);
  FSteps[FStep].Hide;
  Inc(FStep);
  FSteps[FStep].Show;
  if FStep = FCount then
    begin
      btnFinish.Show;
      btnNext.Hide;
    end;
  btnBack.Enabled := FStep > 1;
  if @FSteps[FStep].OnEnter <> nil then
    FSteps[FStep].OnEnter(nil);
end;

procedure TFormWizard.pnlStep1Enter(Sender: TObject);
begin
  edtName.SetFocus;
end;

procedure TFormWizard.pnlStep2Enter(Sender: TObject);
begin
  edtPassword.SetFocus;
end;

procedure TFormWizard.pnlStep2Exit(Sender: TObject);
begin
  if Sender = nil then
    begin
      if edtPassword.Text <> edtConfirmation.Text then
        raise Exception.Create('Password and confirmation not equal');
    end;
end;

procedure TFormWizard.pnlStep3Enter(Sender: TObject);
begin
  tmrProgress.Enabled := True;
  btnBack.Enabled := False;
  btnFinish.Enabled := False;
  btnCancel.Enabled := False;
  TKeyGenerationThread.Create(Self);
end;

procedure TFormWizard.rbtDateClick(Sender: TObject);
begin
  dtpExpirationDate.Enabled := rbtDate.Checked;
  if Sender = rbtDate then
    dtpExpirationDate.SetFocus;
end;

procedure TFormWizard.edtNameChange(Sender: TObject);
begin
  btnNext.Enabled := edtName.Text <> '';
end;

procedure TFormWizard.tmrProgressTimer(Sender: TObject);
begin
  if FCompleted then
    begin
      pbrProgress.Position := 100;
      tmrProgress.Enabled := False;
      btnBack.Enabled := True;
      btnFinish.Enabled := True;
      btnCancel.Enabled := True;
    end
  else
    pbrProgress.Position := (pbrProgress.Position + 5) mod 105;
end;

{ TKeyGenerationThread }

constructor TKeyGenerationThread.Create(AWizard: TFormWizard);
begin
  inherited Create(True);
  frmWizard := AWizard;
  FreeOnTerminate := True;
  Resume;
end;

procedure TKeyGenerationThread.DoTerminate;
begin
  frmWizard.FCompleted := True;
  inherited;
end;

procedure TKeyGenerationThread.Execute;
var
  UserName: String;
  Bits, Expires: Integer;
begin
  try
    UserName := frmWizard.edtName.Text;
    if frmWizard.edtEMail.Text <> '' then
      UserName := UserName + '<' + frmWizard.edtEMail.Text + '>';

    if frmWizard.rbtNever.Checked then
      Expires := 0
    else
      Expires := Round(frmWizard.dtpExpirationDate.Date - Date);

    if frmWizard.cmbKeyType.ItemIndex = 0 then
    begin
      Bits := 1024 * Round(IntPower(2, frmWizard.cmbKeySize.ItemIndex));

      frmWizard.FKeyManager.GenerateLegacy(UserName, Bits, frmWizard.edtPassword.Text, Expires);
    end
    else
    if frmWizard.cmbKeyType.ItemIndex = 1 then
    begin
      Bits := 1024 * Round(IntPower(2, frmWizard.cmbKeySize.ItemIndex));

      frmWizard.FKeyManager.GeneratePair(UserName, 'DSA', 1024, 'Elgamal-encrypt', Bits, frmWizard.edtPassword.Text, Expires);
    end
    else
    if frmWizard.cmbKeyType.ItemIndex = 2 then
    begin
      if frmWizard.cmbKeySize.ItemIndex = 0 then
        Bits := 256
      else if frmWizard.cmbKeySize.ItemIndex = 1 then
        Bits := 384
      else
        Bits := 521;

      frmWizard.FKeyManager.GeneratePair(UserName, 'ECDSA', Bits, 'ECDH', Bits, frmWizard.edtPassword.Text, Expires);
    end;
  except
    on E: Exception do
      begin
        FException := E;
        Synchronize(RaiseException);
      end;
  end;
end;

procedure TKeyGenerationThread.RaiseException;
begin
  Application.ShowException(FException);
end;

procedure TFormWizard.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (FStep = 3) and not FCompleted then
    Action := caNone; 
end;

procedure TFormWizard.cmbKeyTypeChange(Sender: TObject);
begin
  if cmbKeyType.ItemIndex <> 2 then
  begin
    cmbKeySize.Items.SetText('1024 bits (weak)'#13#10'2048 bits (normal)'#13#10'4096 bits (strong)'#13#10'8192 bits (paranoic)');
    cmbKeySize.ItemIndex := 1;
  end
  else
  begin
    cmbKeySize.Items.SetText('256 bits (strong)'#13#10'384 bits (stronger)'#13#10'521 bits (paranoic)');
    cmbKeySize.ItemIndex := 0;    
  end;
end;

end.

