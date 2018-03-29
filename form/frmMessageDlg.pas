unit frmMessageDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, frmBase, Buttons;

type

  TButtonType = (btYesNoCancel, btOkCancel, btOK);
  { TFormMessageDlg }

  TFormMessageDlg = class(TFormBase)
    btnCancel: TBitBtn;
    btnYes: TBitBtn;
    btnNo: TBitBtn;
    lblMessage: TLabel;
    pnlButton: TPanel;
  private
  protected
    procedure InitComponents; override;
    procedure InitEvents; override;
    procedure InitLogic; override;
    procedure InitTheme; override;
  public

  end;

var
  FormMessageDlg: TFormMessageDlg;

function MessageDlg(ACaption, AMessage: string; AButtonType: TButtonType): TModalResult;

implementation

uses
  ThemeUtils;

function MessageDlg(ACaption, AMessage: string; AButtonType: TButtonType
  ): TModalResult;
begin
  Result := mrCancel;
  with TFormMessageDlg.Create(nil) do begin
    Caption:= ACaption;
    lblMessage.Caption:= AMessage;
    case AButtonType of
    btYesNoCancel:
      begin
        btnYes.Default:= True;
        btnYes.Cancel:= False;
        btnYes.ModalResult:= mrYes;
        btnYes.Caption:= 'Yes';
        btnNo.Default:= False;
        btnNo.Cancel:= False;
        btnNo.ModalResult:= mrNo;
        btnNo.Caption:= 'No';
        btnCancel.Default:= False;
        btnCancel.Cancel:= True;
        btnCancel.ModalResult:= mrCancel;
        btnCancel.Caption:= 'Cancel';
      end;
    btOkCancel:
      begin
        btnNo.Visible:= False;
        btnYes.Default:= True;
        btnYes.Cancel:= False;
        btnYes.ModalResult:= mrOK;
        btnYes.Caption:= 'OK';
        btnCancel.Default:= False;
        btnCancel.Cancel:= True;
        btnCancel.ModalResult:= mrCancel;
        btnCancel.Caption:= 'Cancel';
      end;
    btOK:
      begin
        btnNo.Visible:= False;
        btnCancel.Visible:= False;
        btnYes.Default:= True;
        btnYes.Cancel:= False;
        btnYes.ModalResult:= mrOK;
        btnYes.Caption:= 'OK';
      end;
    end;
    Result := ShowModal;
    Free;
  end;
end;

{$R *.frm}

{ TFormMessageDlg }

procedure TFormMessageDlg.InitComponents;
begin
  //
end;

procedure TFormMessageDlg.InitEvents;
begin
  //
end;

procedure TFormMessageDlg.InitLogic;
begin
  //
end;

procedure TFormMessageDlg.InitTheme;
begin
  ThemeUtils.RecolorButton(btnYes);
  ThemeUtils.RecolorButton(btnNo);
  ThemeUtils.RecolorButton(btnCancel);
end;

end.

