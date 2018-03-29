unit frmInputBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, frmBase, Buttons;

type

  { TFormInputBox }

  TFormInputBox = class(TFormBase)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    edtText: TEdit;
    lblTitle: TLabel;
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
  FormInputBox: TFormInputBox;

function InputBox(ACaption, AMessage, ADefault: string): string;

implementation

uses
  ThemeUtils;

function InputBox(ACaption, AMessage, ADefault: string): string;
begin
  Result := ADefault;
  with TFormInputBox.Create(nil) do begin
    Caption:= ACaption;
    lblTitle.Caption:= AMessage;
    if ShowModal = mrOK then begin
      Result := edtText.Text;
    end;
    Free;
  end;
end;

{$R *.frm}

{ TFormInputBox }

procedure TFormInputBox.InitComponents;
begin
  //
end;

procedure TFormInputBox.InitEvents;
begin
  //
end;

procedure TFormInputBox.InitLogic;
begin
  //
end;

procedure TFormInputBox.InitTheme;
begin
  ThemeUtils.RecolorEdit(edtText);
  ThemeUtils.RecolorButton(btnOK);
  ThemeUtils.RecolorButton(btnCancel);
end;

end.

