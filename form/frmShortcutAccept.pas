unit frmShortcutAccept;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  frmBase, Menus, LCLType, LCLProc;

type

  { TFormShortcutAccept }

  TFormShortcutAccept = class(TFormBase)
    lblPressKey: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FAcceptShortcut: TShortCut;
  protected
    procedure InitComponents; override;
    procedure InitEvents; override;
    procedure InitLogic; override;
    procedure InitTheme; override;
  public
  published
    property AcceptShortcut: TShortCut read FAcceptShortcut write FAcceptShortcut;
  end;

var
  FormShortcutAccept: TFormShortcutAccept;

implementation

{$R *.lfm}

{ TFormShortcutAccept }

procedure TFormShortcutAccept.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  sc: TShortCut;
begin
  // key down
  sc := ShortCut(key, Shift);
  lblPressKey.Caption:= ShortCutToText(sc);
end;

procedure TFormShortcutAccept.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // key up
  FAcceptShortcut := ShortCut(key, Shift);
  ModalResult:= mrOK;
end;

procedure TFormShortcutAccept.InitComponents;
begin
  //
end;

procedure TFormShortcutAccept.InitEvents;
begin
  //
end;

procedure TFormShortcutAccept.InitLogic;
begin
  //
end;

procedure TFormShortcutAccept.InitTheme;
begin
  //
end;

end.

