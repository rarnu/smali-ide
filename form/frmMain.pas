unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, frmBase;

type

  { TFormMain }

  TFormMain = class(TFormBase)
    miAbout: TMenuItem;
    mm7: TMenuItem;
    miUpdate: TMenuItem;
    miDocument: TMenuItem;
    miHelp: TMenuItem;
    miSyntaxDefault: TMenuItem;
    miSyntxHighlight: TMenuItem;
    mm6: TMenuItem;
    miToJava: TMenuItem;
    miTemplate: TMenuItem;
    miCode: TMenuItem;
    miGotoLine: TMenuItem;
    mm5: TMenuItem;
    miFindInFiles: TMenuItem;
    mm4: TMenuItem;
    miFindNext: TMenuItem;
    miReplace: TMenuItem;
    miFind: TMenuItem;
    mm2: TMenuItem;
    mm3: TMenuItem;
    miSearch: TMenuItem;
    miSelectAll: TMenuItem;
    miDelete: TMenuItem;
    miPaste: TMenuItem;
    miCopy: TMenuItem;
    miCut: TMenuItem;
    miRedo: TMenuItem;
    miUndo: TMenuItem;
    miFile: TMenuItem;
    miEdit: TMenuItem;
    miNewProj: TMenuItem;
    miOpenProj: TMenuItem;
    mm0: TMenuItem;
    miSaveFile: TMenuItem;
    miSaveAllFiles: TMenuItem;
    mm1: TMenuItem;
    miExit: TMenuItem;
    mmMain: TMainMenu;
  private
  protected
    procedure InitComponents; override;
    procedure InitEvents; override;
    procedure InitLogic; override;
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.InitComponents;
begin
  //
end;

procedure TFormMain.InitEvents;
begin
  //
end;

procedure TFormMain.InitLogic;
begin
  //
end;

end.

