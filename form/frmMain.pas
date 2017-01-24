unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, frmBase;

type

  { TFormMain }

  TFormMain = class(TFormBase)
    imgLst: TImageList;
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
    pgCode: TPageControl;
    pnlProjectFiles: TPanel;
    splProjectFiles: TSplitter;
    tbMain: TToolBar;
    tvProjectFiles: TTreeView;
  private
    FCurrentProjectName: string;
    FCurrentProjectPath: string;

  protected
    procedure InitComponents; override;
    procedure InitEvents; override;
    procedure InitLogic; override;
  public
    procedure LoadProjectFiles(path: string);
  published
    property CurrentProjectName: string read FCurrentProjectName write FCurrentProjectName;
    property CurrentProjectPath: string read FCurrentProjectPath write FCurrentProjectPath;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  projectUtils, smaliCodeView;

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
var
  page: TSmaliCodeView;
begin
  LoadProjectFiles('/media/psf/Home/Develop/mi5/Updater/apktool.yml');
  page := TSmaliCodeView.Create(pgCode);
  page.Parent := pgCode;
  page.FileName:= 'TEST';
  pgCode.TabIndex:= 0;
end;

procedure TFormMain.LoadProjectFiles(path: string);
begin
  projectUtils.LoadProjectFiles(path, tvProjectFiles.Items, nil);
end;

end.

