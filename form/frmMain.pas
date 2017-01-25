unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, frmBase, math;

type

  { TFormMain }

  TFormMain = class(TFormBase)
    imgLst: TImageList;
    miSaveAs: TMenuItem;
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
    tBtnNewProject: TToolButton;
    tBtnCut: TToolButton;
    tBtnCopy: TToolButton;
    tBtnPaste: TToolButton;
    tBtnDelete: TToolButton;
    tSp4: TToolButton;
    tBtnJava: TToolButton;
    tBtnOpenProject: TToolButton;
    tSp1: TToolButton;
    tBtnSave: TToolButton;
    tBtnSaveAs: TToolButton;
    tSp2: TToolButton;
    tBtnUndo: TToolButton;
    tBtnRedo: TToolButton;
    tSp3: TToolButton;
    tvProjectFiles: TTreeView;
    procedure miCopyClick(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miFindClick(Sender: TObject);
    procedure miOpenProjClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure miRedoClick(Sender: TObject);
    procedure miSaveAllFilesClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miSaveFileClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure pgCodeCloseTabClicked(Sender: TObject);
    procedure tvProjectFilesClick(Sender: TObject);
  private
    FCurrentProjectName: string;
    FCurrentProjectPath: string;
  protected
    procedure InitComponents; override;
    procedure InitEvents; override;
    procedure InitLogic; override;
  public

  published
    property CurrentProjectName: string read FCurrentProjectName write FCurrentProjectName;
    property CurrentProjectPath: string read FCurrentProjectPath write FCurrentProjectPath;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  projectUtils, smaliCodeView, TextUtils;

{ TFormMain }

procedure TFormMain.tvProjectFilesClick(Sender: TObject);
var
  node: TTreeNode;
  path: string;
begin
  //
  node := tvProjectFiles.Selected;
  if (node <> nil) then begin
    path:= node.Text;
    while (node.Parent <> nil) do begin
      path:= node.Parent.Text + '/' + path;
      node := node.Parent;
    end;
    // TODO: open file
  end;

end;

procedure TFormMain.miOpenProjClick(Sender: TObject);
begin
  CurrentProjectPath:= projectUtils.OpenProject();
  if (CurrentProjectPath <> '') then begin
    projectUtils.LoadProjectFiles(CurrentProjectPath, tvProjectFiles.Items, nil);
  end;
end;

procedure TFormMain.miPasteClick(Sender: TObject);
begin
  if (pgCode.ActivePage is TSmaliCodeView) then begin
    TextUtils.Paste(TSmaliCodeView(pgCode.ActivePage).Editor);
  end;
end;

procedure TFormMain.miCutClick(Sender: TObject);
begin
  if (pgCode.ActivePage is TSmaliCodeView) then begin
    TextUtils.Cut(TSmaliCodeView(pgCode.ActivePage).Editor);
  end;
end;

procedure TFormMain.miDeleteClick(Sender: TObject);
begin
  if (pgCode.ActivePage is TSmaliCodeView) then begin
    TextUtils.Delete(TSmaliCodeView(pgCode.ActivePage).Editor);
  end;
end;

procedure TFormMain.miFindClick(Sender: TObject);
begin
  // TODO: find
end;

procedure TFormMain.miCopyClick(Sender: TObject);
begin
  if (pgCode.ActivePage is TSmaliCodeView) then begin
    TextUtils.Copy(TSmaliCodeView(pgCode.ActivePage).Editor);
  end;
end;

procedure TFormMain.miRedoClick(Sender: TObject);
begin
  if (pgCode.ActivePage is TSmaliCodeView) then begin
    TextUtils.Redo(TSmaliCodeView(pgCode.ActivePage).Editor);
  end;
end;

procedure TFormMain.miSaveAllFilesClick(Sender: TObject);
var
  i: Integer;
begin
  // save all files
  for i := 0 to pgCode.PageCount - 1 do begin
    if (pgCode.Pages[i] is TSmaliCodeView) then begin
      TSmaliCodeView(pgCode.Pages[i]).Save();
    end;
  end;
end;

procedure TFormMain.miSaveAsClick(Sender: TObject);
begin
  if (pgCode.ActivePage is TSmaliCodeView) then begin
    TSmaliCodeView(pgCode.ActivePage).SaveAs();
  end;
end;

procedure TFormMain.miSaveFileClick(Sender: TObject);
begin
  if (pgCode.ActivePage is TSmaliCodeView) then begin
    TSmaliCodeView(pgCode.ActivePage).Save();
  end;
end;

procedure TFormMain.miSelectAllClick(Sender: TObject);
begin
  if (pgCode.ActivePage is TSmaliCodeView) then begin
    TextUtils.SelectAll(TSmaliCodeView(pgCode.ActivePage).Editor);
  end;
end;

procedure TFormMain.miUndoClick(Sender: TObject);
begin
  if (pgCode.ActivePage is TSmaliCodeView) then begin
    TextUtils.Undo(TSmaliCodeView(pgCode.ActivePage).Editor);
  end;
end;

procedure TFormMain.pgCodeCloseTabClicked(Sender: TObject);
var
  idx: Integer;
  tab: TSmaliCodeView;
begin
  idx := pgCode.TabIndex;
  if (Sender is TSmaliCodeView) then begin
    tab := TSmaliCodeView(Sender);
    if (tab.QueryClose()) then begin
      tab.Free;
    end;
  end else begin
    TTabSheet(Sender).Free;
  end;
  idx := ifthen(idx > 0, idx - 1, 0);
  pgCode.TabIndex:= idx;
end;

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
  // TODO: test
  page := TSmaliCodeView.Create(pgCode);
  page.Parent := pgCode;
  page.FileName:= 'TEST';
  pgCode.TabIndex:= 0;
end;

end.

