unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, StdCtrls, frmBase, math, LCLType;

type
  { TFormMain }

  TFormMain = class(TFormBase)
    edtFilterClass: TEdit;
    edtFilterFile: TEdit;
    imgLst: TImageList;
    lstSearchResult: TListBox;
    miSearchResult: TMenuItem;
    miClassIndex: TMenuItem;
    miView: TMenuItem;
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
    pnlSearch: TPanel;
    pnlClassIndex: TPanel;
    pgCode: TPageControl;
    pnlProjectFiles: TPanel;
    splBottom: TSplitter;
    splRight: TSplitter;
    splProjectFiles: TSplitter;
    sbMain: TStatusBar;
    tbMain: TToolBar;
    tBtnNewProject: TToolButton;
    tBtnCut: TToolButton;
    tBtnCopy: TToolButton;
    tBtnPaste: TToolButton;
    tBtnDelete: TToolButton;
    tvClassIndex: TTreeView;
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
    procedure lstSearchResultKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure miAboutClick(Sender: TObject);
    procedure miClassIndexClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miDocumentClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miFindClick(Sender: TObject);
    procedure miFindInFilesClick(Sender: TObject);
    procedure miFindNextClick(Sender: TObject);
    procedure miGotoLineClick(Sender: TObject);
    procedure miNewProjClick(Sender: TObject);
    procedure miOpenProjClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure miRedoClick(Sender: TObject);
    procedure miReplaceClick(Sender: TObject);
    procedure miSaveAllFilesClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miSaveFileClick(Sender: TObject);
    procedure miSearchResultClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure miTemplateClick(Sender: TObject);
    procedure miToJavaClick(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure miUpdateClick(Sender: TObject);
    procedure pgCodeCloseTabClicked(Sender: TObject);
    procedure tvProjectFilesClick(Sender: TObject);
  private
    FCurrentProjectName: string;
    FCurrentProjectPath: string;
    procedure codeJumpCallback(sender: TObject; path: string; method: string; typ: Integer);
    function IsPageExists(path: string): Integer;
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
  smaliCodeView, TextUtils, CodeUtils, ProjectUtils;

{ TFormMain }

procedure TFormMain.tvProjectFilesClick(Sender: TObject);
var
  node: TTreeNode;
  path: string;
  page: TSmaliCodeView;
  idx: Integer;
begin
  //
  node := tvProjectFiles.Selected;
  if (node <> nil) then begin
    if (node.ImageIndex = 1) then begin
      // expand node
      if (not node.HasChildren) then ProjectUtils.ExpandProjectNode(CurrentProjectPath, tvProjectFiles.Items, node);
    end else begin
      path:= node.Text;
      while (node.Parent <> nil) do begin
        path:= node.Parent.Text + '/' + path;
        node := node.Parent;
      end;
      // open file
      path:= ExtractFilePath(CurrentProjectPath) + path;
      idx := IsPageExists(path);
      if idx = -1 then begin
        if (path.EndsWith('.smali')) then begin
          page := TSmaliCodeView.Create(pgCode);
          page.Parent := pgCode;
          page.FileName:= path;
          page.OnCodeJump:=@codeJumpCallback;
          pgCode.TabIndex:= pgCode.PageCount - 1;
        end else begin
          // TODO: open other file
        end;
      end else begin
        pgCode.TabIndex:= idx;
      end;
    end;

  end;
end;

function TFormMain.IsPageExists(path: string): Integer;
var
  i: Integer;
begin
  // is page exists
  Result := -1;
  for i := 0 to pgCode.PageCount - 1 do begin
    if (pgCode.Pages[i] is TSmaliCodeView) then begin
      if (TSmaliCodeView(pgCode.Pages[i]).FileName = path) then begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

procedure TFormMain.codeJumpCallback(sender: TObject; path: string; method: string; typ: Integer);
var
  filePath: string;
  openPath: string;
  page: TSmaliCodeView;
  idx: Integer;
  ret: Boolean;
  smaliIdx: Integer = 1;
begin
  while True do begin
    if (smaliIdx = 1) then begin
      filePath:= ExtractFilePath(CurrentProjectPath) + 'smali/' + path;
    end else begin
      filePath:= ExtractFilePath(CurrentProjectPath) + 'smali_classes' + IntToStr(smaliIdx) + '/' + path;
    end;

    if (FileExists(filePath + '.smali')) then begin
      openPath:= filePath + '.smali';
      Break;
    end else if (FileExists(filePath + '.1.smali')) then begin
      openPath:= filePath + '.1.smali';
      Break;
    end;
    Inc(smaliIdx);
    if (smaliIdx > 9) then Break;
  end;

  if (FileExists(openPath)) then begin
    // open code jump
    idx := IsPageExists(openPath);
    if idx = -1 then begin
      page := TSmaliCodeView.Create(pgCode);
      page.Parent := pgCode;
      page.FileName:= openPath;
      page.OnCodeJump:=@codeJumpCallback;
      pgCode.TabIndex:= pgCode.PageCount - 1;
    end else begin
      pgCode.TabIndex:= idx;
    end;
  end else begin
    if (typ = 0) then MessageDlg('Hint', Format('Class "%s" is not included in the project.', [path]), mtInformation, [mbOK], 0);
  end;
  if (typ = 1) then begin
    ret := TSmaliCodeView(pgCode.ActivePage).FindMethodAndJump(method);
    if not ret then MessageDlg('Hint', Format('Method "%s" is not included in class "%s".', [method, path]), mtInformation, [mbOK], 0);
  end;
end;

procedure TFormMain.miOpenProjClick(Sender: TObject);
begin
  CurrentProjectPath:= ProjectUtils.OpenProject();
  if (CurrentProjectPath <> '') then begin
    ProjectUtils.LoadProject(CurrentProjectPath, tvProjectFiles.Items);
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

procedure TFormMain.miDocumentClick(Sender: TObject);
begin
  // TODO: documents
end;

procedure TFormMain.miExitClick(Sender: TObject);
begin
  // TODO: exit
end;

procedure TFormMain.miFindClick(Sender: TObject);
begin
  // find
  if (pgCode.ActivePage is TSmaliCodeView) then begin
    TSmaliCodeView(pgCode.ActivePage).Find();
  end;
end;

procedure TFormMain.miFindInFilesClick(Sender: TObject);
begin
  // TODO: file in files
end;

procedure TFormMain.miFindNextClick(Sender: TObject);
begin
  if (pgCode.ActivePage is TSmaliCodeView) then begin
    TSmaliCodeView(pgCode.ActivePage).FindNext();
  end;
end;

procedure TFormMain.miGotoLineClick(Sender: TObject);
begin
  // TODO: goto line
end;

procedure TFormMain.miNewProjClick(Sender: TObject);
begin
  // TODO: new project
end;

procedure TFormMain.miCopyClick(Sender: TObject);
begin
  if (pgCode.ActivePage is TSmaliCodeView) then begin
    TextUtils.Copy(TSmaliCodeView(pgCode.ActivePage).Editor);
  end;
end;

procedure TFormMain.miAboutClick(Sender: TObject);
begin
  // TODO: about
end;

procedure TFormMain.miClassIndexClick(Sender: TObject);
begin
  pnlClassIndex.Visible:= not pnlClassIndex.Visible;
  splRight.Visible:= pnlClassIndex.Visible;
  miClassIndex.Checked:= pnlClassIndex.Visible;
end;

procedure TFormMain.lstSearchResultKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then begin
    pnlSearch.Visible:= False;
    splBottom.Visible:= False;
    miSearchResult.Checked:= False;
  end;
end;

procedure TFormMain.miRedoClick(Sender: TObject);
begin
  if (pgCode.ActivePage is TSmaliCodeView) then begin
    TextUtils.Redo(TSmaliCodeView(pgCode.ActivePage).Editor);
  end;
end;

procedure TFormMain.miReplaceClick(Sender: TObject);
begin
  if (pgCode.ActivePage is TSmaliCodeView) then begin
    TSmaliCodeView(pgCode.ActivePage).Replace();
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

procedure TFormMain.miSearchResultClick(Sender: TObject);
begin
  pnlSearch.Visible:= not pnlSearch.Visible;
  splBottom.Visible:= pnlSearch.Visible;
  miSearchResult.Checked:= pnlSearch.Visible;
end;

procedure TFormMain.miSelectAllClick(Sender: TObject);
begin
  if (pgCode.ActivePage is TSmaliCodeView) then begin
    TextUtils.SelectAll(TSmaliCodeView(pgCode.ActivePage).Editor);
  end;
end;

procedure TFormMain.miTemplateClick(Sender: TObject);
begin
  // TODO: code template

end;

procedure TFormMain.miToJavaClick(Sender: TObject);
begin
  // TODO: smali to java

end;

procedure TFormMain.miUndoClick(Sender: TObject);
begin
  if (pgCode.ActivePage is TSmaliCodeView) then begin
    TextUtils.Undo(TSmaliCodeView(pgCode.ActivePage).Editor);
  end;
end;

procedure TFormMain.miUpdateClick(Sender: TObject);
begin
  // TODO: check update
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
  pnlClassIndex.Visible:= False;
  pnlSearch.Visible:= False;
  splRight.Visible:= False;
  splBottom.Visible:= False;
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

