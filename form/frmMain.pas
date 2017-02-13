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
    imgIndex: TImageList;
    imgLst: TImageList;
    lstSearchResult: TListBox;
    miCloseAllOtherPages: TMenuItem;
    miCloseAllPages: TMenuItem;
    mm12: TMenuItem;
    miNewAnnotation: TMenuItem;
    miNewEnum: TMenuItem;
    miDeleteFile: TMenuItem;
    mm11: TMenuItem;
    miNewTextFile: TMenuItem;
    miNewInterfce: TMenuItem;
    miNewClass: TMenuItem;
    miConsole: TMenuItem;
    mm10: TMenuItem;
    miInstallFramework: TMenuItem;
    mm9: TMenuItem;
    miCompile: TMenuItem;
    miDecompile: TMenuItem;
    miPackage: TMenuItem;
    mm8: TMenuItem;
    miSettings: TMenuItem;
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
    popCodeFiles: TPopupMenu;
    splBottom: TSplitter;
    splRight: TSplitter;
    splProjectFiles: TSplitter;
    sbMain: TStatusBar;
    tbMain: TToolBar;
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
    procedure edtFilterClassChange(Sender: TObject);
    procedure lstSearchResultKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure miAboutClick(Sender: TObject);
    procedure miClassIndexClick(Sender: TObject);
    procedure miCloseAllOtherPagesClick(Sender: TObject);
    procedure miCloseAllPagesClick(Sender: TObject);
    procedure miCompileClick(Sender: TObject);
    procedure miConsoleClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miDecompileClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miDeleteFileClick(Sender: TObject);
    procedure miDocumentClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miFindClick(Sender: TObject);
    procedure miFindInFilesClick(Sender: TObject);
    procedure miFindNextClick(Sender: TObject);
    procedure miGotoLineClick(Sender: TObject);
    procedure miInstallFrameworkClick(Sender: TObject);
    procedure miNewAnnotationClick(Sender: TObject);
    procedure miNewClassClick(Sender: TObject);
    procedure miNewEnumClick(Sender: TObject);
    procedure miNewInterfceClick(Sender: TObject);
    procedure miNewTextFileClick(Sender: TObject);
    procedure miOpenProjClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure miRedoClick(Sender: TObject);
    procedure miReplaceClick(Sender: TObject);
    procedure miSaveAllFilesClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miSaveFileClick(Sender: TObject);
    procedure miSearchResultClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure miSettingsClick(Sender: TObject);
    procedure miTemplateClick(Sender: TObject);
    procedure miToJavaClick(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure miUpdateClick(Sender: TObject);
    procedure pgCodeCloseTabClicked(Sender: TObject);
    procedure tvClassIndexClick(Sender: TObject);
    procedure tvProjectFilesClick(Sender: TObject);
    procedure tvProjectFilesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FCurrentProjectName: string;
    FCurrentProjectPath: string;
    procedure buildClassIndexCallback(sender: TObject; path: string;
      count: Integer);
    procedure buildClassIndexCompleteCallback(sender: TObject);
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
  smaliCodeView, TextUtils, CodeUtils, ProjectUtils, EncryptUtils, textCodeView, codeViewIntf, imageView;

{ TFormMain }

procedure TFormMain.tvProjectFilesClick(Sender: TObject);
var
  node: TTreeNode;
  path: string;
  page: TSmaliCodeView;
  pageText: TTextCodeView;
  pageImage: TImageCodeView;
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
          page.ProjectPath:= CurrentProjectPath;
          page.FileName:= path;
          page.OnCodeJump:=@codeJumpCallback;
          pgCode.TabIndex:= pgCode.PageCount - 1;
        end else begin
          // open other file
          if (CodeUtils.IsTextFile(path)) then begin
            pageText := TTextCodeView.Create(pgCode);
            pageText.Parent := pgCode;
            pageText.ProjectPath:= CurrentProjectPath;
            pageText.FileName:= path;
            pgCode.TabIndex:= pgCode.PageCount - 1;
          end else if (CodeUtils.IsImageFile(path)) then begin
            // open image path
            pageImage := TImageCodeView.Create(pgCode);
            pageImage.Parent := pgCode;
            pageImage.FileName:= path;
            pgCode.TabIndex:= pgCode.PageCount - 1;
          end;
        end;
      end else begin
        pgCode.TabIndex:= idx;
      end;
    end;

  end;
end;

procedure TFormMain.tvProjectFilesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  node: TTreeNode;
  p: TPoint;
begin
  if (Button = mbRight) then begin
    node := tvProjectFiles.GetNodeAt(X, Y);
    if (node <> nil) then begin
      tvProjectFiles.Selected := node;
      p := ClientToScreen(TPoint.Create(X, Y + node.Height));
      popCodeFiles.PopUp(p.X, p.y);
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
    if (pgCode.Pages[i] is ICodeViewIntf) then begin
      if ((pgCode.Pages[i] as ICodeViewIntf).GetFileName() = path) then begin
        Result := i;
        Break;
      end;
    end else if (pgCode.Pages[i] is TImageCodeView) then begin
      if (TImageCodeView(pgCode.Pages[i]).FileName = path) then begin
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
      page.ProjectPath:= CurrentProjectPath;
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

procedure TFormMain.buildClassIndexCallback(sender: TObject; path: string;
  count: Integer);
begin
  sbMain.Panels[0].Text:= Format('Indexing: [%d] %s', [count, path]);
  Application.ProcessMessages;
end;

procedure TFormMain.buildClassIndexCompleteCallback(sender: TObject);
var
  indexPath: string;
  list: TStringList;
  s: string;
begin
  sbMain.Panels[0].Text:= 'Ready';
  indexPath := ExtractFilePath(ParamStr(0)) + 'index/' + md5EncryptString(CurrentProjectPath) + '/index';
  if (FileExists(indexPath)) then begin
    tvClassIndex.BeginUpdate;
    list := TStringList.Create;
    list.LoadFromFile(indexPath);
    for s in list do begin
      if (s.Trim <> '') then begin
        tvClassIndex.Items.Add(nil, s.Trim);
      end;
    end;
    list.Free;
    tvClassIndex.EndUpdate;
  end;
end;

procedure TFormMain.miOpenProjClick(Sender: TObject);
begin
  CurrentProjectPath:= ProjectUtils.OpenProject();
  if (CurrentProjectPath <> '') then begin
    ProjectUtils.LoadProject(CurrentProjectPath, tvProjectFiles.Items);
    CodeUtils.ThreadBuildClassIndex(CurrentProjectPath, @buildClassIndexCallback, @buildClassIndexCompleteCallback);
  end;
end;

procedure TFormMain.miPasteClick(Sender: TObject);
begin
  if (pgCode.ActivePage is ICodeViewIntf) then begin
    TextUtils.Paste((pgCode.ActivePage as ICodeViewIntf).GetEditor());
  end;
end;

procedure TFormMain.miCutClick(Sender: TObject);
begin
  if (pgCode.ActivePage is ICodeViewIntf) then begin
    TextUtils.Cut((pgCode.ActivePage as ICodeViewIntf).GetEditor());
  end;
end;

procedure TFormMain.miDecompileClick(Sender: TObject);
begin
  // TODO: decompile package
end;

procedure TFormMain.miDeleteClick(Sender: TObject);
begin
  if (pgCode.ActivePage is ICodeViewIntf) then begin
    TextUtils.Delete((pgCode.ActivePage as ICodeViewIntf).GetEditor());
  end;
end;

procedure TFormMain.miDeleteFileClick(Sender: TObject);
var
  node: TTreeNode;
  path: string;
begin
  node := tvProjectFiles.Selected;
  path := CodeUtils.NodeToPath(CurrentProjectPath, node);
  if (DirectoryExists(path)) then begin
    if MessageDlg('Hint', 'Delete whole folder? You cannot undo this operation.', mtConfirmation, mbOKCancel, 0) = mrOK then begin
       // delete directory
      if DeleteDirectory(path, False) then begin
        tvProjectFiles.Items.Delete(node);
      end else begin
        MessageDlg('Error', 'Delete folder failed.', mtError, [mbOK], 0);
      end;
    end;
  end else begin
    if MessageDlg('Hint', 'Delete the file? You cannot undo this operation.', mtConfirmation, mbOKCancel, 0) = mrOK then begin
      // delete file
      if DeleteFile(path) then begin
        tvProjectFiles.Items.Delete(node);
      end else begin
        MessageDlg('Error', 'Delete file failed', mtError, [mbOK], 0);
      end;
    end;
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
  if (pgCode.ActivePage is ICodeViewIntf) then begin
    (pgCode.ActivePage as ICodeViewIntf).Find();
  end;
end;

procedure TFormMain.miFindInFilesClick(Sender: TObject);
begin
  // TODO: file in files
end;

procedure TFormMain.miFindNextClick(Sender: TObject);
begin
  if (pgCode.ActivePage is ICodeViewIntf) then begin
    (pgCode.ActivePage as ICodeViewIntf).FindNext();
  end;
end;

procedure TFormMain.miGotoLineClick(Sender: TObject);
var
  line: string;
  linenum: Integer;
begin
  // goto line
  if (pgCode.ActivePage is ICodeViewIntf) then begin
    line := InputBox('Goto Line', 'Input a line number to goto:', '');
    linenum:= StrToIntDef(line, -1);
    if (linenum <> -1) then begin
      (pgCode.ActivePage as ICodeViewIntf).GotoLine(linenum);
    end;
  end;
end;

procedure TFormMain.miInstallFrameworkClick(Sender: TObject);
begin
  // TODO: install framework
end;

procedure TFormMain.miNewAnnotationClick(Sender: TObject);
var
  node: TTreeNode;
  path: string;
begin
  // new annotation
  node := tvProjectFiles.Selected;
  path := CodeUtils.NodeToPath(CurrentProjectPath, node);
  CodeUtils.NewAnnotation(CurrentProjectPath, path, tvProjectFiles.Items, node, pgCode, @codeJumpCallback);
end;

procedure TFormMain.miNewClassClick(Sender: TObject);
var
  node: TTreeNode;
  path: string;
begin
  // new class
  node := tvProjectFiles.Selected;
  path := CodeUtils.NodeToPath(CurrentProjectPath, node);
  CodeUtils.NewClass(CurrentProjectPath, path, tvProjectFiles.Items, node, pgCode, @codeJumpCallback);
end;

procedure TFormMain.miNewEnumClick(Sender: TObject);
var
  node: TTreeNode;
  path: string;
begin
  // new enum
  node := tvProjectFiles.Selected;
  path := CodeUtils.NodeToPath(CurrentProjectPath, node);
  CodeUtils.NewEnum(CurrentProjectPath, path, tvProjectFiles.Items, node, pgCode, @codeJumpCallback);
end;

procedure TFormMain.miNewInterfceClick(Sender: TObject);
var
  node: TTreeNode;
  path: string;
begin
  // new interface
  node := tvProjectFiles.Selected;
  path := CodeUtils.NodeToPath(CurrentProjectPath, node);
  CodeUtils.NewInterface(CurrentProjectPath, path, tvProjectFiles.Items, node, pgCode, @codeJumpCallback);
end;

procedure TFormMain.miNewTextFileClick(Sender: TObject);
var
  node: TTreeNode;
  path: string;
begin
  // new text file
  node := tvProjectFiles.Selected;
  path := CodeUtils.NodeToPath(CurrentProjectPath, node);
  CodeUtils.NewTextFile(CurrentProjectPath, path, tvProjectFiles.Items, node, pgCode);
end;

procedure TFormMain.miCopyClick(Sender: TObject);
begin
  if (pgCode.ActivePage is ICodeViewIntf) then begin
    TextUtils.Copy((pgCode.ActivePage as ICodeViewIntf).GetEditor());
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

procedure TFormMain.miCloseAllOtherPagesClick(Sender: TObject);
var
  i: Integer;
  idx: Integer;
begin
  // close all other pages
  idx:= pgCode.TabIndex;
  for i := pgCode.PageCount - 1 downto 0 do begin
    if (i <> idx) then begin
      pgCode.Pages[i].Free;
    end;
  end;
  pgCode.TabIndex:= 0;
end;

procedure TFormMain.miCloseAllPagesClick(Sender: TObject);
var
  i: Integer;
begin
  // close all pages
  for i := pgCode.PageCount - 1 downto 0 do begin
    pgCode.Pages[i].Free;
  end;
  pgCode.TabIndex:= -1;
end;

procedure TFormMain.miCompileClick(Sender: TObject);
begin
  // TODO: compile package

end;

procedure TFormMain.miConsoleClick(Sender: TObject);
begin
  // TODO: show /hide console

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

procedure TFormMain.edtFilterClassChange(Sender: TObject);
var
  filter: string;
  i: Integer;
begin
  filter:= edtFilterClass.Text;
  for i := 0 to tvClassIndex.Items.Count - 1 do begin
    if (filter.Trim = '') then begin
      tvClassIndex.Items.Item[i].Visible := True;
    end else begin
      tvClassIndex.Items.Item[i].Visible := tvClassIndex.Items.Item[i].Text.Contains(filter);
    end;
  end;
end;

procedure TFormMain.miRedoClick(Sender: TObject);
begin
  if (pgCode.ActivePage is ICodeViewIntf) then begin
    TextUtils.Redo((pgCode.ActivePage as ICodeViewIntf).GetEditor());
  end;
end;

procedure TFormMain.miReplaceClick(Sender: TObject);
begin
  if (pgCode.ActivePage is ICodeViewIntf) then begin
    (pgCode.ActivePage as ICodeViewIntf).Replace();
  end;
end;

procedure TFormMain.miSaveAllFilesClick(Sender: TObject);
var
  i: Integer;
begin
  // save all files
  for i := 0 to pgCode.PageCount - 1 do begin
    if (pgCode.Pages[i] is ICodeViewIntf) then begin
      (pgCode.Pages[i] as ICodeViewIntf).Save();
    end;
  end;
end;

procedure TFormMain.miSaveAsClick(Sender: TObject);
begin
  if (pgCode.ActivePage is ICodeViewIntf) then begin
    (pgCode.ActivePage as ICodeViewIntf).SaveAs();
  end;
end;

procedure TFormMain.miSaveFileClick(Sender: TObject);
begin
  if (pgCode.ActivePage is ICodeViewIntf) then begin
    (pgCode.ActivePage as ICodeViewIntf).Save();
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
  if (pgCode.ActivePage is ICodeViewIntf) then begin
    TextUtils.SelectAll((pgCode.ActivePage as ICodeViewIntf).GetEditor());
  end;
end;

procedure TFormMain.miSettingsClick(Sender: TObject);
begin
  // TODO: settings

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
  if (pgCode.ActivePage is ICodeViewIntf) then begin
    TextUtils.Undo((pgCode.ActivePage as ICodeViewIntf).GetEditor());
  end;
end;

procedure TFormMain.miUpdateClick(Sender: TObject);
begin
  // TODO: check update

end;

procedure TFormMain.pgCodeCloseTabClicked(Sender: TObject);
var
  idx: Integer;
  tab: ICodeViewIntf;
begin
  idx := pgCode.TabIndex;
  if (Sender is ICodeViewIntf) then begin
    tab := (Sender as ICodeViewIntf);
    if (tab.QueryClose()) then begin
      tab.Free;
    end;
  end else begin
    TTabSheet(Sender).Free;
  end;
  idx := ifthen(idx > 0, idx - 1, 0);
  pgCode.TabIndex:= idx;
end;

procedure TFormMain.tvClassIndexClick(Sender: TObject);
var
  node: TTreeNode;
  path: string;
  page: TSmaliCodeView;

  function OpenNewPage(p: string): TSmaliCodeView;
  var
    idx: Integer;
  begin
    idx:= IsPageExists(p);
    if (idx = -1) then begin
      Result := TSmaliCodeView.Create(pgCode);
      Result.Parent := pgCode;
      Result.ProjectPath:= CurrentProjectPath;
      Result.FileName:= p;
      Result.OnCodeJump:= @codeJumpCallback;
      pgCode.TabIndex:= pgCode.PageCount - 1;
    end else begin
      pgCode.TabIndex:= idx;
      Result := TSmaliCodeView(pgCode.Pages[idx]);
    end;
  end;

begin
  node := tvClassIndex.Selected;
  if (node = nil) then Exit;
  path := CodeUtils.ClassIndexToFilePath(CurrentProjectPath, node.Text);
  if (path.Trim <> '') then begin
    page := OpenNewPage(path);
    CodeUtils.BuildMethodIndex(CurrentProjectPath, path, tvClassIndex.Items, node);
  end else begin
    if (node.Parent <> nil) then begin
      path := CodeUtils.ClassIndexToFilePath(CurrentProjectPath, node.Parent.Text);
      if (path.Trim <> '') then begin
        page := OpenNewPage(path);
        page.FindMethodAndJump(node.Text);
      end;
    end;
  end;
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

