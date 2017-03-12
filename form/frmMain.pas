unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, SmaliIdeAPI,
  ExtCtrls, ComCtrls, StdCtrls, frmBase, math, LCLType, SearchInFileUtils, CommandUtils, LCLIntf, process;

type
  { TFormMain }

  TFormMain = class(TFormBase)
    btnCancelsearch: TButton;
    btnCleanConsole: TButton;
    edtFilterClass: TEdit;
    imgIndex: TImageList;
    imgLst: TImageList;
    lblConsoleTitle: TLabel;
    lblSearchState: TLabel;
    lblSearchResult: TLabel;
    lstSearchResult: TListBox;
    miSSmali: TMenuItem;
    mmConsole: TMemo;
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
    pnlConsole: TPanel;
    pnlConsoleTitle: TPanel;
    pnlSearchOper: TPanel;
    pnlSearch: TPanel;
    pnlClassIndex: TPanel;
    pgCode: TPageControl;
    pnlProjectFiles: TPanel;
    popCodeFiles: TPopupMenu;
    splBottom: TSplitter;
    splConsole: TSplitter;
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
    tBtnOpenProject: TToolButton;
    tSp1: TToolButton;
    tBtnSave: TToolButton;
    tBtnSaveAs: TToolButton;
    tSp2: TToolButton;
    tBtnUndo: TToolButton;
    tBtnRedo: TToolButton;
    tSp3: TToolButton;
    tvProjectFiles: TTreeView;

    procedure btnCancelsearchClick(Sender: TObject);
    procedure btnCleanConsoleClick(Sender: TObject);
    procedure edtFilterClassChange(Sender: TObject);
    procedure lstSearchResultClick(Sender: TObject);
    procedure lstSearchResultKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    procedure miSSmaliClick(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure miUpdateClick(Sender: TObject);
    procedure pgCodeCloseTabClicked(Sender: TObject);
    procedure tvClassIndexClick(Sender: TObject);
    procedure tvProjectFilesClick(Sender: TObject);
    procedure tvProjectFilesMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FCurrentProjectName: string;
    FCurrentProjectPath: string;
    threadSearchInFile: TSearchInFileThread;
    procedure buildClassIndexCallback(sender: TObject; path: string; count: Integer);
    procedure buildClassIndexCompleteCallback(sender: TObject);
    procedure codeJumpCallback(sender: TObject; path: string; method: string; typ: Integer);
    procedure compileCallback(Sender: TObject; ACmdType: TCommandType;
      AOutput: string);
    procedure compileComplete(Sender: TObject; ACmdType: TCommandType;
      AParam: array of string);
    procedure decompileCallback(Sender: TObject; ACmdType: TCommandType; AOutput: string);
    procedure decompileComplete(Sender: TObject; ACmdType: TCommandType; AParam: array of string);
    procedure installFrameworkCallback(Sender: TObject; ACmdType: TCommandType;
      AOutput: string);
    procedure installFrameworkComplete(Sender: TObject; ACmdType: TCommandType;
      AParam: array of string);
    function IsPageExists(path: string): Integer;
    procedure onSearchInFileComplete(Sender: TObject);
    procedure onSearchInFileFound(Sender: TObject; AFilePath: string; AIndex: Integer; AShortcut: string);

    // config
    procedure loadShortcut();
    procedure OpenFileWithExternalEditor(path: string);
    procedure updateCheckCallback(Sender: TObject; AInfo: TUpdateInfo);

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
  smaliCodeView, TextUtils, CodeUtils, ProjectUtils, EncryptUtils, textCodeView, codeViewIntf, imageView,
  frmDecompile, frmAbout, frmSettings, config, frmUpdate, baseData, sdkCodeView;

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
        path:= node.Parent.Text + SPLIT + path;
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
          page.SetCodeTheme(GlobalConfig.CodeTheme);
          page.OnCodeJump:=@codeJumpCallback;
          pgCode.TabIndex:= pgCode.PageCount - 1;
        end else begin
          if (CodeUtils.IsTextFile(path)) then begin
            // open text file
            pageText := TTextCodeView.Create(pgCode);
            pageText.Parent := pgCode;
            pageText.ProjectPath:= CurrentProjectPath;
            pageText.FileName:= path;
            pageText.SetCodeTheme(GlobalConfig.CodeTheme);
            pgCode.TabIndex:= pgCode.PageCount - 1;
          end else if (CodeUtils.IsImageFile(path)) then begin
            // open image path
            pageImage := TImageCodeView.Create(pgCode);
            pageImage.Parent := pgCode;
            pageImage.FileName:= path;
            pgCode.TabIndex:= pgCode.PageCount - 1;
          end else begin
            // open other file
            OpenFileWithExternalEditor(path);
          end;
        end;
      end else begin
        pgCode.TabIndex:= idx;
      end;
    end;
  end;
end;

procedure TFormMain.tvProjectFilesMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TFormMain.onSearchInFileComplete(Sender: TObject);
begin
  // search completed
  lblSearchState.Caption:= 'Ready';
  threadSearchInFile := nil;
end;

procedure TFormMain.onSearchInFileFound(Sender: TObject; AFilePath: string; AIndex: Integer; AShortcut: string);
var
  s: string;
begin
  // found
  s := Format('[%d] %s  (%s)', [AIndex, AShortcut.Replace(#13, '').Replace(#10, '').Trim, AFilePath]);
  lstSearchResult.Items.Add(s);
  Application.ProcessMessages;
end;

procedure TFormMain.loadShortcut;
var
  i: Integer;
begin
  // load shortcut
  miNewClass.ShortCut:= GlobalConfig.NewClass;
  miNewInterfce.ShortCut:= GlobalConfig.NewInterface;
  miNewEnum.ShortCut:= GlobalConfig.NewEnum;
  miNewAnnotation.ShortCut:= GlobalConfig.NewAnnotation;
  miNewTextFile.ShortCut:= GlobalConfig.NewTextFile;
  miDeleteFile.ShortCut:= GlobalConfig.DeleteFile;
  miClassIndex.ShortCut:= GlobalConfig.ShowClassIndex;
  miSearchResult.ShortCut:= GlobalConfig.ShowSearchResult;
  miSSmali.ShortCut:= GlobalConfig.ShowSsmaliShortcut;
  miConsole.ShortCut:= GlobalConfig.ShowConsole;
  miCloseAllPages.ShortCut:= GlobalConfig.CloseAllPages;
  miCloseAllOtherPages.ShortCut:= GlobalConfig.CloseAllOtherPages;
  miDecompile.ShortCut:= GlobalConfig.Decompile;
  miCompile.ShortCut:= GlobalConfig.Compile;
  miInstallFramework.ShortCut:= GlobalConfig.InstallFramework;
  miSettings.ShortCut:= GlobalConfig.Settings;

  for i := 0 to pgCode.PageCount - 1 do if (pgCode.Pages[i] is ICodeViewIntf) then (pgCode.Page[i] as ICodeViewIntf).LoadShortcut();
end;

procedure TFormMain.OpenFileWithExternalEditor(path: string);
var
  ext: string;
  aeditor: string;
begin
  // open file with external editor
  ext := ExtractFileExt(path);
  ext := ext.Trim(['.']).ToLower;
  aeditor:= GlobalConfig.FileTypeEditor[ext];
  if (aeditor.Trim <> '') and (FileExists(aeditor)) then begin
    with TProcess.Create(nil) do begin
      Executable:= aeditor;
      Parameters.Add(path);
      Options:= [];
      ShowWindow:= swoShow;
      Execute;
      Free;
    end;
  end;
end;

procedure TFormMain.updateCheckCallback(Sender: TObject; AInfo: TUpdateInfo);
begin
  // get update info
  if (AInfo <> nil) then begin
    with TFormUpdate.Create(nil) do begin
      UpdateInfo := AInfo;
      ShowModal;
      Free;
    end;
  end;
end;

procedure TFormMain.codeJumpCallback(sender: TObject; path: string; method: string; typ: Integer);
var
  filePath: string;
  openPath: string;
  page: TSmaliCodeView;
  javaPage: TSDKJavaCodeView;
  idx: Integer;
  ret: Boolean;
  smaliIdx: Integer = 1;
begin
  while True do begin
    if (smaliIdx = 1) then begin
      filePath:= ExtractFilePath(CurrentProjectPath) + 'smali' + SPLIT + path;
    end else begin
      filePath:= ExtractFilePath(CurrentProjectPath) + 'smali_classes' + IntToStr(smaliIdx) + SPLIT + path;
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
      page.SetCodeTheme(GlobalConfig.CodeTheme);
      page.OnCodeJump:=@codeJumpCallback;
      pgCode.TabIndex:= pgCode.PageCount - 1;
    end else begin
      pgCode.TabIndex:= idx;
    end;
  end else begin
    openPath:= CodeUtils.FindFileInAndroidSDK(path);
    if (FileExists(openPath)) then begin
      typ := 3; // avoid find method
      idx := IsPageExists(openPath);
      if (idx = -1) then begin
        javaPage:= TSDKJavaCodeView.Create(pgCode);
        javaPage.Parent := pgCode;
        javaPage.FileName:= openPath;
        javaPage.SetCodeTheme(GlobalConfig.CodeTheme);
        pgCode.TabIndex:= pgCode.PageCount - 1;
      end else begin
        pgCode.TabIndex:= idx;
      end;
    end else begin
      if (typ = 0) then MessageDlg('Hint', Format('Class "%s" is not included in the project.', [path]), mtInformation, [mbOK], 0);
    end;
  end;
  if (typ = 1) then begin
    ret := TSmaliCodeView(pgCode.ActivePage).FindMethodAndJump(method);
    if not ret then MessageDlg('Hint', Format('Method "%s" is not included in class "%s".', [method, path]), mtInformation, [mbOK], 0);
  end;
end;

procedure TFormMain.compileCallback(Sender: TObject; ACmdType: TCommandType;
  AOutput: string);
begin
  if (AOutput.Trim <> '') then mmConsole.Lines.Add(AOutput);
end;

procedure TFormMain.compileComplete(Sender: TObject; ACmdType: TCommandType;
  AParam: array of string);
begin
  mmConsole.Lines.Add(#13#10'Completed.');
  mmConsole.Lines.Add(Format('APK file is located in "%s"', [AParam[0]]));
end;

procedure TFormMain.decompileCallback(Sender: TObject; ACmdType: TCommandType; AOutput: string);
begin
  // decompile callback
  if (AOutput.Trim <> '') then mmConsole.Lines.Add(AOutput);
end;

procedure TFormMain.decompileComplete(Sender: TObject; ACmdType: TCommandType;
  AParam: array of string);
begin
  if (ACmdType = ctDecompile) then begin
    mmConsole.Lines.Add(#13#10'Completed.');
    // open project after decompile
    CurrentProjectPath:= AParam[0];
    tvProjectFiles.Items.Clear;
    ProjectUtils.LoadProject(CurrentProjectPath, tvProjectFiles.Items);
    CodeUtils.ThreadBuildClassIndex(CurrentProjectPath, @buildClassIndexCallback, @buildClassIndexCompleteCallback);
  end;
end;

procedure TFormMain.installFrameworkCallback(Sender: TObject;
  ACmdType: TCommandType; AOutput: string);
begin
  if (AOutput.Trim <> '') then mmConsole.Lines.Add(AOutput);
end;

procedure TFormMain.installFrameworkComplete(Sender: TObject;
  ACmdType: TCommandType; AParam: array of string);
begin
  mmConsole.Lines.Add(#13#10'Completed.');
end;

procedure TFormMain.buildClassIndexCallback(sender: TObject; path: string; count: Integer);
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
  tvClassIndex.Items.Clear;
  sbMain.Panels[0].Text:= 'Ready';
  indexPath := ExtractFilePath(ParamStr(0)) + 'index' + SPLIT + md5EncryptString(CurrentProjectPath) + SPLIT + 'index';
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
    tvProjectFiles.Items.Clear;
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
var
  apkPath: string;
  outputPath: string;
  isNoRes: Boolean;
  isNoSrc: Boolean;
begin
  with TFormDecompile.Create(nil) do begin
    if ShowModal = mrOK then begin
      apkPath:= edtApkPath.Text;
      outputPath:= edtOutputPath.Text;
      isNoRes:= chkNoRes.Checked;
      isNoSrc:= chkNoSrc.Checked;
      // decompile package

      CodeUtils.DecompilePackage(apkPath, outputPath, isNoRes, isNoSrc, @decompileCallback, @decompileComplete);

      pnlConsole.Visible:= True;
      splConsole.Visible:= True;
      miConsole.Checked:= True;

    end;
    Free;
  end;
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
  if (node = nil) then Exit;
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
  // documents
  LCLIntf.OpenURL('https://github.com/rarnu/smali-ide/blob/master/README.md');
end;

procedure TFormMain.miExitClick(Sender: TObject);
begin
  // exit
  Close;
end;

procedure TFormMain.miFindClick(Sender: TObject);
begin
  // find
  if (pgCode.ActivePage is ICodeViewIntf) then begin
    (pgCode.ActivePage as ICodeViewIntf).Find();
  end;
end;

procedure TFormMain.miFindInFilesClick(Sender: TObject);
var
  key: string;
begin
  // file in files
  key := InputBox('Find in Files', 'Input keyword', '').Trim;
  if (key = '') then Exit;
  if (threadSearchInFile = nil) then begin
    threadSearchInFile:= TSearchInFileThread.Create(ExtractFilePath(CurrentProjectPath), key);
    threadSearchInFile.OnTerminate:= @onSearchInFileComplete;
    threadSearchInFile.OnSearchInFileFound:= @onSearchInFileFound;
    threadSearchInFile.Start;

    pnlSearch.Visible:= True;
    splBottom.Visible:= pnlSearch.Visible;
    miSearchResult.Checked:= pnlSearch.Visible;
    lstSearchResult.Items.Clear;
    lblSearchState.Caption:= 'Searching';

  end else begin
    MessageDlg('Hint', 'Last search is not completed.', mtInformation, [mbOK], 0);
  end;
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
var
  frameworkPath: string;
begin
  // install framework
  with TOpenDialog.Create(nil) do begin
    Filter:= 'jar files|*.jar|apk files|*.apk';
    if Execute then begin
      frameworkPath:= FileName;
      pnlConsole.Visible:= True;
      splConsole.Visible:= True;
      miConsole.Visible:= True;
      CodeUtils.InstallFramework(frameworkPath, @installFrameworkCallback, @installFrameworkComplete);
    end;
    Free;
  end;
end;

procedure TFormMain.miNewAnnotationClick(Sender: TObject);
var
  node: TTreeNode;
  path: string;
begin
  // new annotation
  node := tvProjectFiles.Selected;
  if (node = nil) then Exit;
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
  if (node = nil) then Exit;
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
  if (node = nil) then Exit;
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
  if (node = nil) then Exit;
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
  if (node = nil) then Exit;
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
  // about
  with TFormAbout.Create(nil) do begin
    ShowModal;
    Free;
  end;
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
var
  path: string;
begin
  // compile package
  path := ExtractFilePath(CurrentProjectPath);
  if (path.Trim = '') then Exit;
  pnlConsole.Visible:= True;
  splConsole.Visible:= True;
  miConsole.Checked:= True;
  CodeUtils.CompilePackage(path, @compileCallback, @compileComplete);
end;

procedure TFormMain.miConsoleClick(Sender: TObject);
begin
  // show /hide console
  pnlConsole.Visible:= not pnlConsole.Visible;
  splConsole.Visible:= pnlConsole.Visible;
  miConsole.Checked:= pnlConsole.Visible;
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

procedure TFormMain.lstSearchResultClick(Sender: TObject);
var
  idx: Integer;
  s: string;
  APath: string;
  AStart: Integer;
  pgIdx: Integer;
  page: TSmaliCodeView;
  pageText: TTextCodeView;
begin
  // click search result
  idx := lstSearchResult.ItemIndex;
  if (idx = -1) then Exit;
  s := lstSearchResult.Items[idx];

  APath:= s.Substring(s.LastIndexOf('(')).TrimLeft(['(']).TrimRight([')']);
  AStart:= StrToInt(s.Substring(0, s.IndexOf(']')).TrimLeft('[').TrimRight(']'));
  pgIdx:= IsPageExists(APath);
  if pgIdx = -1 then begin
    if (APath.EndsWith('.smali')) then begin
      page := TSmaliCodeView.Create(pgCode);
      page.Parent := pgCode;
      page.ProjectPath:= CurrentProjectPath;
      page.FileName:= APath;
      page.SetCodeTheme(GlobalConfig.CodeTheme);
      page.OnCodeJump:=@codeJumpCallback;
      pgCode.TabIndex:= pgCode.PageCount - 1;
    end else begin
      if (CodeUtils.IsTextFile(APath)) then begin
        pageText := TTextCodeView.Create(pgCode);
        pageText.Parent := pgCode;
        pageText.ProjectPath:= CurrentProjectPath;
        pageText.FileName:= APath;
        pageText.SetCodeTheme(GlobalConfig.CodeTheme);
        pgCode.TabIndex:= pgCode.PageCount - 1;
      end
    end;
  end else begin
    pgCode.TabIndex:= pgIdx;
  end;
  (pgCode.ActivePage as ICodeViewIntf).GotoPosition(AStart);
  (pgCode.ActivePage as ICodeViewIntf).FocusEditor();
end;

procedure TFormMain.btnCancelsearchClick(Sender: TObject);
begin
  threadSearchInFile.AbortSearch();
end;

procedure TFormMain.btnCleanConsoleClick(Sender: TObject);
begin
  mmConsole.Lines.Clear;
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
  // settings
  with TFormSettings.Create(nil) do begin
    ShowModal;
    Free;
  end;
  loadShortcut();
end;

procedure TFormMain.miSSmaliClick(Sender: TObject);
var
  i: Integer;
begin
  miSSmali.Checked:= not miSSmali.Checked;
  GlobalConfig.ShowSSmali:= miSSmali.Checked;
  for i := 0 to pgCode.PageCount - 1 do if (pgCode.Pages[i] is TSmaliCodeView) then TSmaliCodeView(pgCode.Pages[i]).ShowSSmali(miSSmali.Checked);
end;

procedure TFormMain.miUndoClick(Sender: TObject);
begin
  if (pgCode.ActivePage is ICodeViewIntf) then begin
    TextUtils.Undo((pgCode.ActivePage as ICodeViewIntf).GetEditor());
  end;
end;

procedure TFormMain.miUpdateClick(Sender: TObject);
begin
  // check update
  SmaliIdeAPI.UpdateCheck('main', @updateCheckCallback);
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
      Result.SetCodeTheme(GlobalConfig.CodeTheme);
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
  pnlConsole.Visible:= False;
  splConsole.Visible:= False;
end;

procedure TFormMain.InitEvents;
begin
  //
end;

procedure TFormMain.InitLogic;
begin
  loadShortcut();
  miSSmali.Checked:= GlobalConfig.ShowSSmali;
end;

end.

