unit CodeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, lockbox, Dialogs, codeViewIntf, CommandUtils, strutils;

type

  TOnBuildIndexCallback = procedure (sender: TObject; path: string; count: Integer) of object;
  TOnBuildIndexCompleteCallback = procedure (sender: TObject) of object;

  { TBuildClassIndexThread }

  TBuildClassIndexThread = class(TThread)
  private
    FOnBuildIndexCallback: TOnBuildIndexCallback;
    FOnBuildIndexCompleteCallback: TOnBuildIndexCompleteCallback;
    FPath: string;
    FCount: Integer;
    FIndexFile: string;
    FClassList: TStringList;
    procedure SendSyncCallback();
    procedure BuildClassIndex(projectPath: string);
    procedure buildIndexTerminate(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(path: string);
    destructor Destroy; override;
  published
    property OnBuildIndexCallback: TOnBuildIndexCallback read FOnBuildIndexCallback write FOnBuildIndexCallback;
    property OnBuildIndexCompleteCallback: TOnBuildIndexCompleteCallback read FOnBuildIndexCompleteCallback write FOnBuildIndexCompleteCallback;
  end;

function FullPathToClassPath(path: string): string;
procedure ThreadBuildClassIndex(projectPath: string; callback: TOnBuildIndexCallback; complete: TOnBuildIndexCompleteCallback);
procedure BuildMethodIndex(projectPath: string; classPath: string; root: TTreeNodes; parent: TTreeNode);
procedure BuildMethodIndex(projectPath: string; classPath: string);
function LoadMethodIndex(projectPath: string; classPath: string): string;
function ClassIndexToFilePath(projectPath: string; indexPkg: string): string;
function ConvertSmaliToJava(projectPath: string; path: string): string;
function NodeToPath(projectPath: string; node: TTreeNode): string;

function IsTextFile(path: string): Boolean;
function IsImageFile(path: string): Boolean;

procedure NewClass(projectPath: string; filePath: string; root: TTreeNodes; node: TTreeNode; pageControl: TPageControl; onCodeJump: TOnCodeJump);
procedure NewInterface(projectPath: string; filePath: string; root: TTreeNodes; node: TTreeNode; pageControl: TPageControl; onCodeJump: TOnCodeJump);
procedure NewEnum(projectPath: string; filePath: string; root: TTreeNodes; node: TTreeNode; pagecontrol: TPageControl; onCodeJump: TOnCodeJump);
procedure NewAnnotation(projectPath: string; filePath: string; root: TTreeNodes; node: TTreeNode; pagecontrol: TPageControl; onCodeJump: TOnCodeJump);
procedure NewTextFile(projectPath: string; filePath: string; root: TTreeNodes; node: TTreeNode; pageControl: TPageControl);

procedure DecompilePackage(AAPkPath: string; AOutputPath: string; AIsNoRes: Boolean; AISNoSrc: Boolean; callback: TOnCommandOutput; complete: TOnCommandComplete);
procedure CompilePackage(AProjectPath: string; callback: TOnCommandOutput; complete: TOnCommandComplete);
procedure InstallFramework(AFrameworkPath: string; callback: TOnCommandOutput; complete: TOnCommandComplete);

function FindFileInAndroidSDK(APath: string): string;
function FindSsmaliFile(AProjectPath: string; classPath: string): string;

implementation

uses
  EncryptUtils, baseData,smaliCodeView, textCodeView, config, frmInputBox;

procedure TBuildClassIndexThread.SendSyncCallback;
begin
  if (Assigned(FOnBuildIndexCallback)) then begin
    FOnBuildIndexCallback(Self, FIndexFile, FCount);
  end;
end;

function FullPathToClassPath(path: string): string;
var
  i: Integer;
  ret: string = '';
begin
  for i := 1 to 100 do begin
    if (i = 1) then begin
      if (path.Contains(SPLIT + 'smali' + SPLIT)) then begin
        ret := path.Substring(path.IndexOf(SPLIT + 'smali' + SPLIT) + 7);
        break;
      end;
    end else begin
      if (path.Contains(SPLIT + 'smali_classes' + IntToStr(i) + SPLIT)) then begin
        ret := path.Substring(path.IndexOf(SPLIT + 'smali_classes' + IntToStr(i) + SPLIT) + 16);
        break;
      end;
    end;
  end;
  if (ret <> '') then begin
    ret := ret.Substring(0, ret.IndexOf('.smali'));
  end;
  if (ret.EndsWith('.1')) then begin
    ret := ret.Substring(0, ret.Length - 2);
  end;
  ret := ret.Replace(SPLIT, '.', [rfReplaceAll, rfIgnoreCase]);
  Result := ret;
end;

procedure TBuildClassIndexThread.BuildClassIndex(projectPath: string);
var
  basePath: string;
  src: TSearchRec;
  fp: string;
begin
  if (projectPath.EndsWith('.yml')) then begin
    basePath:= ExtractFilePath(projectPath);
  end else begin
    basePath:= projectPath;
  end;
  if (FindFirst(basePath + '*', faAnyFile, src) = 0) then begin
    repeat
      if (src.Name = '.') or (src.Name = '..') then begin
        Continue;
      end;
      if (DirectoryExists(basePath + src.Name)) then begin
        BuildClassIndex(basePath + src.Name + SPLIT);
      end else begin
        if (string(src.Name).EndsWith('.smali')) then begin
          fp := FullPathToClassPath(basePath + src.Name);
          if (fp.Trim <> '') then FClassList.Add(fp);
          if (Assigned(FOnBuildIndexCallback)) then begin
            Inc(FCount);
            FIndexFile:= src.Name;
            if ((FCount mod 1000) = 0) then Synchronize(@SendSyncCallback);
          end;
        end;
      end;
    until FindNext(src) <> 0;
    FindClose(src);
  end;
end;

procedure TBuildClassIndexThread.buildIndexTerminate(Sender: TObject);
var
  indexPath: string;
begin
  indexPath := ExtractFilePath(ParamStr(0)) + 'index' + SPLIT + md5EncryptString(FPath);
  ForceDirectories(indexPath);
  if (not FileExists(indexPath + SPLIT + 'index')) then begin
    FClassList.SaveToFile(indexPath + SPLIT + 'index');
  end;
  if (Assigned(FOnBuildIndexCompleteCallback)) then begin
    FOnBuildIndexCompleteCallback(Self);
  end;
end;

procedure ThreadBuildClassIndex(projectPath: string; callback: TOnBuildIndexCallback; complete: TOnBuildIndexCompleteCallback);
begin
  with TBuildClassIndexThread.Create(projectPath) do begin
    OnBuildIndexCallback:= callback;
    OnBuildIndexCompleteCallback:= complete;
    Start;
  end;
end;

function ExtractField(str: string): string;
var
  n: string;
begin
  // .field private static final TAG:Ljava/lang/String;=
  // .field private static final TAG:Ljava/lang/String;
  if (str.Contains('=')) then begin
    n := str.Substring(0, str.LastIndexOf('=')).Trim;
    Result := n.Substring(n.LastIndexOf(' ')).Trim;
  end else begin
    Result := str.Substring(str.LastIndexOf(' ')).Trim;
  end;
end;

function ExtractMethod(str: string): string;
begin
  // .method public constructor <init>()V
  //.method static synthetic a(Lcom/android/updater/ActivityForDialog;Lcom/android/updater/A;)Lcom/android/updater/A;
  Result := str.Substring(str.LastIndexOf(' ')).Trim;
end;

procedure BuildMethodIndex(projectPath: string; classPath: string; root: TTreeNodes; parent: TTreeNode);
var
  savePath: string;
  i: Integer;
  list: TStringList;
  n: TTreeNode;
  ex: string;
begin
  // build method index
  savePath:= ExtractFilePath(ParamStr(0)) + 'index' + SPLIT + md5EncryptString(projectPath) + SPLIT + 'class' + SPLIT;
  ForceDirectories(savePath);
  savePath += md5EncryptString(classPath);
  list := TStringList.Create;
  if (not FileExists(savePath)) then begin
    with TStringList.Create do begin
      LoadFromFile(classPath);
      for i := 0 to Count - 1 do begin
        if (Strings[i].StartsWith('.field')) then begin
          ex := ExtractField(Strings[i]);
          if (ex.Trim <> '') then list.Add(ex);
        end else if (Strings[i].StartsWith('.method')) then begin
          ex := ExtractMethod(Strings[i]);
          if (ex.Trim <> '') then list.Add(ex);
        end;
      end;
      Free;
    end;
    list.SaveToFile(savePath);
  end else begin
    list.LoadFromFile(savePath);
  end;
  if (root <> nil) and (parent <> nil) and (not parent.HasChildren) then begin
    for i := 0 to list.Count - 1 do begin
      n := root.AddChild(parent, list[i]);
      if (list[i].Contains(':')) then begin
        n.ImageIndex:= 0;
        n.SelectedIndex:= 0;
      end else begin
        n.ImageIndex:= 1;
        n.SelectedIndex:= 1;
      end;
    end;
  end;
  list.Free;
end;

procedure BuildMethodIndex(projectPath: string; classPath: string);
var
  savePath: string;
  list: TStringList;
  i: Integer;
begin
  savePath:= ExtractFilePath(ParamStr(0)) + 'index' + SPLIT + md5EncryptString(projectPath) + SPLIT + 'class' + SPLIT;
  ForceDirectories(savePath);
  savePath += md5EncryptString(classPath);
  list := TStringList.Create;
  if (not FileExists(savePath)) then begin
    with TStringList.Create do begin
      LoadFromFile(classPath);
      for i := 0 to Count - 1 do begin
        if (Strings[i].StartsWith('.field')) then begin
          list.Add(ExtractField(Strings[i]));
        end else if (Strings[i].StartsWith('.method')) then begin
          list.Add(ExtractMethod(Strings[i]));
        end;
      end;
      Free;
    end;
    list.SaveToFile(savePath);
  end;
end;

function LoadMethodIndex(projectPath: string; classPath: string): string;
var
  idxPath: string;
begin
  idxPath:= ExtractFilePath(ParamStr(0)) + 'index' + SPLIT + md5EncryptString(projectPath) + SPLIT + 'class' + SPLIT;
  idxPath += md5EncryptString(classPath);
  Result := '';
  if (FileExists(idxPath)) then begin
    with TStringList.Create do begin
      LoadFromFile(idxPath);
      Result := Text;
      Free;
    end;
  end;
end;

function ClassIndexToFilePath(projectPath: string; indexPkg: string): string;
var
  i: Integer;
  basePath: string;
  fullPath: string;
begin
  Result := '';
  // class index to file path
  basePath:= ExtractFilePath(projectPath);
  for i := 1 to 100 do begin
    if (i = 1) then begin
      fullPath:= basePath + 'smali' + SPLIT + indexPkg.Replace('.', SPLIT, [rfIgnoreCase, rfReplaceAll]) + '.smali';
      if (FileExists(fullPath)) then begin
        Result := fullPath;
        Break;
      end else begin
        fullPath:= basePath + 'smali' + SPLIT + indexPkg.Replace('.', SPLIT, [rfIgnoreCase, rfReplaceAll]) + '.1.smali';
        if (FileExists(fullPath)) then begin
          Result := fullPath;
          Break;
        end;
      end;
    end else begin
      fullPath:= basePath + 'smali_classes' + IntToStr(i) + SPLIT + indexPkg.Replace('.', SPLIT, [rfIgnoreCase, rfReplaceAll]) + '.smali';
      if (FileExists(fullPath)) then begin
        Result := fullPath;
        Break;
      end else begin
        fullPath:= basePath + 'smali_classes' + IntToStr(i) + SPLIT + indexPkg.Replace('.', SPLIT, [rfIgnoreCase, rfReplaceAll]) + '.1.smali';
        if (FileExists(fullPath)) then begin
          Result := fullPath;
          Break;
        end;
      end;
    end;
  end;
end;

function GetJavaFilePath(indexPath: string; classPath: string): string;
var
  p: string;
begin
  Result := '';
  p := classPath;
  while True do begin
    Result := indexPath + p.Replace('.', SPLIT, [rfIgnoreCase, rfReplaceAll]) + '.java';
    if FileExists(Result) then Break;
    if (not p.Contains('$')) then Break;
    p := p.Substring(0, p.LastIndexOf('$'));
  end;
end;

function ConvertSmaliToJava(projectPath: string; path: string): string;
var
  p: string;
  pclass: string;
  javaFilePath: string;
begin
  Result := '';
  // smali to java
  p := ExtractFilePath(ParamStr(0)) + 'index' + SPLIT + md5EncryptString(projectPath) + SPLIT + 'proj' + SPLIT;
  pclass := FullPathToClassPath(path);
  javaFilePath:= GetJavaFilePath(p, pclass);
  if (FileExists(javaFilePath)) then begin
    with TStringList.Create do begin
      LoadFromFile(javaFilePath);
      Result := Text;
      Free;
    end;
  end;
end;

function NodeToPath(projectPath: string; node: TTreeNode): string;
var
  p: string = '';
begin
  // node to path
  p := node.Text;
  while node.Parent <> nil do begin
    p := node.Parent.Text + SPLIT + p;
    node := node.Parent;
  end;
  p := ExtractFilePath(projectPath) + p;
  if (DirectoryExists(p)) then begin
    p += SPLIT;
  end;
  Result := p;
end;

procedure NewFile(projectPath: string; filePath: string; name: string; template: string; root: TTreeNodes; node: TTreeNode; pageControl: TPageControl; onCodeJump: TOnCodeJump);
var
  indexPath: string;
  smaliFilePath: string;
  tmpPath: string;
  classPath: string;
  page: TSmaliCodeView;
  pageText: TTextCodeView;
  n: TTreeNode;
begin
  if (template <> '') and (onCodeJump <> nil) then begin
    indexPath := ExtractFilePath(ParamStr(0)) + 'index' + SPLIT + md5EncryptString(projectPath) + SPLIT + 'index';
    ForceDirectories(indexPath);
    smaliFilePath:= filePath + name + '.smali';
    tmpPath:= ExtractFilePath(ParamStr(0)) + 'template' + SPLIT + template;
    classPath:= FullPathToClassPath(smaliFilePath);
    with TStringList.Create do begin
      LoadFromFile(tmpPath);
      Text:= StringReplace(Text, '{% class name %}', 'L' + StringReplace(classPath, '.', '/', [rfIgnoreCase, rfReplaceAll]) + ';', [rfReplaceAll, rfIgnoreCase]);
      Text:= StringReplace(Text, '{% java file name %}', string(ExtractFileName(smaliFilePath)).Replace('.smali', '').Trim, [rfIgnoreCase, rfReplaceAll]);
      SaveToFile(smaliFilePath);
      Free;
    end;
    with TStringList.Create do begin
      if (FileExists(indexPath)) then
        LoadFromFile(indexPath);
      Add(classPath);
      Sort;
      SaveToFile(indexPath);
      Free;
    end;
    n := root.AddChild(node, ExtractFileName(smaliFilePath));
    n.ImageIndex:= 2;
    n.SelectedIndex:= 2;
    page := TSmaliCodeView.Create(pageControl);
    page.Parent := pageControl;
    page.ProjectPath:= projectPath;
    page.FileName:= smaliFilePath;
    page.OnCodeJump:=onCodeJump;
  end else begin
    smaliFilePath:= filePath + name;
    with TStringList.Create do begin
      SaveToFile(filePath + name);
      Free;
    end;
    n := root.AddChild(node, ExtractFileName(smaliFilePath));
    n.ImageIndex:= 0;
    n.SelectedIndex:= 0;
    pageText := TTextCodeView.Create(pageControl);
    pageText.Parent := pageControl;
    pageText.FileName:= smaliFilePath;
  end;
  pageControl.TabIndex:= pageControl.PageCount - 1;
end;

function IsTextFile(path: string): Boolean;
var
  ext: string;
begin
  ext := string(ExtractFileExt(path)).ToLower;
  Result := (ext = '.txt') or (ext = '.xml') or (ext = '.conf') or (ext = '.ini') or (ext = '.js')
    or (ext = '.css') or (ext = '.html') or (ext = '.htm') or (ext = '.aidl') or (ext = '.sh')
    or (ext = '.properties') or (ext = '.gradle') or (ext = '.yml');
end;

function IsImageFile(path: string): Boolean;
var
  ext: string;
begin
  ext := string(ExtractFileExt(path)).ToLower;
  Result := (ext = '.png') or (ext = '.jpg') or (ext = '.jpeg') or (ext = '.bmp') or (ext = '.gif')
    or (ext = '.ico') or (ext = '.cur') or (ext = '.xpm') or (ext = '.icns') or (ext = '.tif') or (ext = '.tiff')
    or (ext = '.pbm') or (ext = '.pgm') or (ext = '.ppm') or (ext = '.tga');
end;

procedure NewClass(projectPath: string; filePath: string; root: TTreeNodes; node: TTreeNode; pageControl: TPageControl; onCodeJump: TOnCodeJump);
var
  cn: string;
begin
  cn := frmInputBox.InputBox('New Class', 'Class Name', '').Trim;
  if (cn = '') then Exit;
  NewFile(projectPath, filePath, cn, 'new_class', root, node, pageControl, onCodeJump);
end;

procedure NewInterface(projectPath: string; filePath: string; root: TTreeNodes; node: TTreeNode; pageControl: TPageControl; onCodeJump: TOnCodeJump);
var
  cn: string;
begin
  cn := frmInputBox.InputBox('New Interface', 'Interface Name', '').Trim;
  if (cn = '') then Exit;
  NewFile(projectPath, filePath, cn, 'new_interface', root, node, pageControl, onCodeJump);
end;

procedure NewEnum(projectPath: string; filePath: string; root: TTreeNodes; node: TTreeNode; pagecontrol: TPageControl; onCodeJump: TOnCodeJump);
var
  cn: string;
begin
  cn := frmInputBox.InputBox('New Enum', 'Enum Name', '').Trim;
  if (cn = '') then Exit;
  NewFile(projectPath, filePath, cn, 'new_enum', root, node, pagecontrol, onCodeJump);
end;

procedure NewAnnotation(projectPath: string; filePath: string; root: TTreeNodes; node: TTreeNode; pagecontrol: TPageControl; onCodeJump: TOnCodeJump);
var
  cn: string;
begin
  cn := frmInputBox.InputBox('New Annotation', 'Annotation Name', '').Trim;
  if (cn = '') then Exit;
  NewFile(projectPath, filePath, cn, 'new_annotation', root, node, pagecontrol, onCodeJump);
end;

procedure NewTextFile(projectPath: string; filePath: string; root: TTreeNodes; node: TTreeNode; pageControl: TPageControl);
var
  cn: string;
begin
  cn := frmInputBox.InputBox('New Text File', 'File Name', '').Trim;
  if (cn = '') then Exit;
  if (not cn.Contains('.')) then begin
    cn += '.txt';
  end;
  NewFile(projectPath, filePath, cn, '', root, node, pageControl, nil);
end;

function ExtractPureFileName(path: string): string;
var
  r: string;
begin
  r := ExtractFileName(path);
  r := r.Replace('.apk', '', [rfIgnoreCase, rfReplaceAll]);
  Result := r;
end;

procedure DecompilePackage(AAPkPath: string; AOutputPath: string;
  AIsNoRes: Boolean; AISNoSrc: Boolean; callback: TOnCommandOutput;
  complete: TOnCommandComplete);
var
  p: string;
begin
  // decompile
  with TCommandThread.Create(ctDecompile, [AAPkPath, AOutputPath, IfThen(AIsNoRes, '1', '0'), IfThen(AISNoSrc, '1', '0')]) do begin
    OnCommandOutput:= callback;
    OnCommandComplete:= complete;
    Start;
  end;
  if (not AOutputPath.EndsWith(SPLIT)) then AOutputPath += SPLIT;
  AOutputPath += ExtractPureFileName(AAPkPath) + SPLIT;
  AOutputPath += 'apktool.yml';
  p := ExtractFilePath(ParamStr(0)) + 'index' + SPLIT + md5EncryptString(AOutputPath) + SPLIT + 'proj' + SPLIT;
  if (not DirectoryExists(p)) then ForceDirectories(p);
  with TCommandThread.Create(ctJadxDecompile, [AAPkPath, p]) do begin
    OnCommandOutput:= callback;
    OnCommandComplete:= complete;
    Start;
  end;
  p := ExtractFilePath(ParamStr(0)) + 'index' + SPLIT + md5EncryptString(AOutputPath) + SPLIT + 'ssmali' + SPLIT;
  with TCommandThread.Create(ctSsmaliDecompile, [AAPkPath, p]) do begin
    OnCommandOutput:= callback;
    OnCommandComplete:= complete;
    Start;
  end;
end;

procedure CompilePackage(AProjectPath: string; callback: TOnCommandOutput;
  complete: TOnCommandComplete);
begin
  // compile package
  with TCommandThread.Create(ctCompile, [AProjectPath]) do begin
    OnCommandOutput:= callback;
    OnCommandComplete:= complete;
    Start;
  end;
end;

procedure InstallFramework(AFrameworkPath: string; callback: TOnCommandOutput;
  complete: TOnCommandComplete);
begin
  // install framework
  with TCommandThread.Create(ctInstallFramework, [AFrameworkPath]) do begin
    OnCommandOutput:= callback;
    OnCommandComplete:= complete;
    Start;
  end;
end;

function FindFileInAndroidSDK(APath: string): string;
var
  p: string;
  base: string;
begin
  // find file in android sdk
  base := GlobalConfig.AndroidSDKPath;
  if (not base.EndsWith(SPLIT)) then base += SPLIT;
  base += 'sources';
  base += SPLIT;
  base += GlobalConfig.AndroidSDKVersion;
  base += SPLIT;
  p := base + APath;
  p += '.java';
  Result := p;
  if (FileExists(p)) then Exit;

  // $
  while (APath.Contains('$')) do begin
    APath:= APath.Substring(0, APath.LastIndexOf('$'));
    p := base + APath;
    p += '.java';
    Result := p;
    if (FileExists(p)) then Exit;
  end;

  if (not FileExists(p)) then Result := '';
end;

function FindSsmaliFile(AProjectPath: string; classPath: string): string;
var
  p: string;
begin
  Result := '';
  p := ExtractFilePath(ParamStr(0)) + 'index' + SPLIT + md5EncryptString(AProjectPath) + SPLIT + 'ssmali' + SPLIT;
  p += classPath.Replace('.', SPLIT, [rfIgnoreCase, rfReplaceAll]);
  if (FileExists(p + '.smali')) then Result := p + '.smali'
  else if (FileExists(p + '.1.smali')) then Result := p + '.1.smali';
end;

{ TBuildClassIndexThread }

procedure TBuildClassIndexThread.Execute;
var
  indexPath: string;
begin
  indexPath := ExtractFilePath(ParamStr(0)) + 'index' + SPLIT + md5EncryptString(FPath);
  ForceDirectories(indexPath);
  if (not FileExists(indexPath + SPLIT + 'index')) then begin
    BuildClassIndex(FPath);
  end;
end;

constructor TBuildClassIndexThread.Create(path: string);
begin
  inherited Create(True);
  FreeOnTerminate:= True;
  FCount := 0;
  FClassList := TStringList.Create;
  FPath:= path;
  OnTerminate:=@buildIndexTerminate;
end;

destructor TBuildClassIndexThread.Destroy;
begin
  FClassList.Free;
  inherited Destroy;
end;

end.

