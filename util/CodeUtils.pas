unit CodeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, lockbox, Dialogs, smaliCodeView, textCodeView, CommandUtils, strutils;

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

procedure ThreadBuildClassIndex(projectPath: string; callback: TOnBuildIndexCallback; complete: TOnBuildIndexCompleteCallback);
procedure BuildMethodIndex(projectPath: string; classPath: string; root: TTreeNodes; parent: TTreeNode);
procedure BuildMethodIndex(projectPath: string; classPath: string);
function LoadMethodIndex(projectPath: string; classPath: string): string;
function ClassIndexToFilePath(projectPath: string; indexPkg: string): string;
function ConvertSmaliToJava(path: string): string;
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

implementation

uses
  EncryptUtils;

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
  for i := 1 to 9 do begin
    if (i = 1) then begin
      if (path.Contains('/smali/')) then begin
        ret := path.Substring(path.IndexOf('/smali/') + 7);
      end;
    end else begin
      if (path.Contains('/smali_classes' + IntToStr(i) + '/')) then begin
        ret := path.Substring(path.IndexOf('/smali_classes' + IntToStr(i) + '/'), 16);
      end;
    end;
  end;
  if (ret <> '') then begin
    ret := ret.Substring(0, ret.IndexOf('.smali'));
  end;
  if (ret.EndsWith('.1')) then begin
    ret := ret.Substring(0, ret.Length - 2);
  end;
  ret := ret.Replace('/', '.', [rfReplaceAll, rfIgnoreCase]);
  Result := ret;
end;

procedure TBuildClassIndexThread.BuildClassIndex(projectPath: string);
var
  basePath: string;
  src: TSearchRec;
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
        BuildClassIndex(basePath + src.Name + '/');
      end else begin
        if (string(src.Name).EndsWith('.smali')) then begin
          FClassList.Add(FullPathToClassPath(basePath + src.Name));
          if (Assigned(FOnBuildIndexCallback)) then begin
            Inc(FCount);
            FIndexFile:= src.Name;
            Synchronize(@SendSyncCallback);
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
  indexPath := ExtractFilePath(ParamStr(0)) + 'index/' + md5EncryptString(FPath);
  ForceDirectories(indexPath);
  if (not FileExists(indexPath + '/index')) then begin
    FClassList.SaveToFile(indexPath + '/index');
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
  savePath:= ExtractFilePath(ParamStr(0)) + 'index/' + md5EncryptString(projectPath) + '/class/';
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
  savePath:= ExtractFilePath(ParamStr(0)) + 'index/' + md5EncryptString(projectPath) + '/class/';
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
  idxPath:= ExtractFilePath(ParamStr(0)) + 'index/' + md5EncryptString(projectPath) + '/class/';
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
  for i := 1 to 9 do begin
    if (i = 1) then begin
      fullPath:= basePath + 'smali/' + indexPkg.Replace('.', '/', [rfIgnoreCase, rfReplaceAll]) + '.smali';
      if (FileExists(fullPath)) then begin
        Result := fullPath;
        Break;
      end else begin
        fullPath:= basePath + 'smali/' + indexPkg.Replace('.', '/', [rfIgnoreCase, rfReplaceAll]) + '.1.smali';
        if (FileExists(fullPath)) then begin
          Result := fullPath;
          Break;
        end;
      end;
    end else begin
      fullPath:= basePath + 'smali_classes' + IntToStr(i) + '/' + indexPkg.Replace('.', '/', [rfIgnoreCase, rfReplaceAll]) + '.smali';
      if (FileExists(fullPath)) then begin
        Result := fullPath;
        Break;
      end else begin
        fullPath:= basePath + 'smali_classes' + IntToStr(i) + '/' + indexPkg.Replace('.', '/', [rfIgnoreCase, rfReplaceAll]) + '.1.smali';
        if (FileExists(fullPath)) then begin
          Result := fullPath;
          Break;
        end;
      end;
    end;
  end;
end;

function ConvertSmaliToJava(path: string): string;
begin
  // TODO: smali to java
  result := '';
end;

function NodeToPath(projectPath: string; node: TTreeNode): string;
var
  p: string = '';
begin
  // node to path
  p := node.Text;
  while node.Parent <> nil do begin
    p := node.Parent.Text + '/' + p;
    node := node.Parent;
  end;
  p := ExtractFilePath(projectPath) + p;
  if (DirectoryExists(p)) then begin
    p += '/';
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
    indexPath := ExtractFilePath(ParamStr(0)) + 'index/' + md5EncryptString(projectPath) + '/index';
    ForceDirectories(indexPath);
    smaliFilePath:= filePath + name + '.smali';
    tmpPath:= ExtractFilePath(ParamStr(0)) + 'template/' + template;
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
    or (ext = '.properties') or (ext = '.gradle');
end;

function IsImageFile(path: string): Boolean;
var
  ext: string;
begin
  ext := string(ExtractFileExt(path)).ToLower;
  Result := (ext = '.png') or (ext = '.jpg') or (ext = '.jpeg') or (ext = '.bmp') or (ext = '.gif');
end;

procedure NewClass(projectPath: string; filePath: string; root: TTreeNodes; node: TTreeNode; pageControl: TPageControl; onCodeJump: TOnCodeJump);
var
  cn: string;
begin
  cn := InputBox('New Class', 'Class Name', '').Trim;
  if (cn = '') then Exit;
  NewFile(projectPath, filePath, cn, 'new_class', root, node, pageControl, onCodeJump);
end;

procedure NewInterface(projectPath: string; filePath: string; root: TTreeNodes; node: TTreeNode; pageControl: TPageControl; onCodeJump: TOnCodeJump);
var
  cn: string;
begin
  cn := InputBox('New Interface', 'Interface Name', '').Trim;
  if (cn = '') then Exit;
  NewFile(projectPath, filePath, cn, 'new_interface', root, node, pageControl, onCodeJump);
end;

procedure NewEnum(projectPath: string; filePath: string; root: TTreeNodes; node: TTreeNode; pagecontrol: TPageControl; onCodeJump: TOnCodeJump);
var
  cn: string;
begin
  cn := InputBox('New Enum', 'Enum Name', '').Trim;
  if (cn = '') then Exit;
  NewFile(projectPath, filePath, cn, 'new_enum', root, node, pagecontrol, onCodeJump);
end;

procedure NewAnnotation(projectPath: string; filePath: string; root: TTreeNodes; node: TTreeNode; pagecontrol: TPageControl; onCodeJump: TOnCodeJump);
var
  cn: string;
begin
  cn := InputBox('New Annotation', 'Annotation Name', '').Trim;
  if (cn = '') then Exit;
  NewFile(projectPath, filePath, cn, 'new_annotation', root, node, pagecontrol, onCodeJump);
end;

procedure NewTextFile(projectPath: string; filePath: string; root: TTreeNodes; node: TTreeNode; pageControl: TPageControl);
var
  cn: string;
begin
  cn := InputBox('New Text File', 'File Name', '').Trim;
  if (cn = '') then Exit;
  if (not cn.Contains('.')) then begin
    cn += '.txt';
  end;
  NewFile(projectPath, filePath, cn, '', root, node, pageControl, nil);
end;

procedure DecompilePackage(AAPkPath: string; AOutputPath: string;
  AIsNoRes: Boolean; AISNoSrc: Boolean; callback: TOnCommandOutput;
  complete: TOnCommandComplete);
begin
  // decompile
  with TCommandThread.Create(ctDecompile, [AAPkPath, AOutputPath, IfThen(AIsNoRes, '1', '0'), IfThen(AISNoSrc, '1', '0')]) do begin
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

{ TBuildClassIndexThread }

procedure TBuildClassIndexThread.Execute;
var
  indexPath: string;
begin
  indexPath := ExtractFilePath(ParamStr(0)) + 'index/' + md5EncryptString(FPath);
  ForceDirectories(indexPath);
  if (not FileExists(indexPath + '/index')) then begin
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

