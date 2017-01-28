unit CodeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, lockbox;

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
procedure BuildMethodIndex(classPath: string);
function ConvertSmaliToJava(path: string): string;

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
  FClassList.SaveToFile(indexPath + '/index');
  if (Assigned(FOnBuildIndexCompleteCallback)) then begin
    FOnBuildIndexCompleteCallback(Self);
  end;
end;

procedure ThreadBuildClassIndex(projectPath: string;
  callback: TOnBuildIndexCallback; complete: TOnBuildIndexCompleteCallback);
begin
  with TBuildClassIndexThread.Create(projectPath) do begin
    OnBuildIndexCallback:= callback;
    OnBuildIndexCompleteCallback:= complete;
    Start;
  end;
end;

procedure BuildMethodIndex(classPath: string);
begin
  // TODO: build method index
end;

function ConvertSmaliToJava(path: string): string;
begin
  // TODO: smali to java
  result := '';
end;

{ TBuildClassIndexThread }

procedure TBuildClassIndexThread.Execute;
begin
  BuildClassIndex(FPath);
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

