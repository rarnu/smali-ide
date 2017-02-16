unit GithubUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, FileUtil;

const
  GIT_URL = 'https://github.com/iBotPeaches/Apktool.git';
  GIT_ZIP = 'https://github.com/iBotPeaches/Apktool/archive/master.zip';

procedure CloneApkTool();
procedure Pull();
procedure Compile();
procedure ReplaceJar(path: string);

implementation

procedure Execute(cmd: string; params: array of string);
const
  BUF_SIZE = 2048;
var
  AProcess: TProcess;
  bytesRead: Integer;
  buffer: array[0..BUF_SIZE - 1] of byte;
  outstr: string;
  i: Integer;
begin
  AProcess := TProcess.Create(nil);
  AProcess.Executable:= cmd;
  for i := 0 to Length(params) - 1 do AProcess.Parameters.Add(params[i]);
  AProcess.Options:= [poUsePipes];
  AProcess.Execute;
  repeat
    FillChar(buffer, BUF_SIZE, 0);
    bytesRead:= AProcess.Output.Read(buffer, BUF_SIZE);
    outstr:= string(StringOf(buffer));
    WriteLn(outstr);
  until bytesRead = 0;
  AProcess.Free;
end;

procedure CloneApkTool;
var
  p: string;
begin
  p := ExtractFilePath(ParamStr(0)) + 'Apktool';
  if (DirectoryExists(p)) then Exit;
  // clone
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  Execute('git', ['clone', GIT_URL]);
end;

procedure Pull;
var
  p: string;
begin
  // pull
  p := ExtractFilePath(ParamStr(0)) + 'Apktool';
  SetCurrentDir(p);
  Execute('git', ['pull']);
end;

procedure Compile;
var
  p: string;
begin

  // clean
  p := ExtractFilePath(ParamStr(0)) + 'Apktool/brut.apktool/apktool-cli/build/libs';
  if (DirectoryExists(p)) then DeleteDirectory(p, True);

  // compile
  p:= ExtractFilePath(ParamStr(0)) + 'Apktool';
  SetCurrentDir(p);
  Execute(p + '/gradlew', ['build', 'fatJar']);
end;

procedure ReplaceJar(path: string);
var
  p: string;
  jarPath: string = '';
  src: TSearchRec;
begin
  // replace jar
  p := ExtractFilePath(ParamStr(0)) + 'Apktool/brut.apktool/apktool-cli/build/libs';
  if (FindFirst(p + '/*.jar', faAnyFile, src) = 0) then begin
    jarPath:= p + '/' + src.Name;
    FindClose(src);
  end;
  if (jarPath.Trim <> '') and (FileExists(jarPath)) then begin
    CopyFile(jarPath, path);
  end;
end;

end.

