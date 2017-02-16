program apktoolupdater;

{$mode objfpc}{$H+}

uses
  {$IFNDEF WINDOWS}
  cthreads,
  {$ENDIF}
  Interfaces,
  Classes, sysutils, process, GithubUtils, Forms;

var
  apkToolPath: string;
begin
  apkToolPath:= ParamStr(1);
  if (apkToolPath.Trim = '') or (not FileExists(apkToolPath)) then Exit;
  CloneApkTool();
  Pull();
  Compile();
  ReplaceJar(apkToolPath);
end.

