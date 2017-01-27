unit CodeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, lockbox;

procedure BuildClassIndex(projectPath: string; tree: TTreeNodes);
procedure BuildMethodIndex(classPath: string);
function ConvertSmaliToJava(path: string): string;

implementation

function md5EncryptString(str: string): string;
var
  d: TMD5Digest;
begin
  Result := '';
  try
    StringHashMD5(d, str);
    Result := BufferToHex(d, SizeOf(d));
  except
  end;
end;

function GetFullPathOfNode(n: TTreeNode): string;
begin
  Result := n.Text;
  while n.Parent <> nil do begin
    Result := n.Parent.Text + '/' + Result;
    n := n.Parent;
  end;
end;

procedure BuildClassIndex(projectPath: string; tree: TTreeNodes);
var
  dir: string;
  basePath: string;
  i: Integer;
begin
  dir := ExtractFilePath(ParamStr(0)) + md5EncryptString(projectPath);
  ForceDirectories(dir);
  basePath:= ExtractFilePath(projectPath);
  for i := 0 to tree.Count - 1 do begin
    if (tree.Item[i].Text.EndsWith('.smali')) then begin
      WriteLn(basePath + GetFullPathOfNode(tree.Item[i]));
    end;
  end;
end;

procedure BuildMethodIndex(classPath: string);
begin
  //
end;

function ConvertSmaliToJava(path: string): string;
begin
  // TODO: smali to java
  result := '';
end;

end.

