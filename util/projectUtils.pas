unit projectUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Dialogs;

function OpenProject(): String;
procedure LoadProjectFiles(path: string; root: TTreeNodes; parent: TTreeNode);

implementation

function OpenProject: String;
begin
  Result := '';
  with TOpenDialog.Create(nil) do begin
    Filter:= 'yml file|*.yml';
    if Execute then begin
      Result := FileName;
    end;
    Free;
  end;
end;

procedure LoadProjectFiles(path: string; root: TTreeNodes; parent: TTreeNode);
var
  rootPath: string;
  src: TSearchRec;
  node: TTreeNode;
begin
  if (path.EndsWith('.yml')) then begin
    rootPath:= ExtractFilePath(path);
  end else begin
    rootPath:= path;
  end;
  if FindFirst(rootPath + '*', faAnyFile, src) = 0 then begin
    repeat
      if (src.Name = '.') or (src.Name = '..') then begin
        continue;
      end;
      if (DirectoryExists(rootPath + src.Name)) then begin
        node := root.AddChild(parent, src.Name);
        node.ImageIndex:= 1;
        node.SelectedIndex:= 1;
        LoadProjectFiles(rootPath + src.Name + '/', root, node);
      end else begin
        node := root.AddChild(parent, src.Name);
        if (string(src.Name).EndsWith('.smali')) then begin
          node.ImageIndex:= 2;
          node.SelectedIndex:= 2;
        end else begin
          node.ImageIndex:= 0;
          node.SelectedIndex:= 0;
        end;
      end;
    until FindNext(src) <> 0;
    FindClose(src);
  end;
end;

end.

