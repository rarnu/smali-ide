unit ProjectUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Dialogs;

function OpenProject(): String;
procedure LoadProject(path: string; root: TTreeNodes);
procedure ExpandProjectNode(path: string; root: TTreeNodes; node: TTreeNode);

implementation

uses
  baseData;

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

procedure LoadProject(path: string; root: TTreeNodes);
var
  basePath: string;
  src: TSearchRec;
  node: TTreeNode;
begin
  basePath:= ExtractFilePath(path);
  if (FindFirst(basePath + '*', faAnyFile, src) = 0) then begin
    repeat
      if (src.Name = '.') or (src.Name = '..') then begin
        continue;
      end;
      if (DirectoryExists(basePath + src.Name)) then begin
        node := root.AddChild(nil, src.Name);
        node.ImageIndex:= 1;
        node.SelectedIndex:= 1;
      end else begin
        node := root.AddChild(nil, src.Name);
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

procedure ExpandProjectNode(path: string; root: TTreeNodes; node: TTreeNode);
var
  basePath: string;
  filePath: string;
  fullPath: string;
  src: TSearchRec;
  tmpNode: TTreeNode;
  subNode: TTreeNode;
begin
  tmpNode := node;
  basePath:= ExtractFilePath(path);
  filePath:= tmpNode.Text;
  while tmpNode.Parent <> nil do begin
    filePath:= tmpNode.Parent.Text + SPLIT + filePath;
    tmpNode := tmpNode.Parent;;
  end;
  fullPath:= basePath + filePath + SPLIT;
  if (FindFirst(fullPath + '*', faAnyFile, src) = 0) then begin
    repeat
      if (src.Name = '.') or (src.Name = '..') then begin
        continue;
      end;
      if (DirectoryExists(fullPath + src.Name)) then begin
        subNode := root.AddChild(node, src.Name);
        subNode.ImageIndex:= 1;
        subNode.SelectedIndex:= 1;
      end else begin
        subNode := root.AddChild(node, src.Name);
        if (string(src.Name).EndsWith('.smali')) then begin
          subNode.ImageIndex:= 2;
          subNode.SelectedIndex:= 2;
        end else begin
          subNode.ImageIndex:= 0;
          subNode.SelectedIndex:= 0;
        end;
      end;
    until FindNext(src) <> 0;
    FindClose(src);
  end;
end;

end.

