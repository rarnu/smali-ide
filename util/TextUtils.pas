unit TextUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit;

procedure Undo(edit: TSynEdit);
procedure Redo(edit: TSynEdit);
procedure Cut(edit: TSynEdit);
procedure Copy(edit: TSynEdit);
procedure Paste(edit: TSynEdit);
procedure Delete(edit: TSynEdit);

implementation

procedure Undo(edit: TSynEdit);
begin
  if (edit.CanUndo) then edit.Undo;
end;

procedure Redo(edit: TSynEdit);
begin
  if (edit.CanRedo) then edit.Redo;
end;

procedure Cut(edit: TSynEdit);
begin
  edit.CutToClipboard;
end;

procedure Copy(edit: TSynEdit);
begin
  edit.CopyToClipboard;
end;

procedure Paste(edit: TSynEdit);
begin
  edit.PasteFromClipboard;
end;

procedure Delete(edit: TSynEdit);
begin
  edit.SelText:= '';
end;

end.

