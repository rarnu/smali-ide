unit ThemeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Buttons, Forms, StdCtrls, ExtCtrls, ComCtrls, Controls, SynEdit, SynCompletion, Graphics, Menus,
  SynGutterBase, SynGutterLineNumber, SynGutter, SynGutterCodeFolding;

procedure RecolorButton(b: TBitBtn);
procedure RecolorTreeView(t: TTreeView);
procedure RecolorListView(l: TListBox);
procedure RecolorSynEdit(e: TSynEdit);
procedure RecolorEdit(e: TEdit);
procedure RecolorMemo(m: TMemo);
procedure RecolorComboBox(c: TComboBox);
procedure RecolorCompleter(c: TSynCompletion);

implementation

uses
  config;

procedure RecolorButton(b: TBitBtn);
begin
  b.Color:= GlobalConfig.Color;
  b.Font.Name:= GlobalConfig.FontName;
  b.Font.Color:= GlobalConfig.FontColor;
  b.Font.Size:= GlobalConfig.FontSize;
end;

procedure RecolorTreeView(t: TTreeView);
begin
  t.BackgroundColor:= GlobalConfig.Color;
  t.Font.Name:= GlobalConfig.FontName;
  t.Font.Color:= GlobalConfig.FontColor;
  t.Font.Size:= GlobalConfig.FontSize;
end;

procedure RecolorListView(l: TListBox);
begin
  l.Color:= GlobalConfig.Color;
  l.Font.Name:= GlobalConfig.FontName;
  l.Font.Size:= GlobalConfig.FontSize;
  l.Font.Color:= GlobalConfig.FontColor;
end;

procedure RecolorSynEdit(e: TSynEdit);
var
  i: Integer;
  part: TSynGutterPartBase;
begin
  if (e.Highlighter = nil) then begin
    e.Color:= GlobalConfig.Color;
    e.Font.Color:= GlobalConfig.FontColor;
  end;
  e.Font.Name:= GlobalConfig.FontName;
  e.Font.Size:= GlobalConfig.FontSize;
  e.Gutter.Color:= GlobalConfig.Color;
  for i := 0 to e.Gutter.Parts.Count - 1 do begin
    part := e.Gutter.Parts.Part[i];
    if (part is TSynGutterLineNumber) then begin
      TSynGutterLineNumber(part).MarkupInfo.Background:= GlobalConfig.Color;
      TSynGutterLineNumber(part).MarkupInfo.Foreground:= GlobalConfig.FontColor;
    end;
    if (part is TSynGutterSeparator) then begin
      TSynGutterSeparator(part).MarkupInfo.Foreground:= clSilver;
    end;
    if (part is TSynGutterCodeFolding) then begin
      TSynGutterCodeFolding(part).MarkupInfo.Foreground:= clSilver;
    end;
  end;
  e.RightEdgeColor:= e.Color;
end;

procedure RecolorEdit(e: TEdit);
begin
  e.Color:= GlobalConfig.Color;
  e.Font.Name:= GlobalConfig.FontName;
  e.Font.Size:= GlobalConfig.FontSize;
  e.Font.Color:= GlobalConfig.FontColor;
end;

procedure RecolorMemo(m: TMemo);
begin
  m.Color:= GlobalConfig.Color;
  m.Font.Name:= GlobalConfig.FontName;
  m.Font.Size:= GlobalConfig.FontSize;
  m.Font.Color:= GlobalConfig.FontColor;
end;

procedure RecolorComboBox(c: TComboBox);
begin
  c.Color:= GlobalConfig.Color;
  c.Font.Name:= GlobalConfig.FontName;
  c.Font.Size:= GlobalConfig.FontSize;
  c.Font.Color:= GlobalConfig.FontColor;
end;

procedure RecolorCompleter(c: TSynCompletion);
begin
  c.TheForm.BackgroundColor:= GlobalConfig.Color;
  c.TheForm.TextColor:= GlobalConfig.FontColor;
end;

end.

