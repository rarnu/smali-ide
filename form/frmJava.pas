unit frmJava;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, IniFiles,
  frmBase, SynEdit, SynHighlighterJava, SynGutterBase, SynGutterLineNumber, SynGutterCodeFolding, SynGutter;

type

  { TFormJava }

  TFormJava = class(TFormBase)
    miSaveToFile: TMenuItem;
    popJava: TPopupMenu;
    procedure miSaveToFileClick(Sender: TObject);
  private
    FSynJava: TSynEdit;
    FSynJavaSyn: TSynJavaSyn;
    function GetCode: string;
    procedure SetCode(AValue: string);

    procedure SetCodeTheme(AThemeFile: string);
  protected
    procedure InitComponents; override;
    procedure InitEvents; override;
    procedure InitLogic; override;
  public
    property Code: string read GetCode write SetCode;
  end;

var
  FormJava: TFormJava;

implementation

uses
  config, baseData;

{$R *.lfm}

{ TFormJava }

procedure TFormJava.miSaveToFileClick(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do begin
    Filter:= 'java file|*.java';
    if Execute then FSynJava.Lines.SaveToFile(FileName);
    Free;
  end;
end;

function TFormJava.GetCode: string;
begin
  Result := FSynJava.Lines.Text;
end;

procedure TFormJava.SetCode(AValue: string);
begin
  FSynJava.Lines.Text:= AValue;
end;

function IfThen(b: Boolean; trueValue: TFontStyles; falseValue: TFontStyles): TFontStyles;
begin
  if b then Result := trueValue else Result := falseValue;
end;

procedure TFormJava.SetCodeTheme(AThemeFile: string);
var
  path: string;
begin
  path := ExtractFilePath(ParamStr(0)) + 'style' + SPLIT + AThemeFile;
  with TIniFile.Create(path) do begin
    FSynJava.Color:= ReadInteger(SEC_JAVA, KEY_BACKGROUND, clWhite);
    with FSynJavaSyn do begin
      AnnotationAttri.Foreground:= ReadInteger(SEC_JAVA, KEY_ANNOTATION_COLOR, clOlive);
      AnnotationAttri.Style:= IfThen(ReadInteger(SEC_JAVA, KEY_ANNOTATION_BOLD, 0) <> 0, [fsBold], []);
      CommentAttri.Foreground:= ReadInteger(SEC_JAVA, KEY_COMMENT_COLOR, clGreen);
      CommentAttri.Style:= IfThen(ReadInteger(SEC_JAVA, KEY_COMMENT_BOLD, 0) <> 0, [fsBold], []);
      DocumentAttri.Foreground:= ReadInteger(SEC_JAVA, KEY_DOCUMENT_COLOR, clGreen);
      DocumentAttri.Style:= IfThen(ReadInteger(SEC_JAVA, KEY_DOCUMENT_BOLD, 0) <> 0, [fsBold], []);
      IdentifierAttri.Foreground:= ReadInteger(SEC_JAVA, KEY_IDENTIFIER_COLOR, clBlack);
      IdentifierAttri.Style:= IfThen(ReadInteger(SEC_JAVA, KEY_IDENTIFIER_BOLD, 0) <> 0, [fsBold], []);
      KeyAttri.Foreground:= ReadInteger(SEC_JAVA, KEY_KEY_COLOR, clBlue);
      KeyAttri.Style:= IfThen(ReadInteger(SEC_JAVA, KEY_KEY_BOLD, 0) <> 0, [fsBold], []);
      NumberAttri.Foreground:= ReadInteger(SEC_JAVA, KEY_NUMBER_COLOR, clBlack);
      NumberAttri.Style := IfThen(ReadInteger(SEC_JAVA, KEY_NUMBER_BOLD, 0) <> 0, [fsBold], []);
      SpaceAttri.Foreground:= ReadInteger(SEC_JAVA, KEY_SPACE_COLOR, clWhite);
      StringAttri.Foreground:= ReadInteger(SEC_JAVA, KEY_STRING_COLOR, clMaroon);
      StringAttri.Style:= IfThen(ReadInteger(SEC_JAVA, KEY_STRING_BOLD, 0) <> 0, [fsBold], []);
      SymbolAttri.Foreground:= ReadInteger(SEC_JAVA, KEY_SYMBOL_COLOR, clGray);
      SymbolAttri.Style:= IfThen(ReadInteger(SEC_JAVA, KEY_SYMBOL_BOLD, 0) <> 0, [fsBold], []);
    end;
    Free;
  end;
end;

procedure TFormJava.InitComponents;
var
  i: Integer;
  part: TSynGutterPartBase;
begin
  FSynJava := TSynEdit.Create(Self);
  FSynJavaSyn := TSynJavaSyn.Create(Self);
  with FSynJava do begin
    Parent := Self;
    Align:= alClient;
    Color:= clWhite;
    Gutter.Color:= clWhite;
    for i := 0 to Gutter.Parts.Count - 1 do begin
      part := Gutter.Parts.Part[i];
      if (part is TSynGutterLineNumber) then begin
        TSynGutterLineNumber(part).MarkupInfo.Background:= clWhite;
      end;
      if (part is TSynGutterSeparator) then begin
        TSynGutterSeparator(part).MarkupInfo.Foreground:= clSilver;
      end;
      if (part is TSynGutterCodeFolding) then begin
        TSynGutterCodeFolding(part).MarkupInfo.Foreground:= clSilver;
      end;
    end;
    Options:= Options + [eoKeepCaretX] - [eoAutoIndent, eoScrollPastEol, eoSmartTabs];
    RightEdgeColor:= clWhite;
    RightGutter.Visible:= False;
    ScrollBars:= ssAutoBoth;
    TabWidth:= 4;
    ReadOnly:= True;
    Highlighter := FSynJavaSyn;
    PopupMenu := popJava;
  end;
  SetCodeTheme(GlobalConfig.CodeTheme);
end;

procedure TFormJava.InitEvents;
begin
  //
end;

procedure TFormJava.InitLogic;
begin
  //
end;

end.

