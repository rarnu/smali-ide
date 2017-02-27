unit sdkCodeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, StdCtrls, ExtCtrls, ComCtrls, Controls, Graphics, SynEdit, codeViewIntf, SynHighlighterJava,
  SynGutterBase, SynGutterLineNumber, SynGutter, SynGutterCodeFolding, SynEditTypes, Menus, LCLType, Forms, IniFiles;

type

  { TSDKJavaCodeView }

  TSDKJavaCodeView = class(TTabSheet, ICodeViewIntf)
  private
    FFileName: string;
    FEditor: TSynEdit;
    FHighlighter: TSynJavaSyn;
    FMenu: TPopupMenu;
    // menu items
    FMiCopy: TMenuItem;

    // find
    FPnlFind: TPanel;
    FFindTitle1: TLabel;
    FFindEdit: TEdit;
    FFindMatchCase: TCheckBox;
    FFindBtn: TButton;
    FFindBtnNext: TButton;
    FFindBtnPrior: TButton;
    FFindBtnClose: TButton;

    procedure btnClicked(Sender: TObject);
    procedure menuClicked(Sender: TObject);
    procedure SetFileName(AValue: string);
  public
    constructor Create(TheOwner: TComponent); override;
    function QueryClose(): Boolean;
    procedure Save();
    procedure SaveAs();
    procedure Find();
    procedure FindNext();
    procedure CancelFind();
    procedure Replace();
    procedure CancelReplace();
    procedure GotoLine(line: Integer);
    procedure GotoPosition(apos: Integer);
    function GetEditor(): TSynEdit;
    function GetFileName(): string;
    procedure FocusEditor();
    procedure LoadShortcut();
    procedure SetCodeTheme(AThemeFile: string);
  published
    property FileName: string read GetFileName write SetFileName;
  end;

implementation

uses
  TextUtils, baseData, config;

{ TSDKJavaCodeView }

procedure TSDKJavaCodeView.SetFileName(AValue: string);
begin
  FFileName:=AValue;
  Caption:= ExtractFileName(FFileName);
  FEditor.Lines.LoadFromFile(FFileName);
end;

procedure TSDKJavaCodeView.btnClicked(Sender: TObject);
var
  opt: TSynSearchOptions;
begin
  if (sender = FFindBtn) or (sender = FFindBtnNext) then begin
    opt := [];
    if (FFindMatchCase.Checked) then opt := opt + [ssoMatchCase];
    FEditor.SearchReplace(FFindEdit.Text, '', opt);
  end else if (sender = FFindBtnPrior) then begin
    opt := [ssoBackwards];
    if (FFindMatchCase.Checked) then opt := opt + [ssoMatchCase];
    FEditor.SearchReplace(FFindEdit.Text, '', opt);
  end else if (sender = FFindBtnClose) then begin
    CancelFind();
  end;
end;

procedure TSDKJavaCodeView.menuClicked(Sender: TObject);
begin
  if (sender = FMiCopy) then begin
    TextUtils.Copy(FEditor);
  end
end;

constructor TSDKJavaCodeView.Create(TheOwner: TComponent);
var
  i: integer;
  part: TSynGutterPartBase;
begin
  inherited Create(TheOwner);

  FEditor := TSynEdit.Create(Self);
  FHighlighter := TSynJavaSyn.Create(Self);

  with FEditor do begin
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
    Highlighter := FHighlighter;
    ReadOnly:= True;
  end;

  // menu
  FMenu := TPopupMenu.Create(Self);
  FMenu.Parent := Self;
  FEditor.PopupMenu := FMenu;
  FMiCopy:= TMenuItem.Create(FMenu);
  FMiCopy.Caption:= 'Copy';
  FMiCopy.ShortCut:= ShortCut(VK_C, [ssCtrl]);
  FMiCopy.OnClick:= @menuClicked;

  with FMenu.Items do begin
    Add(FMiCopy);
  end;

  // find
  FPnlFind:= TPanel.Create(Self);
  FPnlFind.Parent := Self;
  FPnlFind.Align:= alTop;
  FPnlFind.BevelInner:= bvRaised;
  FPnlFind.BevelOuter:= bvLowered;
  FPnlFind.Height:= 40;

  FFindTitle1:= TLabel.Create(FPnlFind);
  FFindTitle1.Parent := FPnlFind;
  FFindTitle1.AutoSize:= False;
  FFindTitle1.Align:= alLeft;
  FFindTitle1.Layout:= tlCenter;
  FFindTitle1.Width:= 60;
  FFindTitle1.Caption:= '  Find';

  FFindEdit:= TEdit.Create(FPnlFind);
  FFindEdit.Parent := FPnlFind;
  FFindEdit.Align:= alLeft;
  FFindEdit.Width:= 250;
  FFindEdit.Left:= 60;
  FFindEdit.BorderSpacing.Top:= 4;
  FFindEdit.BorderSpacing.Bottom:= 4;

  FFindMatchCase:= TCheckBox.Create(FPnlFind);
  FFindMatchCase.Parent := FPnlFind;
  FFindMatchCase.Align:= alLeft;
  FFindMatchCase.Width:= 60;
  FFindMatchCase.Left:= 310;
  FFindMatchCase.BorderSpacing.Top:= 4;
  FFindMatchCase.BorderSpacing.Bottom:=4;
  FFindMatchCase.Caption:= 'Match case';

  FFindBtn:= TButton.Create(FPnlFind);
  FFindBtn.Parent := FPnlFind;
  FFindBtn.Align:= alLeft;
  FFindBtn.Width:= 60;
  FFindBtn.Left:= 370;
  FFindBtn.BorderSpacing.Top:= 4;
  FFindBtn.BorderSpacing.Bottom:= 4;
  FFindBtn.Caption:= 'Find';

  FFindBtnNext:= TButton.Create(FPnlFind);
  FFindBtnNext.Parent := FPnlFind;
  FFindBtnNext.Align:= alLeft;
  FFindBtnNext.Width:= 60;
  FFindBtnNext.Left:= 430;
  FFindBtnNext.BorderSpacing.Top:= 4;
  FFindBtnNext.BorderSpacing.Bottom:= 4;
  FFindBtnNext.Caption:= 'Next';

  FFindBtnPrior:= TButton.Create(FPnlFind);
  FFindBtnPrior.Parent := FPnlFind;
  FFindBtnPrior.Align:= alLeft;
  FFindBtnPrior.Width:= 60;
  FFindBtnPrior.Left:= 490;
  FFindBtnPrior.BorderSpacing.Top:= 4;
  FFindBtnPrior.BorderSpacing.Bottom:= 4;
  FFindBtnPrior.Caption:= 'Prior';

  FFindBtnClose:= TButton.Create(FPnlFind);
  FFindBtnClose.Parent := FPnlFind;
  FFindBtnClose.Align:= alRight;
  FFindBtnClose.Width:= 32;
  FFindBtnClose.BorderSpacing.Top:= 4;
  FFindBtnClose.BorderSpacing.Bottom:= 4;
  FFindBtnClose.Caption:= 'X';

  FPnlFind.Visible:= False;

  FFindBtn.OnClick:=@btnClicked;
  FFindBtnNext.OnClick:= @btnClicked;
  FFindBtnPrior.OnClick:= @btnClicked;
  FFindBtnClose.OnClick:= @btnClicked;
end;

function TSDKJavaCodeView.QueryClose: Boolean;
begin
  Result := True;
end;

procedure TSDKJavaCodeView.Save;
begin
  // do nothing
end;

procedure TSDKJavaCodeView.SaveAs;
begin
  // do nothing
end;

procedure TSDKJavaCodeView.Find;
begin
  FPnlFind.Visible:= True;
  FFindEdit.SetFocus;
end;

procedure TSDKJavaCodeView.FindNext;
begin
  FFindBtnNext.Click;
end;

procedure TSDKJavaCodeView.CancelFind;
begin
  FFindEdit.Text:= '';
  FPnlFind.Visible:= False;
  FEditor.SetFocus;
end;

procedure TSDKJavaCodeView.Replace;
begin
  // do nothing
end;

procedure TSDKJavaCodeView.CancelReplace;
begin
  // do nothing
end;

procedure TSDKJavaCodeView.GotoLine(line: Integer);
begin
  FEditor.CaretY:= line;
  FEditor.SetFocus;
end;

procedure TSDKJavaCodeView.GotoPosition(apos: Integer);
begin
  FEditor.SelStart:= apos + 1;
end;

function TSDKJavaCodeView.GetEditor: TSynEdit;
begin
  Result := FEditor;
end;

function TSDKJavaCodeView.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TSDKJavaCodeView.FocusEditor;
var
  f: TCustomForm;
begin
  f := GetParentForm(Self);
  while f.ActiveControl <> FEditor do begin
    Application.ProcessMessages;
    f.ActiveControl := FEditor;
    FEditor.SetFocus;
  end;
end;

procedure TSDKJavaCodeView.LoadShortcut;
begin
  // do nothing
end;

function IfThen(b: Boolean; trueValue: TFontStyles; falseValue: TFontStyles): TFontStyles;
begin
  if b then Result := trueValue else Result := falseValue;
end;

procedure TSDKJavaCodeView.SetCodeTheme(AThemeFile: string);
var
  path: string;
begin
  path := ExtractFilePath(ParamStr(0)) + 'style' + SPLIT + AThemeFile;
  with TIniFile.Create(path) do begin
    FEditor.Color:= ReadInteger(SEC_JAVA, KEY_BACKGROUND, clWhite);
    with FHighlighter do begin
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
      NumberAttri.Style:= IfThen(ReadInteger(SEC_JAVA, KEY_NUMBER_BOLD, 0) <> 0, [fsBold], []);
      SpaceAttri.Foreground:= ReadInteger(SEC_JAVA, KEY_SPACE_COLOR, clWhite);
      StringAttri.Foreground:= ReadInteger(SEC_JAVA, KEY_STRING_COLOR, clMaroon);
      StringAttri.Style:= IfThen(ReadInteger(SEC_JAVA, KEY_STRING_BOLD, 0) <> 0, [fsBold], []);
      SymbolAttri.Foreground:= ReadInteger(SEC_JAVA, KEY_SYMBOL_COLOR, clGray);
      SymbolAttri.Style:= IfThen(ReadInteger(SEC_JAVA, KEY_SYMBOL_BOLD, 0) <> 0, [fsBold], []);
    end;
    Free;
  end;
end;

end.

