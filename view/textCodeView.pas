unit textCodeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, ComCtrls, ExtCtrls, Graphics, SynEdit, SynGutterBase, SynGutterLineNumber, SynGutter, SynGutterCodeFolding, Menus, LCLType,
  SynEditTypes, Dialogs, Forms, codeViewIntf, SynHighlighterXML, SynHighlighterHTML, SynHighlighterCss, SynHighlighterJScript, synhighlighterunixshellscript, IniFiles,
  Buttons;

type
  { TTextCodeView }

  TTextCodeView = class(TTabSheet, ICodeViewIntf)
  private
    FEditor: TSynEdit;

    // highlighters
    FHighlightXml: TSynXMLSyn;
    FHighlightHtml: TSynHTMLSyn;
    FHighlightCss: TSynCssSyn;
    FHighlightJs: TSynJScriptSyn;
    FHighlightShell: TSynUNIXShellScriptSyn;

    FIsChanged: Boolean;
    FMenu: TPopupMenu;
    FTitle: string;
    FFileName: string;
    FProjectPath: string;

    // menu items
    FMiUndo: TMenuItem;
    FMiRedo: TMenuItem;
    FMiS1: TMenuItem;
    FMiCut: TMenuItem;
    FMiCopy: TMenuItem;
    FMiPaste: TMenuItem;
    FMiDelete: TMenuItem;

    // find & replace
    FPnlFind: TPanel;
    FFindTitle1: TLabel;
    FFindEdit: TEdit;
    FFindMatchCase: TCheckBox;
    FFindBtn: TBitBtn;
    FFindBtnNext: TBitBtn;
    FFindBtnPrior: TBitBtn;
    FFindBtnClose: TBitBtn;

    FPnlReplace: TPanel;
    FPnlReplace1: TPanel;
    FPnlReplace2: TPanel;
    FReplaceFindTitle: TLabel;
    FReplaceFindEdit: TEdit;
    FReplaceFindMatchCase: TCheckBox;
    FReplaceFindBtnClose: TBitBtn;
    FReplaceTitle: TLabel;
    FReplaceEdit: TEdit;
    FReplaceBtnReplace: TBitBtn;
    FReplaceBtnReplaceAll: TBitBtn;
    procedure btnClicked(Sender: TObject);
    function GetEditor: TSynEdit;
    function GetFileName: string;
    procedure menuClicked(Sender: TObject);
    procedure OnEditorChange(Sender: TObject);
    procedure SetFileName(AValue: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
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
    procedure SetCodeTheme(AThemeFile: string);
    procedure SetStyleTheme;
    procedure FocusEditor();
    procedure LoadShortcut();
  published
    property ProjectPath: string read FProjectPath write FProjectPath;
    property FileName: string read GetFileName write SetFileName;
    property Editor: TSynEdit read GetEditor;
    property Menu: TPopupMenu read FMenu write FMenu;
    property IsChanged: Boolean read FIsChanged;
  end;

implementation

uses
  TextUtils, baseData, config, ThemeUtils;

{ TTextCodeView }

procedure TTextCodeView.SetFileName(AValue: string);
var
  ext: string;
begin
  FFileName:=AValue;
  FTitle:= ExtractFileName(FFileName);
  Caption:= FTitle;
  FEditor.Lines.LoadFromFile(FFileName);

  ext := string(ExtractFileExt(FFileName)).Trim;
  FEditor.Highlighter := nil;
  if (ext = '.xml') then FEditor.Highlighter := FHighlightXml;
  if (ext = '.htm') or (ext = '.html') then FEditor.Highlighter := FHighlightHtml;
  if (ext = '.js') then FEditor.Highlighter := FHighlightJs;
  if (ext = '.css') then FEditor.Highlighter := FHighlightCss;
  if (ext = '.sh') then FEditor.Highlighter := FHighlightShell;

end;

procedure TTextCodeView.OnEditorChange(Sender: TObject);
begin
  FIsChanged := True;
  Caption:= FTitle + ' *';
end;

procedure TTextCodeView.menuClicked(Sender: TObject);
begin
  if (sender = FMiUndo) then begin
    TextUtils.Undo(FEditor);
  end else if (sender = FMiRedo) then begin
    TextUtils.Redo(FEditor);
  end else if (sender = FMiCut) then begin
    TextUtils.Cut(FEditor);
  end else if (sender = FMiCopy) then begin
    TextUtils.Copy(FEditor);
  end else if (sender = FMiPaste) then begin
    TextUtils.Paste(FEditor);
  end else if (sender = FMiDelete) then begin
    TextUtils.Delete(FEditor);
  end;
end;

procedure TTextCodeView.btnClicked(Sender: TObject);
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
  end else if (sender = FReplaceBtnReplace) then begin
    opt := [ssoReplace];
    if (FReplaceFindMatchCase.Checked) then opt := opt + [ssoMatchCase];
    FEditor.SearchReplace(FReplaceFindEdit.Text, FReplaceEdit.Text, opt);
  end else if (sender = FReplaceBtnReplaceAll) then begin
    opt := [ssoReplaceAll];
    if (FReplaceFindMatchCase.Checked) then opt := opt + [ssoMatchCase];
    FEditor.SearchReplace(FReplaceFindEdit.Text, FReplaceEdit.Text, opt);
  end else if (sender = FReplaceFindBtnClose) then begin
    CancelReplace();
  end;
end;

function TTextCodeView.GetEditor: TSynEdit;
begin
  Result := FEditor;
end;

function TTextCodeView.GetFileName: string;
begin
  Result := FFileName;
end;

constructor TTextCodeView.Create(TheOwner: TComponent);
var
  i: integer;
  part: TSynGutterPartBase;
begin
  inherited Create(TheOwner);
  FEditor := TSynEdit.Create(Self);

  FHighlightXml:= TSynXMLSyn.Create(Self);
  FHighlightHtml:= TSynHTMLSyn.Create(Self);
  FHighlightCss:= TSynCssSyn.Create(Self);
  FHighlightJs:= TSynJScriptSyn.Create(Self);
  FHighlightShell:= TSynUNIXShellScriptSyn.Create(Self);

  with FEditor do begin
    Parent := Self;
    Align:= alClient;
    Color:= clWhite;
    Options:= Options + [eoKeepCaretX] - [eoAutoIndent, eoScrollPastEol, eoSmartTabs];
    RightGutter.Visible:= False;
    ScrollBars:= ssAutoBoth;
    TabWidth:= 4;
    OnChange:=@OnEditorChange;
  end;
  FMenu := TPopupMenu.Create(Self);
  FMenu.Parent := Self;
  FEditor.PopupMenu := FMenu;

  // init menu
  FMiUndo:= TMenuItem.Create(FMenu);
  FMiUndo.Caption:= 'Undo';
  FMiUndo.ShortCut:= ShortCut(VK_Z, [ssCtrl]);
  FMiUndo.OnClick:=@menuClicked;
  FMiRedo:= TMenuItem.Create(FMenu);
  FMiRedo.Caption := 'Redo';
  FMiRedo.ShortCut:= ShortCut(VK_Y, [ssCtrl]);
  FMiRedo.OnClick:= @menuClicked;
  FMiS1:= TMenuItem.Create(FMenu);
  FMiS1.Caption:= '-';
  FMiCut := TMenuItem.Create(FMenu);
  FMiCut.Caption:= 'Cut';
  FMiCut.ShortCut:= ShortCut(VK_X, [ssCtrl]);
  FMiCut.OnClick:= @menuClicked;
  FMiCopy:= TMenuItem.Create(FMenu);
  FMiCopy.Caption:= 'Copy';
  FMiCopy.ShortCut:= ShortCut(VK_C, [ssCtrl]);
  FMiCopy.OnClick:= @menuClicked;
  FMiPaste:= TMenuItem.Create(FMenu);
  FMiPaste.Caption:= 'Paste';
  FMiPaste.ShortCut:= ShortCut(VK_V, [ssCtrl]);
  FMiPaste.OnClick:= @menuClicked;
  FMiDelete:= TMenuItem.Create(FMenu);
  FMiDelete.Caption:= 'Delete';
  FMiDelete.ShortCut:= ShortCut(VK_DELETE, []);
  FMiDelete.OnClick:= @menuClicked;

  with FMenu.Items do begin
    Add(FMiUndo);
    Add(FMiRedo);
    Add(FMiS1);
    Add(FMiCut);
    Add(FMiCopy);
    Add(FMiPaste);
    Add(FMiDelete);
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

  FFindBtn:= TBitBtn.Create(FPnlFind);
  FFindBtn.Parent := FPnlFind;
  FFindBtn.Align:= alLeft;
  FFindBtn.Width:= 60;
  FFindBtn.Left:= 370;
  FFindBtn.BorderSpacing.Top:= 4;
  FFindBtn.BorderSpacing.Bottom:= 4;
  FFindBtn.Caption:= 'Find';

  FFindBtnNext:= TBitBtn.Create(FPnlFind);
  FFindBtnNext.Parent := FPnlFind;
  FFindBtnNext.Align:= alLeft;
  FFindBtnNext.Width:= 60;
  FFindBtnNext.Left:= 430;
  FFindBtnNext.BorderSpacing.Top:= 4;
  FFindBtnNext.BorderSpacing.Bottom:= 4;
  FFindBtnNext.Caption:= 'Next';

  FFindBtnPrior:= TBitBtn.Create(FPnlFind);
  FFindBtnPrior.Parent := FPnlFind;
  FFindBtnPrior.Align:= alLeft;
  FFindBtnPrior.Width:= 60;
  FFindBtnPrior.Left:= 490;
  FFindBtnPrior.BorderSpacing.Top:= 4;
  FFindBtnPrior.BorderSpacing.Bottom:= 4;
  FFindBtnPrior.Caption:= 'Prior';

  FFindBtnClose:= TBitBtn.Create(FPnlFind);
  FFindBtnClose.Parent := FPnlFind;
  FFindBtnClose.Align:= alRight;
  FFindBtnClose.Width:= 32;
  FFindBtnClose.BorderSpacing.Top:= 4;
  FFindBtnClose.BorderSpacing.Bottom:= 4;
  FFindBtnClose.Caption:= 'X';

  FPnlFind.Visible:= False;

  // replace
  FPnlReplace := TPanel.Create(Self);
  FPnlReplace.Parent := Self;
  FPnlReplace.Align:= alTop;
  FPnlReplace.BevelInner:= bvNone;
  FPnlReplace.BevelOuter:= bvNone;
  FPnlReplace.Height:= 80;

  FPnlReplace1 := TPanel.Create(FPnlReplace);
  FPnlReplace1.Parent := FPnlReplace;
  FPnlReplace1.Align:= alTop;
  FPnlReplace1.BevelInner:= bvRaised;
  FPnlReplace1.BevelOuter:= bvLowered;
  FPnlReplace1.Height:= 40;

  FPnlReplace2 := TPanel.Create(FPnlReplace);
  FPnlReplace2.Parent := FPnlReplace;
  FPnlReplace2.Align:= alTop;
  FPnlReplace2.BevelInner:= bvRaised;
  FPnlReplace2.BevelOuter:= bvLowered;
  FPnlReplace2.Height:= 40;
  FPnlReplace2.Top:= 40;

  FReplaceFindTitle:= TLabel.Create(FPnlReplace1);
  FReplaceFindTitle.Parent := FPnlReplace1;
  FReplaceFindTitle.AutoSize:= False;
  FReplaceFindTitle.Align:= alLeft;
  FReplaceFindTitle.Layout:= tlCenter;
  FReplaceFindTitle.Width:= 60;
  FReplaceFindTitle.Caption:= '  Find';

  FReplaceFindEdit:= TEdit.Create(FPnlReplace1);
  FReplaceFindEdit.Parent := FPnlReplace1;
  FReplaceFindEdit.Align:= alLeft;
  FReplaceFindEdit.Width:= 250;
  FReplaceFindEdit.Left:= 60;
  FReplaceFindEdit.BorderSpacing.Top:= 4;
  FReplaceFindEdit.BorderSpacing.Bottom:= 4;

  FReplaceFindMatchCase:= TCheckBox.Create(FPnlReplace1);
  FReplaceFindMatchCase.Parent := FPnlReplace1;
  FReplaceFindMatchCase.Align:= alLeft;
  FReplaceFindMatchCase.Width:= 60;
  FReplaceFindMatchCase.Left:= 310;
  FReplaceFindMatchCase.BorderSpacing.Top:= 4;
  FReplaceFindMatchCase.BorderSpacing.Bottom:=4;
  FReplaceFindMatchCase.Caption:= 'Match case';

  FReplaceFindBtnClose:= TBitBtn.Create(FPnlReplace1);
  FReplaceFindBtnClose.Parent := FPnlReplace1;
  FReplaceFindBtnClose.Align:= alRight;
  FReplaceFindBtnClose.Width:= 32;
  FReplaceFindBtnClose.BorderSpacing.Top:= 4;
  FReplaceFindBtnClose.BorderSpacing.Bottom:= 4;
  FReplaceFindBtnClose.Caption:= 'X';

  FReplaceTitle:= TLabel.Create(FPnlReplace2);
  FReplaceTitle.Parent := FPnlReplace2;
  FReplaceTitle.AutoSize:= False;
  FReplaceTitle.Align:= alLeft;
  FReplaceTitle.Layout:= tlCenter;
  FReplaceTitle.Width:= 60;
  FReplaceTitle.Caption:= '  Replace';

  FReplaceEdit:= TEdit.Create(FPnlReplace2);
  FReplaceEdit.Parent := FPnlReplace2;
  FReplaceEdit.Align:= alLeft;
  FReplaceEdit.Width:= 250;
  FReplaceEdit.Left:= 60;
  FReplaceEdit.BorderSpacing.Top:= 4;
  FReplaceEdit.BorderSpacing.Bottom:= 4;

  FReplaceBtnReplace := TBitBtn.Create(FPnlReplace2);
  FReplaceBtnReplace.Parent := FPnlReplace2;
  FReplaceBtnReplace.Align:= alLeft;
  FReplaceBtnReplace.Width:= 60;
  FReplaceBtnReplace.Left:= 310;
  FReplaceBtnReplace.BorderSpacing.Top:= 4;
  FReplaceBtnReplace.BorderSpacing.Bottom:= 4;
  FReplaceBtnReplace.Caption:= 'Replace';

  FReplaceBtnReplaceAll := TBitBtn.Create(FPnlReplace2);
  FReplaceBtnReplaceAll.Parent := FPnlReplace2;
  FReplaceBtnReplaceAll.Align:= alLeft;
  FReplaceBtnReplaceAll.Width:= 60;
  FReplaceBtnReplaceAll.Left:= 370;
  FReplaceBtnReplaceAll.BorderSpacing.Top:= 4;
  FReplaceBtnReplaceAll.BorderSpacing.Bottom:= 4;
  FReplaceBtnReplaceAll.Caption:= 'All';

  FPnlReplace.Visible:= False;

  FFindBtn.OnClick:=@btnClicked;
  FFindBtnNext.OnClick:= @btnClicked;
  FFindBtnPrior.OnClick:= @btnClicked;
  FFindBtnClose.OnClick:= @btnClicked;
  FReplaceBtnReplace.OnClick:= @btnClicked;
  FReplaceBtnReplaceAll.OnClick:= @btnClicked;
  FReplaceFindBtnClose.OnClick:= @btnClicked;

  LoadShortcut();
end;

destructor TTextCodeView.Destroy;
begin
  FEditor.Free;
  inherited Destroy;
end;

function TTextCodeView.QueryClose: Boolean;
var
  dlg: TModalResult;
begin
  Result := True;
  if FIsChanged then begin
    dlg := MessageDlg('Hint', 'File changed, do you want to save it?', mtConfirmation, mbYesNoCancel, 0);
    if dlg = mrYes then begin
      Save();
    end;
    if dlg = mrCancel then begin
      Result := False;
    end;
  end;
end;

procedure TTextCodeView.Save;
begin
  FEditor.Lines.SaveToFile(FileName);
  FIsChanged := False;
  Caption:= FTitle;
end;

procedure TTextCodeView.SaveAs;
var
  dlg: TSaveDialog;
begin
  dlg := TSaveDialog.Create(nil);
  dlg.Filter:= 'all file|*.*';
  if dlg.Execute then begin
    FEditor.Lines.SaveToFile(dlg.FileName);
    self.FileName:= dlg.FileName;
    FIsChanged := False;
  end;
  dlg.Free;
end;

procedure TTextCodeView.Find;
begin
  FPnlReplace.Visible:= False;
  FPnlFind.Visible:= True;
  FFindEdit.SetFocus;
end;

procedure TTextCodeView.FindNext;
begin
  FFindBtnNext.Click;
end;

procedure TTextCodeView.CancelFind;
begin
  FFindEdit.Text:= '';
  FPnlFind.Visible:= False;
  FEditor.SetFocus;
end;

procedure TTextCodeView.Replace;
begin
  FPnlFind.Visible:= False;
  FPnlReplace.Visible:= True;
  FReplaceFindEdit.SetFocus;
end;

procedure TTextCodeView.CancelReplace;
begin
  FReplaceFindEdit.Text:= '';
  FReplaceEdit.Text:= '';
  FPnlReplace.Visible:= False;
  FEditor.SetFocus;
end;

procedure TTextCodeView.GotoLine(line: Integer);
begin
  FEditor.CaretY:= line;
  FEditor.SetFocus;
end;

procedure TTextCodeView.GotoPosition(apos: Integer);
begin
  FEditor.SelStart:= apos + 1;
end;

function IfThen(b: Boolean; trueValue: TFontStyles; falseValue: TFontStyles): TFontStyles;
begin
  if b then Result := trueValue else Result := falseValue;
end;

procedure TTextCodeView.SetCodeTheme(AThemeFile: string);
var
  path: string;
begin
  path := ExtractFilePath(ParamStr(0)) + 'style' + SPLIT + AThemeFile;
  with TIniFile.Create(path) do begin
    if (FEditor.Highlighter = FHighlightXml) then begin
      FEditor.Color:= ReadInteger(SEC_XML, KEY_BACKGROUND, clWhite);
      with FHighlightXml do begin
        ElementAttri.Foreground:= ReadInteger(SEC_XML, KEY_ELEMENT_COLOR, clMaroon);
        ElementAttri.Style:= IfThen(ReadInteger(SEC_XML, KEY_ELEMENT_BOLD, 0) <> 0, [fsBold], []);
        AttributeAttri.Foreground:= ReadInteger(SEC_XML, KEY_ATTR_COLOR, clMaroon);
        AttributeAttri.Style:= IfThen(ReadInteger(SEC_XML, KEY_ATTR_BOLD, 0) <> 0, [fsBold], []);
        NamespaceAttributeAttri.Foreground:= ReadInteger(SEC_XML, KEY_NAMESPACE_COLOR, clRed);
        NamespaceAttributeAttri.Style:= IfThen(ReadInteger(SEC_XML, KEY_NAMESPACE_BOLD, 0) <> 0, [fsBold], []);
        AttributeValueAttri.Foreground:= ReadInteger(SEC_XML, KEY_ATTRVALUE_COLOR, clNavy);
        AttributeValueAttri.Style:= IfThen(ReadInteger(SEC_XML, KEY_ATTRVALUE_BOLD, 0) <> 0, [fsBold], []);
        NamespaceAttributeValueAttri.Foreground:= ReadInteger(SEC_XML, KEY_NAMESPACEVALUE_COLOR, clRed);
        NamespaceAttributeValueAttri.Style:= IfThen(ReadInteger(SEC_XML, KEY_NAMESPACEVALUE_BOLD, 0) <> 0, [fsBold], []);
        TextAttri.Foreground:= ReadInteger(SEC_XML, KEY_TEXT_COLOR, clBlack);
        TextAttri.Style:= IfThen(ReadInteger(SEC_XML, KEY_TEXT_BOLD, 0) <> 0, [fsBold], []);
        CDATAAttri.Foreground:= ReadInteger(SEC_XML, KEY_CDATA_COLOR, clOlive);
        CDATAAttri.Style:= IfThen(ReadInteger(SEC_XML, KEY_CDATA_BOLD, 0) <> 0, [fsBold], []);
        EntityRefAttri.Foreground:= ReadInteger(SEC_XML, KEY_ENTITY_COLOR, clblue);
        EntityRefAttri.Style:= IfThen(ReadInteger(SEC_XML, KEY_ENTITY_BOLD, 0) <> 0, [fsBold], []);
        ProcessingInstructionAttri.Foreground:= ReadInteger(SEC_XML, KEY_PROCESSING_COLOR, clblue);
        ProcessingInstructionAttri.Style:= IfThen(ReadInteger(SEC_XML, KEY_PROCESSING_BOLD, 0) <> 0, [fsBold], []);
        CommentAttri.Foreground:= ReadInteger(SEC_XML, KEY_COMMENT_COLOR, clGray);
        CommentAttri.Style:= IfThen(ReadInteger(SEC_XML, KEY_COMMENT_BOLD, 0) <> 0, [fsBold], []);
        DocTypeAttri.Foreground:= ReadInteger(SEC_XML, KEY_DOCTYPE_COLOR, clblue);
        DocTypeAttri.Style:= IfThen(ReadInteger(SEC_XML, KEY_DOCTYPE_BOLD, 0) <> 0, [fsBold], []);
        SpaceAttri.Foreground:= ReadInteger(SEC_XML, KEY_SPACE_COLOR, clWhite);
        SymbolAttri.Foreground:= ReadInteger(SEC_XML, KEY_SYMBOL_COLOR, clGray);
        SymbolAttri.Style:= IfThen(ReadInteger(SEC_XML, KEY_SYMBOL_BOLD, 0) <> 0, [fsBold], []);
      end;
    end else if (FEditor.Highlighter = FHighlightHtml) then begin
      FEditor.Color:= ReadInteger(SEC_HTML, KEY_BACKGROUND, clWhite);
      with FHighlightHtml do begin
        AndAttri.Foreground:= ReadInteger(SEC_HTML, KEY_AND_COLOR, $0000ff00);
        AndAttri.Style:= IfThen(ReadInteger(SEC_HTML, KEY_AND_BOLD, 0) <> 0, [fsBold], []);
        ASPAttri.Foreground:= ReadInteger(SEC_HTML, KEY_ASP_COLOR, clBlack);
        ASPAttri.Style:= IfThen(ReadInteger(SEC_HTML, KEY_ASP_BOLD, 0) <> 0, [fsBold], []);
        CDATAAttri.Foreground:= ReadInteger(SEC_HTML, KEY_CDATA_COLOR, clGreen);
        CDATAAttri.Style:= IfThen(ReadInteger(SEC_HTML, KEY_CDATA_BOLD, 0) <> 0, [fsBold], []);
        DocTypeAttri.Foreground:= ReadInteger(SEC_HTML, KEY_DOCTYPE_COLOR, clBlack);
        DocTypeAttri.Style:= IfThen(ReadInteger(SEC_HTML, KEY_DOCTYPE_BOLD, 0) <> 0, [fsBold], []);
        CommentAttri.Foreground:= ReadInteger(SEC_HTML, KEY_COMMENT_COLOR, clGray);
        CommentAttri.Style:= IfThen(ReadInteger(SEC_HTML, KEY_COMMENT_BOLD, 0) <> 0, [fsBold], []);
        IdentifierAttri.Foreground:= ReadInteger(SEC_HTML, KEY_IDENTIFIER_COLOR, clBlack);
        IdentifierAttri.Style:= IfThen(ReadInteger(SEC_HTML, KEY_IDENTIFIER_BOLD, 0) <> 0, [fsBold], []);
        KeyAttri.Foreground:= ReadInteger(SEC_HTML, KEY_KEY_COLOR, $00ff0080);
        KeyAttri.Style:= IfThen(ReadInteger(SEC_HTML, KEY_KEY_BOLD, 0) <> 0, [fsBold], []);
        SpaceAttri.Foreground:= ReadInteger(SEC_HTML, KEY_SPACE_COLOR, clWhite);
        SymbolAttri.Foreground:= ReadInteger(SEC_HTML, KEY_SYMBOL_COLOR, clGray);
        SymbolAttri.Style:= IfThen(ReadInteger(SEC_HTML, KEY_SYMBOL_BOLD, 0) <> 0, [fsBold], []);
        TextAttri.Foreground:= ReadInteger(SEC_HTML, KEY_TEXT_COLOR, clBlack);
        TextAttri.Style := IfThen(ReadInteger(SEC_HTML, KEY_TEXT_BOLD, 0) <> 0, [fsBold], []);
        UndefKeyAttri.Foreground:= ReadInteger(SEC_HTML, KEY_UNDEF_KEY_COLOR, clRed);
        UndefKeyAttri.Style:= IfThen(ReadInteger(SEC_HTML, KEY_UNDEF_KEY_BOLD, 0) <> 0, [fsBold], []);
        ValueAttri.Foreground:= ReadInteger(SEC_HTML, KEY_VALUE_COLOR, $00ff8000);
        ValueAttri.Style:= IfThen(ReadInteger(SEC_HTML, KEY_VALUE_BOLD, 0) <> 0, [fsBold], []);
      end;
    end else if (FEditor.Highlighter = FHighlightCss) then begin
      FEditor.Color:= ReadInteger(SEC_CSS, KEY_BACKGROUND, clWhite);
      with FHighlightCss do begin
        CommentAttri.Foreground:= ReadInteger(SEC_CSS, KEY_COMMENT_COLOR, clGreen);
        CommentAttri.Style:= IfThen(ReadInteger(SEC_CSS, KEY_COMMENT_BOLD, 0) <> 0, [fsBold], []);
        IdentifierAttri.Foreground:= ReadInteger(SEC_CSS, KEY_IDENTIFIER_COLOR, clBlack);
        IdentifierAttri.Style:= IfThen(ReadInteger(SEC_CSS, KEY_IDENTIFIER_BOLD, 0) <> 0, [fsBold], []);
        KeyAttri.Foreground:= ReadInteger(SEC_CSS, KEY_KEY_COLOR, clBlue);
        KeyAttri.Style:= IfThen(ReadInteger(SEC_CSS, KEY_KEY_BOLD, 0) <> 0, [fsBold], []);
        NumberAttri.Foreground:= ReadInteger(SEC_CSS, KEY_NUMBER_COLOR, clBlack);
        NumberAttri.Style:= IfThen(ReadInteger(SEC_CSS, KEY_NUMBER_BOLD, 0) <> 0, [fsBold], []);
        SpaceAttri.Foreground:= ReadInteger(SEC_CSS, KEY_SPACE_COLOR, clWhite);
        StringAttri.Foreground:= ReadInteger(SEC_CSS, KEY_STRING_COLOR, clMaroon);
        StringAttri.Style:= IfThen(ReadInteger(SEC_CSS, KEY_STRING_BOLD, 0) <> 0, [fsBold], []);
        SymbolAttri.Foreground:= ReadInteger(SEC_CSS, KEY_SYMBOL_COLOR, clGray);
        SymbolAttri.Style:= IfThen(ReadInteger(SEC_CSS, KEY_SYMBOL_BOLD, 0) <> 0, [fsBold], []);
        MeasurementUnitAttri.Foreground:= ReadInteger(SEC_CSS, KEY_MEASURE_COLOR, clBlack);
        MeasurementUnitAttri.Style:= IfThen(ReadInteger(SEC_CSS, KEY_MEASURE_BOLD, 0) <> 0, [fsBold], []);
        SelectorAttri.Foreground:= ReadInteger(SEC_CSS, KEY_SELECTOR_COLOR, clBlack);
        SelectorAttri.Style:= IfThen(ReadInteger(SEC_CSS, KEY_SELECTOR_BOLD, 0) <> 0, [fsBold], []);
      end;
    end else if (FEditor.Highlighter = FHighlightJs) then begin
      FEditor.Color:= ReadInteger(SEC_JS, KEY_BACKGROUND, clWhite);
      with FHighlightJs do begin
        CommentAttri.Foreground:= ReadInteger(SEC_JS, KEY_COMMENT_COLOR, clGreen);
        CommentAttri.Style:= IfThen(ReadInteger(SEC_JS, KEY_COMMENT_BOLD, 0) <> 0, [fsBold], []);
        IdentifierAttri.Foreground:= ReadInteger(SEC_JS, KEY_IDENTIFIER_COLOR, clBlack);
        IdentifierAttri.Style:= IfThen(ReadInteger(SEC_JS, KEY_IDENTIFIER_BOLD, 0) <> 0, [fsBold], []);
        KeyAttri.Foreground:= ReadInteger(SEC_JS, KEY_KEY_COLOR, clBlue);
        KeyAttri.Style:= IfThen(ReadInteger(SEC_JS, KEY_KEY_BOLD, 0) <> 0, [fsBold], []);
        NonReservedKeyAttri.Foreground:= ReadInteger(SEC_JS, KEY_NON_RESERVED_KEY_COLOR, clNavy);
        NonReservedKeyAttri.Style:= IfThen(ReadInteger(SEC_JS, KEY_NON_RESERVED_KEY_BOLD, 0) <> 0, [fsBold], []);
        EventAttri.Foreground:= ReadInteger(SEC_JS, KEY_EVENT_COLOR, clBlack);
        EventAttri.Style:= IfThen(ReadInteger(SEC_JS, KEY_ELEMENT_BOLD, 0) <> 0, [fsBold], []);
        NumberAttri.Foreground:= ReadInteger(SEC_JS, KEY_NUMBER_COLOR, clBlack);
        NumberAttri.Style:= IfThen(ReadInteger(SEC_JS, KEY_NUMBER_BOLD, 0) <> 0, [fsBold], []);
        SpaceAttri.Foreground:= ReadInteger(SEC_JS, KEY_SPACE_COLOR, clWhite);
        StringAttri.Foreground:= ReadInteger(SEC_JS, KEY_STRING_COLOR, clMaroon);
        StringAttri.Style:= IfThen(ReadInteger(SEC_JS, KEY_STRING_BOLD, 0) <> 0, [fsBold], []);
        SymbolAttri.Foreground:= ReadInteger(SEC_JS, KEY_SYMBOL_COLOR, clGray);
        SymbolAttri.Style:= IfThen(ReadInteger(SEC_JS, KEY_SYMBOL_BOLD, 0) <> 0, [fsBold], []);
      end;
    end else if (FEditor.Highlighter = FHighlightShell) then begin
      FEditor.Color:= ReadInteger(SEC_SHELL, KEY_BACKGROUND, clWhite);
      with FHighlightShell do begin
        CommentAttri.Foreground:= ReadInteger(SEC_SHELL, KEY_COMMENT_COLOR, clGreen);
        CommentAttri.Style:= IfThen(ReadInteger(SEC_SHELL, KEY_COMMENT_BOLD, 0) <> 0, [fsBold], []);
        IdentifierAttri.Foreground:= ReadInteger(SEC_SHELL, KEY_IDENTIFIER_COLOR, clBlack);
        IdentifierAttri.Style:= IfThen(ReadInteger(SEC_SHELL, KEY_IDENTIFIER_BOLD, 0) <> 0, [fsBold], []);
        KeyAttri.Foreground:= ReadInteger(SEC_SHELL, KEY_KEY_COLOR, clBlue);
        KeyAttri.Style:= IfThen(ReadInteger(SEC_SHELL, KEY_KEY_BOLD, 0) <> 0, [fsBold], []);
        SecondKeyAttri.Foreground:= ReadInteger(SEC_SHELL, KEY_SECOND_KEY_COLOR, clNavy);
        SecondKeyAttri.Style:= IfThen(ReadInteger(SEC_SHELL, KEY_SECOND_KEY_BOLD, 0) <> 0, [fsBold], []);
        NumberAttri.Foreground:= ReadInteger(SEC_SHELL, KEY_NUMBER_COLOR, clBlack);
        NumberAttri.Style:= IfThen(ReadInteger(SEC_SHELL, KEY_NUMBER_BOLD, 0) <> 0, [fsBold], []);
        SpaceAttri.Foreground:= ReadInteger(SEC_SHELL, KEY_SPACE_COLOR, clWhite);
        StringAttri.Foreground:= ReadInteger(SEC_SHELL, KEY_STRING_COLOR, clMaroon);
        StringAttri.Style:= IfThen(ReadInteger(SEC_SHELL, KEY_STRING_BOLD, 0) <> 0, [fsBold], []);
        SymbolAttri.Foreground:= ReadInteger(SEC_SHELL, KEY_SYMBOL_COLOR, clGray);
        SymbolAttri.Style:= IfThen(ReadInteger(SEC_SHELL, KEY_SYMBOL_BOLD, 0) <> 0, [fsBold], []);
        VarAttri.Foreground:= ReadInteger(SEC_SHELL, KEY_VAR_COLOR, clOlive);
        VarAttri.Style:= IfThen(ReadInteger(SEC_SHELL, KEY_VAR_BOLD, 0) <> 0, [fsBold], []);
      end;
    end;
    Free;
  end;
end;

procedure TTextCodeView.SetStyleTheme;
begin
  ThemeUtils.RecolorButton(FFindBtn);
  ThemeUtils.RecolorButton(FFindBtnNext);
  ThemeUtils.RecolorButton(FFindBtnPrior);
  ThemeUtils.RecolorButton(FFindBtnClose);
  ThemeUtils.RecolorEdit(FFindEdit);

  ThemeUtils.RecolorButton(FReplaceFindBtnClose);
  ThemeUtils.RecolorButton(FReplaceBtnReplace);
  ThemeUtils.RecolorButton(FReplaceBtnReplaceAll);
  ThemeUtils.RecolorEdit(FReplaceFindEdit);
  ThemeUtils.RecolorEdit(FReplaceEdit);

  ThemeUtils.RecolorSynEdit(FEditor);

end;

procedure TTextCodeView.FocusEditor;
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

procedure TTextCodeView.LoadShortcut;
begin
  // load shortcut
end;

end.

