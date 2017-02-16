unit textCodeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, ComCtrls, ExtCtrls, Graphics, SynEdit, SynGutterBase, SynGutterLineNumber, SynGutter, SynGutterCodeFolding, Menus, LCLType,
  SynEditTypes, Dialogs, Forms, codeViewIntf, SynHighlighterXML, SynHighlighterHTML, SynHighlighterCss, SynHighlighterJScript, synhighlighterunixshellscript, IniFiles;

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
    FFindBtn: TButton;
    FFindBtnNext: TButton;
    FFindBtnPrior: TButton;
    FFindBtnClose: TButton;

    FPnlReplace: TPanel;
    FPnlReplace1: TPanel;
    FPnlReplace2: TPanel;
    FReplaceFindTitle: TLabel;
    FReplaceFindEdit: TEdit;
    FReplaceFindMatchCase: TCheckBox;
    FReplaceFindBtnClose: TButton;
    FReplaceTitle: TLabel;
    FReplaceEdit: TEdit;
    FReplaceBtnReplace: TButton;
    FReplaceBtnReplaceAll: TButton;
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
    procedure FocusEditor();
  published
    property ProjectPath: string read FProjectPath write FProjectPath;
    property FileName: string read GetFileName write SetFileName;
    property Editor: TSynEdit read GetEditor;
    property Menu: TPopupMenu read FMenu write FMenu;
    property IsChanged: Boolean read FIsChanged;
  end;

implementation

uses
  TextUtils, baseData;

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

  FReplaceFindBtnClose:= TButton.Create(FPnlReplace1);
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

  FReplaceBtnReplace := TButton.Create(FPnlReplace2);
  FReplaceBtnReplace.Parent := FPnlReplace2;
  FReplaceBtnReplace.Align:= alLeft;
  FReplaceBtnReplace.Width:= 60;
  FReplaceBtnReplace.Left:= 310;
  FReplaceBtnReplace.BorderSpacing.Top:= 4;
  FReplaceBtnReplace.BorderSpacing.Bottom:= 4;
  FReplaceBtnReplace.Caption:= 'Replace';

  FReplaceBtnReplaceAll := TButton.Create(FPnlReplace2);
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

procedure TTextCodeView.SetCodeTheme(AThemeFile: string);
begin
  // TODO: set code theme
  (*
  FHighlightXml: TSynXMLSyn;
      FHighlightHtml: TSynHTMLSyn;
      FHighlightCss: TSynCssSyn;
      FHighlightJs: TSynJScriptSyn;
      FHighlightShell: TSynUNIXShellScriptSyn;
  *)
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

end.

