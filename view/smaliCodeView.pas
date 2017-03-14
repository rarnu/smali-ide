unit smaliCodeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, ComCtrls, Controls, Graphics, SynEdit, SynGutterBase, SynGutterLineNumber, SynGutter, SynGutterCodeFolding, Menus, LCLType, Dialogs, Forms,
  SynEditTypes, synhighlightersmali, SynCompletion, SynEditKeyCmds, codeViewIntf, IniFiles, synhighlighterdodolasmali, Buttons, SynEditMouseCmds;

type
  { TSmaliCodeView }

  TSmaliCodeView = class(TTabSheet, ICodeViewIntf)
  private
    FEditor: TSynEdit;
    FEditorSSmali: TSynEdit;
    FSplitSsmali: TSplitter;

    FHighlighter: TSynSmaliSyn;
    FhighlighterSSmali: TSynDodolaSmaliSyn;
    FCompleteSmali: TSynCompletion;
    FCompleteClass: TSynCompletion;
    FCompleteTemplate: TSynCompletion;

    FFileName: string;
    FIsChanged: Boolean;
    FMenu: TPopupMenu;
    FOnCodeJump: TOnCodeJump;
    FProjectPath: string;
    FTitle: string;
    // menu items
    FMiJump: TMenuItem;
    FMiS0: TMenuItem;
    FMiUndo: TMenuItem;
    FMiRedo: TMenuItem;
    FMiS1: TMenuItem;
    FMiCut: TMenuItem;
    FMiCopy: TMenuItem;
    FMiPaste: TMenuItem;
    FMiDelete: TMenuItem;
    FMiS2: TMenuItem;
    FMiToJava: TMenuItem;

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
    procedure completeClassCompletion(var Value: string; SourceValue: string;
      var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure completeClassExecute(Sender: TObject);
    procedure completeClassCancel(Sender: TObject);
    procedure completeClassKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure completeClassKeyPress(Sender: TObject; var Key: char);

    procedure completeSmaliCancel(Sender: TObject);
    procedure completeSmaliCompletion(var Value: string; SourceValue: string;
      var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure completeSmaliKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure completeSmaliKeyPress(Sender: TObject; var Key: char);
    procedure completeTemplateCancel(Sender: TObject);
    procedure completeTemplateCompletion(var Value: string;
      SourceValue: string; var SourceStart, SourceEnd: TPoint;
      KeyChar: TUTF8Char; Shift: TShiftState);
    procedure completeTemplateExecute(Sender: TObject);
    procedure completeTemplateKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure completeTemplateKeyPress(Sender: TObject; var Key: char);
    function GetEditor: TSynEdit;
    function GetFileName: string;

    procedure OnEditorChange(Sender: TObject);
    procedure OnLinkClicked(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetFileName(AValue: string);
    function FindSmaliString(aset: TCharSet): string;
    function FindClassToJump(): string;
    function FindMethodToJump(): string;
    procedure menuClicked(sender: TObject);
  protected

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
    procedure FocusEditor();
    function FindMethodAndJump(methodSig: string): Boolean;
    procedure SetCodeTheme(AThemeFile: string);
    procedure SetStyleTheme;
    procedure LoadShortcut();
    procedure ShowSSmali(AShow: Boolean);
  published
    property ProjectPath: string read FProjectPath write FProjectPath;
    property FileName: string read GetFileName write SetFileName;
    property Editor: TSynEdit read GetEditor;
    property Menu: TPopupMenu read FMenu write FMenu;
    property IsChanged: Boolean read FIsChanged;
    property OnCodeJump: TOnCodeJump read FOnCodeJump write FOnCodeJump;
  end;

implementation

uses
  TextUtils, EncryptUtils, CodeUtils, baseData, config, frmJava, ThemeUtils;

{ TSmaliCodeView }

procedure TSmaliCodeView.SetFileName(AValue: string);
var
  p: string;
begin
  FFileName:=AValue;
  FTitle:= ExtractFileName(FFileName);
  Caption:= FTitle;
  FEditor.Lines.LoadFromFile(FFileName);
  // load ssmali
  p := CodeUtils.FullPathToClassPath(FFileName);
  p := CodeUtils.FindSsmaliFile(ProjectPath, p);
  if (FileExists(p)) then FEditorSSmali.Lines.LoadFromFile(p);
end;

function TSmaliCodeView.FindSmaliString(aset: TCharSet): string;
var
  s: string = '';
  idx: Integer;
  c: Char;
begin
  // find class to jump
  idx:= FEditor.SelStart;
  while idx > 0 do begin
    c := FEditor.Lines.Text[idx];
    if (c in aset) then begin
      if (c = ';') then Break;
      s := c + s;
    end else begin
      Break;
    end;
    Dec(idx);
  end;
  idx := FEditor.SelStart + 1;
  while idx < FEditor.Lines.Text.Length do begin
    c := FEditor.Lines.Text[idx];
    if (c in aset) then begin
      s := s + c;
      if (c = ';') then Break;
    end else begin
      Break;
    end;
    Inc(idx);
  end;
  Result := s;
end;

function TSmaliCodeView.FindClassToJump: string;
const
  CLASS_CHARS: TCharSet = ['A'..'Z', 'a'..'z', '0'..'9', '/', '_', '$', ';'];
begin
  Result := FindSmaliString(CLASS_CHARS);
end;

function TSmaliCodeView.FindMethodToJump: string;
const
  CLASS_CHARS: TCharSet = ['A'..'Z', 'a'..'z', '0'..'9', '/', ';', '_', '$', '-', '<', '>', '(', ')', '[', ':'];
begin
  Result := FindSmaliString(CLASS_CHARS);
end;

procedure TSmaliCodeView.OnEditorChange(Sender: TObject);
begin
  FIsChanged := True;
  Caption:= FTitle + ' *';
end;

procedure TSmaliCodeView.OnLinkClicked(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FEditor.SelectWord;
  menuClicked(FMiJump);
end;

procedure TSmaliCodeView.btnClicked(Sender: TObject);
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

procedure TSmaliCodeView.completeClassCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
begin
  if (FCompleteClass.Tag = 0) then Value:= 'L' + Value.Replace('.', '/') + ';';
  FCompleteClass.Deactivate;
end;

procedure TSmaliCodeView.completeClassExecute(Sender: TObject);
var
  i: Integer;
  str: string = '';
  cls: string = '';
  indexPath: string;
begin
  FCompleteClass.ItemList.Clear;
  // complete class execute
  for i := FEditor.SelStart - 1 downto 1 do begin
    str := FEditor.Lines.Text[i] + str;
    if (str.Length = 2) then Break;
  end;
  if (str = '->') then begin
    // hint method and field
    FCompleteClass.Tag:= 1;
    for i := FEditor.SelStart - 3 downto 1 do begin
      if (not (FEditor.Lines.Text[i] in ['a'..'z', 'A'..'Z', '0'..'9', '/', ';', '$', '_'])) then Break;
      cls := FEditor.Lines.Text[i] + cls;
    end;
    cls := cls.Substring(1, cls.Length - 2).Replace('/', '.');
    indexPath:= CodeUtils.ClassIndexToFilePath(ProjectPath, cls);
    if (FileExists(indexPath)) then begin
      CodeUtils.BuildMethodIndex(ProjectPath, indexPath);
      FCompleteClass.ItemList.Text:= CodeUtils.LoadMethodIndex(ProjectPath, indexPath);
      FCompleteClass.Position:= 0;
    end;
  end else begin
    FCompleteClass.Tag:= 0;
    indexPath := ExtractFilePath(ParamStr(0)) + 'index' + SPLIT + md5EncryptString(FProjectPath);
    if (FileExists(indexPath + SPLIT + 'index')) then begin
      FCompleteClass.ItemList.LoadFromFile(indexPath + SPLIT + 'index');
      FCompleteClass.Position:= 0;
    end;
  end;
end;

procedure TSmaliCodeView.menuClicked(sender: TObject);
var
  c: string;
  methodSig: string;
  javaCode: string;
begin
  if (sender = FMiJump) then begin
    c := FindClassToJump();
    if (c <> '') and (c.StartsWith('L')) and (c.EndsWith(';')) then begin
      c := c.Substring(1, c.Length - 2);
      if (Assigned(FOnCodeJump)) then begin
        FOnCodeJump(Self, c, '', 0);
      end;
    end else begin
      c := FindMethodToJump();
      if (c <> '') and (c.StartsWith('L')) and (c.Contains('->')) then begin
        methodSig:= c.Substring(c.IndexOf('->') + 2);
        c := c.Substring(0, c.IndexOf('->'));
        c := c.Substring(1, c.Length - 2);
        if (Assigned(FOnCodeJump)) then begin
          FOnCodeJump(Self, c, methodSig, 1);
        end;
      end;
    end;
  end else if (sender = FMiUndo) then begin
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
  end else if (sender = FMiToJava) then begin
    // to java
    javaCode:= CodeUtils.ConvertSmaliToJava(ProjectPath, FileName);
    if (javaCode.Trim <> '') then begin
      with TFormJava.Create(nil) do begin
        Code:= javaCode;
        ShowModal;
        Free;
      end;
    end;
  end;
end;

procedure TSmaliCodeView.completeSmaliCancel(Sender: TObject);
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

procedure TSmaliCodeView.completeSmaliCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
begin
  FCompleteSmali.Deactivate;
end;

procedure TSmaliCodeView.completeSmaliKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = VK_ESCAPE) then FCompleteSmali.Deactivate;
end;

procedure TSmaliCodeView.completeSmaliKeyPress(Sender: TObject; var Key: char);
begin
  if (Ord(Key) = VK_ESCAPE) then FCompleteSmali.Deactivate;
end;

procedure TSmaliCodeView.completeTemplateCancel(Sender: TObject);
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

procedure TSmaliCodeView.completeTemplateCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
var
  p: string;
begin
  p := ExtractFilePath(ParamStr(0)) + 'template' + SPLIT + 'custom' + SPLIT + Value + '.template';
  with TStringList.Create do begin
    LoadFromFile(p);
    Value:= Text;
    Free;
  end;
  Value:= Value.Trim;
  FCompleteTemplate.Deactivate;
end;

procedure TSmaliCodeView.completeTemplateExecute(Sender: TObject);
var
  p: string;
  src: TSearchRec;
begin
  FCompleteTemplate.ItemList.Clear;
  p := ExtractFilePath(ParamStr(0)) + 'template' + SPLIT + 'custom' + SPLIT;
  if (FindFirst(p + '*.template', faAnyFile, src) = 0) then begin
    repeat
      if (src.Name = '.') or (src.Name = '..') then Continue;
      FCompleteTemplate.ItemList.Add(string(src.Name).Replace('.template', '', [rfIgnoreCase, rfReplaceAll]));
    until SysUtils.FindNext(src) <> 0;
    FindClose(src);
  end;
end;

procedure TSmaliCodeView.completeTemplateKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (key = VK_ESCAPE) then FCompleteTemplate.Deactivate;
end;

procedure TSmaliCodeView.completeTemplateKeyPress(Sender: TObject; var Key: char
  );
begin
  if (Ord(Key) = VK_ESCAPE) then FCompleteTemplate.Deactivate;
end;

function TSmaliCodeView.GetEditor: TSynEdit;
begin
  Result := FEditor;
end;

function TSmaliCodeView.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TSmaliCodeView.completeClassKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = VK_ESCAPE) then FCompleteClass.Deactivate;
end;

procedure TSmaliCodeView.completeClassKeyPress(Sender: TObject; var Key: char);
begin
  if (Ord(Key) = VK_ESCAPE) then FCompleteClass.Deactivate;
end;

procedure TSmaliCodeView.completeClassCancel(Sender: TObject);
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

constructor TSmaliCodeView.Create(TheOwner: TComponent);
var
  i: integer;
  part: TSynGutterPartBase;
  smaliCmdPath: string;
  smaliCmd: string;
begin
  inherited Create(TheOwner);
  FEditor := TSynEdit.Create(Self);
  FHighlighter := TSynSmaliSyn.Create(Self);
  FhighlighterSSmali := TSynDodolaSmaliSyn.Create(Self);
  FCompleteSmali:= TSynCompletion.Create(Self);
  FCompleteClass := TSynCompletion.Create(Self);
  FCompleteTemplate:= TSynCompletion.Create(Self);

  with TStringList.Create do begin
    smaliCmdPath:= ExtractFilePath(ParamStr(0)) + 'template' + SPLIT + 'smalicmd';
    if (FileExists(smaliCmdPath)) then begin
      LoadFromFile(smaliCmdPath);
      smaliCmd:= Text;
    end;
    Free;
  end;
  with FCompleteSmali do begin
    ExecCommandID:= ecUserDefinedFirst;
    ItemList.Text:= smaliCmd;
    ShowSizeDrag:= True;
    EndOfTokenChr:= ';';
    Editor := FEditor;
    ShortCut:= Menus.ShortCut(VK_J, [ssCtrl]);
    OnCodeCompletion:=@completeSmaliCompletion;
    OnCancel:=@completeSmaliCancel;
    OnKeyPress:=@completeSmaliKeyPress;
    OnKeyDown:=@completeSmaliKeyDown;
  end;
  with FCompleteClass do begin
    ExecCommandID:= ecUserDefinedFirst + 1;
    ShowSizeDrag:= True;
    EndOfTokenChr:= ';';
    Editor := FEditor;
    ShortCut:= Menus.ShortCut(VK_K, [ssCtrl]);
    OnExecute:=@completeClassExecute;
    OnCodeCompletion:=@completeClassCompletion;
    OnCancel:=@completeClassCancel;
    OnKeyPress:=@completeClassKeyPress;
    OnKeyDown:=@completeClassKeyDown;
  end;
  with FCompleteTemplate do begin
    ExecCommandID:= ecUserDefinedFirst + 2;
    ShowSizeDrag:= True;
    EndOfTokenChr:= ';';
    Editor := FEditor;
    ShortCut:= Menus.ShortCut(VK_L, [ssCtrl]);
    OnExecute:=@completeTemplateExecute;
    OnCancel:=@completeTemplateCancel;
    OnKeyPress:=@completeTemplateKeyPress;
    OnKeyDown:=@completeTemplateKeyDown;
    OnCodeCompletion:=@completeTemplateCompletion;
  end;

  with FEditor do begin
    Parent := Self;
    Align:= alClient;
    Color:= clWhite;
    MouseOptions:= MouseOptions + [emShowCtrlMouseLinks];
    Options:= Options + [eoKeepCaretX, eoShowCtrlMouseLinks] - [eoAutoIndent, eoScrollPastEol, eoSmartTabs];
    RightGutter.Visible:= False;
    ScrollBars:= ssAutoBoth;
    TabWidth:= 4;
    OnChange:=@OnEditorChange;
    OnClickLink:=@OnLinkClicked;
    Highlighter := FHighlighter;
  end;

  FEditorSSmali:= TSynEdit.Create(Self);
  with FEditorSSmali do begin
    Parent := Self;
    Align:= alRight;
    Color:= clWhite;
    Options:= Options + [eoKeepCaretX] - [eoAutoIndent, eoScrollPastEol, eoSmartTabs];
    RightGutter.Visible:= False;
    ScrollBars:= ssAutoBoth;
    TabWidth:= 4;
    ReadOnly:= True;
    Width:= 300;
    Highlighter := FhighlighterSSmali;
  end;

  FSplitSsmali:= TSplitter.Create(Self);
  with FSplitSsmali do begin
    Parent := Self;
    Align:= alRight;
    Width:= 3;
    Left:= FEditorSSmali.Left - Width;
  end;

  FMenu := TPopupMenu.Create(Self);
  FMenu.Parent := Self;
  FEditor.PopupMenu := FMenu;

  // init menu
  FMiJump:= TMenuItem.Create(FMenu);
  FMiJump.Caption:= 'Jump';
  FMiJump.ShortCut:= ShortCut(VK_F2, []);
  FMiJump.OnClick:= @menuClicked;
  FMiS0:= TMenuItem.Create(FMenu);
  FMiS0.Caption:= '-';
  FMiUndo:= TMenuItem.Create(FMenu);
  FMiUndo.Caption:= 'Undo';
  FMiUndo.ShortCut:= ShortCut(VK_Z, [ssCtrl]);
  FMiUndo.OnClick:= @menuClicked;
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
  FMiS2:= TMenuItem.Create(FMenu);
  FMiS2.Caption:= '-';
  FMiToJava:= TMenuItem.Create(FMenu);
  FMiToJava.Caption:= 'To Java';
  FMiToJava.ShortCut:= ShortCut(VK_RETURN, [ssAlt]);
  FMiToJava.OnClick:= @menuClicked;

  with FMenu.Items do begin
    Add(FMiJump);
    Add(FMiS0);
    Add(FMiUndo);
    Add(FMiRedo);
    Add(FMiS1);
    Add(FMiCut);
    Add(FMiCopy);
    Add(FMiPaste);
    Add(FMiDelete);
    Add(FMiS2);
    Add(FMiToJava);
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
  ShowSSmali(GlobalConfig.ShowSSmali);
end;

destructor TSmaliCodeView.Destroy;
begin
  FEditor.Free;
  inherited Destroy;
end;

function TSmaliCodeView.QueryClose: Boolean;
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

procedure TSmaliCodeView.Save;
begin
  FEditor.Lines.SaveToFile(FileName);
  FIsChanged := False;
  Caption:= FTitle;
end;

procedure TSmaliCodeView.SaveAs;
var
  dlg: TSaveDialog;
begin
  dlg := TSaveDialog.Create(nil);
  dlg.Filter:= 'smali file|*.smali';
  if dlg.Execute then begin
    FEditor.Lines.SaveToFile(dlg.FileName);
    self.FileName:= dlg.FileName;
    FIsChanged := False;
  end;
  dlg.Free;
end;

procedure TSmaliCodeView.Find;
begin
  FPnlReplace.Visible:= False;
  FPnlFind.Visible:= True;
  FFindEdit.SetFocus;
end;

procedure TSmaliCodeView.FindNext;
begin
  FFindBtnNext.Click;
end;

procedure TSmaliCodeView.CancelFind;
begin
  FFindEdit.Text:= '';
  FPnlFind.Visible:= False;
  FEditor.SetFocus;
end;

procedure TSmaliCodeView.Replace;
begin
  FPnlFind.Visible:= False;
  FPnlReplace.Visible:= True;
  FReplaceFindEdit.SetFocus;
end;

procedure TSmaliCodeView.CancelReplace;
begin
  FReplaceFindEdit.Text:= '';
  FReplaceEdit.Text:= '';
  FPnlReplace.Visible:= False;
  FEditor.SetFocus;
end;

procedure TSmaliCodeView.GotoLine(line: Integer);
begin
  // goto line
  FEditor.CaretY:= line;
  FEditor.SetFocus;
end;

procedure TSmaliCodeView.GotoPosition(apos: Integer);
begin
  FEditor.SelStart:= apos + 1;
end;

procedure TSmaliCodeView.FocusEditor;
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

function TSmaliCodeView.FindMethodAndJump(methodSig: string): Boolean;
var
  r: Integer;
  start: Integer;
  hit: Boolean = False;
  s: string;
  i: integer;
begin
  // find method and jump
  Result := False;
  FEditor.SelStart:= 1;
  while not hit do begin
    r := FEditor.SearchReplace(methodSig, '', []);
    s := '';
    if (r <> 0) then begin
      start:= FEditor.SelStart;
      for i := start downto 0 do begin
        if (FEditor.Lines.Text[i] = #13) or (FEditor.Lines.Text[i] = #10) then begin
          Break;
        end;
        s := FEditor.Lines.Text[i] + s;
      end;
      if (s.Trim.StartsWith('.method')) or (s.Trim.StartsWith('.field')) then begin
        Result := True;
        Break;
      end;
    end else begin
      Break;
    end;
  end;
end;

function IfThen(b: Boolean; trueValue: TFontStyles; falseValue: TFontStyles): TFontStyles;
begin
  if b then Result := trueValue else Result := falseValue;
end;

procedure TSmaliCodeView.SetCodeTheme(AThemeFile: string);
var
  path: string;
begin
  // set code theme
  path := ExtractFilePath(ParamStr(0)) + 'style' + SPLIT + AThemeFile;
  with TIniFile.Create(path) do begin
    FEditor.Color:= ReadInteger(SEC_SMALI, KEY_BACKGROUND, clWhite);
    with FHighlighter, FhighlighterSSmali do begin
      CommentAttri.Foreground:= ReadInteger(SEC_SMALI, KEY_COMMENT_COLOR, clGreen);
      CommentAttri.Style:= IfThen(ReadInteger(SEC_SMALI, KEY_COMMENT_BOLD, 0) <> 0, [fsBold], []);
      IdentifierAttri.Foreground:= ReadInteger(SEC_SMALI, KEY_IDENTIFIER_COLOR, clBlack);
      IdentifierAttri.Style:= IfThen(ReadInteger(SEC_SMALI, KEY_IDENTIFIER_BOLD, 0) <> 0, [fsBold], []);
      KeyAttri.Foreground:= ReadInteger(SEC_SMALI, KEY_KEY_COLOR, clBlue);
      KeyAttri.Style:= IfThen(ReadInteger(SEC_SMALI, KEY_KEY_BOLD, 0) <> 0, [fsBold], []);
      SecondKeyAttri.Foreground:= ReadInteger(SEC_SMALI, KEY_SECOND_KEY_COLOR, clNavy);
      SecondKeyAttri.Style:= IfThen(ReadInteger(SEC_SMALI, KEY_SECOND_KEY_BOLD, 0) <> 0, [fsBold], []);
      ThirdKeyAttri.Foreground:= ReadInteger(SEC_SMALI, KEY_THIRD_KEY_COLOR, clPurple);
      ThirdKeyAttri.Style:= IfThen(ReadInteger(SEC_SMALI, KEY_THIRD_KEY_BOLD, 0) <> 0, [fsBold], []);
      NumberAttri.Foreground:= ReadInteger(SEC_SMALI, KEY_NUMBER_COLOR, clBlack);
      NumberAttri.Style := IfThen(ReadInteger(SEC_SMALI, KEY_NUMBER_BOLD, 0) <> 0, [fsBold], []);
      SpaceAttri.Foreground:= ReadInteger(SEC_SMALI, KEY_SPACE_COLOR, clWhite);
      StringAttri.Foreground:= ReadInteger(SEC_SMALI, KEY_STRING_COLOR, clMaroon);
      StringAttri.Style:= IfThen(ReadInteger(SEC_SMALI, KEY_STRING_BOLD, 0) <> 0, [fsBold], []);
      SymbolAttri.Foreground:= ReadInteger(SEC_SMALI, KEY_SYMBOL_COLOR, clGray);
      SymbolAttri.Style:= IfThen(ReadInteger(SEC_SMALI, KEY_SYMBOL_BOLD, 0) <> 0, [fsBold], []);
      VarAttri.Foreground:= ReadInteger(SEC_SMALI, KEY_VAR_COLOR, clBlack);
      VarAttri.Style:= IfThen(ReadInteger(SEC_SMALI, KEY_VAR_BOLD, 0) <> 0, [fsBold], []);
    end;

    Free;
  end;
end;

procedure TSmaliCodeView.SetStyleTheme;
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
  ThemeUtils.RecolorSynEdit(FEditorSSmali);
  ThemeUtils.RecolorCompleter(FCompleteSmali);
  ThemeUtils.RecolorCompleter(FCompleteClass);
  ThemeUtils.RecolorCompleter(FCompleteTemplate);
end;

procedure TSmaliCodeView.LoadShortcut;
begin
  // load shortcut
  FMiJump.ShortCut:= GlobalConfig.JumpClassMethod;
  FMiToJava.ShortCut:= GlobalConfig.JumpToJava;
  FCompleteSmali.ShortCut:= GlobalConfig.HintKeyword;
  FCompleteClass.ShortCut:= GlobalConfig.HintClassMethod;
  FCompleteTemplate.ShortCut:= GlobalConfig.HintTemplate;
end;

procedure TSmaliCodeView.ShowSSmali(AShow: Boolean);
begin
  FEditorSSmali.Visible:= Ashow;
  FSplitSsmali.Visible:= AShow;
end;

end.

