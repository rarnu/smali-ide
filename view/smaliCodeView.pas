unit smaliCodeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ComCtrls, Controls, Graphics, SynEdit, SynGutterBase, SynGutterLineNumber, SynGutter, SynGutterCodeFolding, Menus, LCLType, Dialogs, Forms;

type

  { TSmaliCodeView }

  TSmaliCodeView = class(TTabSheet)
  private
    FEditor: TSynEdit;
    FFileName: string;
    FIsChanged: Boolean;
    FMenu: TPopupMenu;
    FTitle: string;
    // menu items
    FMiUndo: TMenuItem;
    FMiRedo: TMenuItem;
    FMiS1: TMenuItem;
    FMiCut: TMenuItem;
    FMiCopy: TMenuItem;
    FMiPaste: TMenuItem;
    FMiDelete: TMenuItem;
    FMiS2: TMenuItem;
    FMiToJava: TMenuItem;
    procedure OnEditorChange(Sender: TObject);
    procedure SetFileName(AValue: string);
  protected
    procedure menuClicked(sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function QueryClose(): Boolean;
    procedure Save();
    procedure SaveAs();
  published
    property FileName: string read FFileName write SetFileName;
    property Editor: TSynEdit read FEditor write FEditor;
    property Menu: TPopupMenu read FMenu write FMenu;
    property IsChanged: Boolean read FIsChanged;
  end;

implementation

uses
  TextUtils;

{ TSmaliCodeView }

procedure TSmaliCodeView.SetFileName(AValue: string);
begin
  FFileName:=AValue;
  FTitle:= ExtractFileName(FFileName);
  Caption:= FTitle;
end;

procedure TSmaliCodeView.OnEditorChange(Sender: TObject);
begin
  FIsChanged := True;
  Caption:= FTitle + ' *';
end;

procedure TSmaliCodeView.menuClicked(sender: TObject);
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
  end else if (sender = FMiToJava) then begin
    // TODO: to java
  end;
end;

constructor TSmaliCodeView.Create(TheOwner: TComponent);
var
  i: integer;
  part: TSynGutterPartBase;

begin
  inherited Create(TheOwner);
  FEditor := TSynEdit.Create(Self);
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

end.

