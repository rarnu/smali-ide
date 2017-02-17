unit frmSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, frmBase, LCLType, LCLProc, Menus;

type

  { TFormSettings }

  TFormSettings = class(TFormBase)
    btnChooseJava: TButton;
    btnChooseCurl: TButton;
    btnCodeEditorClassMethodCompletion: TButton;
    btnCodeEditorJumpClassMethod: TButton;
    btnHelpSettings: TButton;
    btnCodeEditorKeywordsCompletion: TButton;
    btnCodeTreeNewInterface: TButton;
    btnCodeTreeNewClass: TButton;
    btnCodeTreeNewEnum: TButton;
    btnCodeTreeDeleteFile: TButton;
    btnCodeEditorSmaliToJava: TButton;
    btnCodeTreeNewTextFile: TButton;
    btnCodeTreeNewAnnotation: TButton;
    btnCodeEditorTemplateCompletion: TButton;
    btnViewClassIndex: TButton;
    btnPackageDecompile: TButton;
    btnPackageInstallFramework: TButton;
    btnViewSearchResult: TButton;
    btnViewConsole: TButton;
    btnViewCloseAllPages: TButton;
    btnViewCloseAllOtherPages: TButton;
    btnPackageCompile: TButton;
    edtJavaPath: TEdit;
    edtCurlPath: TEdit;
    gbCodeEditor: TGroupBox;
    gbHelp: TGroupBox;
    gbJava: TGroupBox;
    gbCurl: TGroupBox;
    gbCodeTree: TGroupBox;
    gbView: TGroupBox;
    gbPackage: TGroupBox;
    lblchooseJava: TLabel;
    lblChooseCurl: TLabel;
    lblCodeEditorClassMethodCompletion: TLabel;
    lblCodeEditorJumpClassMethod: TLabel;
    lblHelpSettings: TLabel;
    lblCodeEditorKeywordsCompletion: TLabel;
    lblCodeEditorSmaliToJava: TLabel;
    lblCodeEditorTemplateCompletion: TLabel;
    lblDefaultCurl: TLabel;
    lblJavaStatus: TLabel;
    lblDefaultJava: TLabel;
    lblCurlStatus: TLabel;
    lblCodeTreeNewInterface: TLabel;
    lblCodeTreeNewClass: TLabel;
    lblCodeTreeNewEnum: TLabel;
    lblCodeTreeDeleteFile: TLabel;
    lblCodeTreeNewTextFile: TLabel;
    lblCodeTreeNewAnnotation: TLabel;
    lblViewClassIndex: TLabel;
    lblPackageDecompile: TLabel;
    lblPackageInstallFramework: TLabel;
    lblViewSearchResult: TLabel;
    lblViewConsole: TLabel;
    lblViewCloseAllPages: TLabel;
    lblViewCloseAllOtherPages: TLabel;
    lblPackageCompile: TLabel;
    pnlCodeEditorClassMethodCompletion: TPanel;
    pnlCodeEditorJumpClassMethod: TPanel;
    pnlHelpSettings: TPanel;
    pnlCodeEditorKeywordsCompletion: TPanel;
    pnlCodeEditorSmaliToJava: TPanel;
    pnlCodeEditorTemplateCompletion: TPanel;
    pnlCurlChoose: TPanel;
    pnlJavaDefault: TPanel;
    pnlJavaChoose: TPanel;
    pgSettings: TPageControl;
    pnlCurlDefault: TPanel;
    pnlCodetreeNewInterface: TPanel;
    pnlCodeTreeNewClass: TPanel;
    pnlCodeTreeNewEnum: TPanel;
    pnlCodeTreeDeleteFile: TPanel;
    pnlCodeTreeNewTextFile: TPanel;
    pnlCodeTreeNewAnnotation: TPanel;
    pnlViewClassIndex: TPanel;
    pnlPackageDecompile: TPanel;
    pnlPackageInstallFramework: TPanel;
    pnlViewSearchResult: TPanel;
    pnlViewConsole: TPanel;
    pnlViewCloseAllPages: TPanel;
    pnlViewCloseAllOtherPages: TPanel;
    pnlPackageCompile: TPanel;
    ScrollBox1: TScrollBox;
    tsTemplate: TTabSheet;
    tsFileType: TTabSheet;
    tsHighlight: TTabSheet;
    tsApktool: TTabSheet;
    tsEnvironment: TTabSheet;
    tsShortcut: TTabSheet;
    procedure btnChooseCurlClick(Sender: TObject);
    procedure btnChooseJavaClick(Sender: TObject);
    procedure btnViewClassIndexClick(Sender: TObject);
  private

    STATIC_SHORTCUTS: array of TShortCut;

    function IsCanSetShortcut(AKey: string; AShortcut: TShortCut): Boolean;
    procedure LoadShortcut();
  protected
    procedure InitComponents; override;
    procedure InitEvents; override;
    procedure InitLogic; override;
  public

  end;

var
  FormSettings: TFormSettings;

implementation

uses
  config, frmShortcutAccept, baseData;

{$R *.lfm}

{ TFormSettings }

procedure TFormSettings.btnChooseJavaClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do begin
    if Execute then begin
      edtJavaPath.Text:= FileName;
    end else begin
      edtJavaPath.Text:= '/usr/bin/java';
    end;
    Free;
  end;
  GlobalConfig.JavaBinaryPath:= edtJavaPath.Text;
end;

procedure TFormSettings.btnViewClassIndexClick(Sender: TObject);
var
  btn: TButton;
  key: String;
begin
  // accept keys
  btn := TButton(Sender);
  key := btn.Hint;
  with TFormShortcutAccept.Create(nil) do begin
    if ShowModal = mrOK then begin
      if (IsCanSetShortcut(key, AcceptShortcut)) then begin
        GlobalConfig.SetShortcut(key, AcceptShortcut);
      end else begin
        // key is combined to another item
        MessageDlg('Error', 'Key is already used', mtError, [mbOK], 0);
      end;
    end else begin
      GlobalConfig.SetShortcut(key, 0);
    end;
    Free;
  end;
  LoadShortcut();
end;

function TFormSettings.IsCanSetShortcut(AKey: string; AShortcut: TShortCut
  ): Boolean;
var
  i: Integer;
begin
  Result := True;
  // is can set shortcut
  for i := 0 to Length(STATIC_SHORTCUTS) - 1 do begin
    if (AShortcut = STATIC_SHORTCUTS[i]) then begin
      Result := False;
      Break;
    end;
  end;
  if (Result) then for i := 0 to Length(KEY_SHORTCUTS) - 1 do begin
    if (AKey <> KEY_SHORTCUTS[i]) then begin
      if (AShortcut = GlobalConfig.GetShortcut(KEY_SHORTCUTS[i])) then begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

procedure TFormSettings.LoadShortcut;
begin
  // shortcuts
  btnViewClassIndex.Caption:= ShortCutToText(GlobalConfig.ShowClassIndex);
  if (string(btnViewClassIndex.Caption).Trim = '') then btnViewClassIndex.Caption:= '(none)';
  btnViewSearchResult.Caption:= ShortCutToText(GlobalConfig.ShowSearchResult);
  if (string(btnViewSearchResult.Caption).Trim = '') then btnViewSearchResult.Caption:= '(none)';
  btnViewConsole.Caption:= ShortCutToText(GlobalConfig.ShowConsole);
  if (string(btnViewConsole.Caption).Trim = '') then btnViewConsole.Caption:= '(none)';
  btnViewCloseAllPages.Caption:= ShortCutToText(GlobalConfig.CloseAllPages);
  if (string(btnViewCloseAllPages.Caption).Trim = '') then btnViewCloseAllPages.Caption:= '(none)';
  btnViewCloseAllOtherPages.Caption:= ShortCutToText(GlobalConfig.CloseAllOtherPages);
  if (string(btnViewCloseAllOtherPages.Caption).Trim = '') then btnViewCloseAllOtherPages.Caption:= '(none)';

  btnPackageDecompile.Caption:= ShortCutToText(GlobalConfig.Decompile);
  if (string(btnPackageDecompile.Caption).Trim = '') then btnPackageDecompile.Caption:= '(none)';
  btnPackageCompile.Caption:= ShortCutToText(GlobalConfig.Compile);
  if (string(btnPackageCompile.Caption).Trim = '') then btnPackageCompile.Caption:= '(none)';
  btnPackageInstallFramework.Caption:= ShortCutToText(GlobalConfig.InstallFramework);
  if (string(btnPackageInstallFramework.Caption).Trim = '') then btnPackageInstallFramework.Caption:= '(none)';

  btnCodeTreeNewClass.Caption:= ShortCutToText(GlobalConfig.NewClass);
  if (string(btnCodeTreeNewClass.Caption).Trim = '') then btnCodeTreeNewClass.Caption:= '(none)';
  btnCodeTreeNewInterface.Caption:= ShortCutToText(GlobalConfig.NewInterface);
  if (string(btnCodeTreeNewInterface.Caption).Trim = '') then btnCodeTreeNewInterface.Caption:= '(none)';
  btnCodeTreeNewEnum.Caption:= ShortCutToText(GlobalConfig.NewEnum);
  if (string(btnCodeTreeNewEnum.Caption).Trim = '') then btnCodeTreeNewEnum.Caption:= '(none)';
  btnCodeTreeNewAnnotation.Caption:= ShortCutToText(GlobalConfig.NewAnnotation);
  if (string(btnCodeTreeNewAnnotation.Caption).Trim = '') then btnCodeTreeNewAnnotation.Caption:= '(none)';
  btnCodeTreeNewTextFile.Caption:= ShortCutToText(GlobalConfig.NewTextFile);
  if (string(btnCodeTreeNewTextFile.Caption).Trim = '') then btnCodeTreeNewTextFile.Caption:= '(none)';
  btnCodeTreeDeleteFile.Caption:= ShortCutToText(GlobalConfig.DeleteFile);
  if (string(btnCodeTreeDeleteFile.Caption).Trim = '') then btnCodeTreeDeleteFile.Caption:= '(none)';

  btnCodeEditorJumpClassMethod.Caption:= ShortCutToText(GlobalConfig.JumpClassMethod);
  if (string(btnCodeEditorJumpClassMethod.Caption).Trim = '') then btnCodeEditorJumpClassMethod.Caption:= '(none)';
  btnCodeEditorSmaliToJava.Caption:= ShortCutToText(GlobalConfig.JumpToJava);
  if (string(btnCodeEditorSmaliToJava.Caption).Trim = '') then btnCodeEditorSmaliToJava.Caption:= '(none)';
  btnCodeEditorKeywordsCompletion.Caption:= ShortCutToText(GlobalConfig.HintKeyword);
  if (string(btnCodeEditorKeywordsCompletion.Caption).Trim = '') then btnCodeEditorKeywordsCompletion.Caption:= '(none)';
  btnCodeEditorClassMethodCompletion.Caption:= ShortCutToText(GlobalConfig.HintClassMethod);
  if (string(btnCodeEditorClassMethodCompletion.Caption).Trim = '') then btnCodeEditorClassMethodCompletion.Caption:= '(none)';
  btnCodeEditorTemplateCompletion.Caption:= ShortCutToText(GlobalConfig.HintTemplate);
  if (string(btnCodeEditorTemplateCompletion.Caption).Trim = '') then btnCodeEditorTemplateCompletion.Caption:= '(none)';

  btnHelpSettings.Caption:= ShortCutToText(GlobalConfig.Settings);
  if (string(btnHelpSettings.Caption).Trim = '') then btnHelpSettings.Caption:= '(none)';

end;

procedure TFormSettings.btnChooseCurlClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do begin
    if Execute then begin
      edtCurlPath.Text:= FileName;
    end else begin
      edtCurlPath.Text:= '/usr/bin/curl';
    end;
    Free;
  end;
  GlobalConfig.CurlBinaryPath:= edtCurlPath.Text;
end;

procedure TFormSettings.InitComponents;
begin
  // TODO: settings
  SetLength(STATIC_SHORTCUTS, 23);
  STATIC_SHORTCUTS[0] := ShortCut(VK_X, [ssCtrl]);
  STATIC_SHORTCUTS[1] := ShortCut(VK_C, [ssCtrl]);
  STATIC_SHORTCUTS[2] := ShortCut(VK_V, [ssCtrl]);
  STATIC_SHORTCUTS[3] := ShortCut(VK_Z, [ssCtrl]);
  STATIC_SHORTCUTS[4] := ShortCut(VK_Y, [ssCtrl]);
  STATIC_SHORTCUTS[5] := ShortCut(VK_A, [ssCtrl]);
  STATIC_SHORTCUTS[6] := ShortCut(VK_DELETE, []);

  STATIC_SHORTCUTS[7] := ShortCut(VK_F, [ssCtrl]);
  STATIC_SHORTCUTS[8] := ShortCut(VK_F3, []);
  STATIC_SHORTCUTS[9] := ShortCut(VK_R, [ssCtrl]);
  STATIC_SHORTCUTS[10] := ShortCut(VK_F, [ssCtrl, ssAlt]);
  STATIC_SHORTCUTS[11] := ShortCut(VK_G, [ssCtrl]);

  STATIC_SHORTCUTS[12] := ShortCut(VK_O, [ssCtrl]);
  STATIC_SHORTCUTS[13] := ShortCut(VK_S, [ssCtrl]);
  STATIC_SHORTCUTS[14] := ShortCut(VK_S, [ssShift, ssAlt]);
  STATIC_SHORTCUTS[15] := ShortCut(VK_S, [ssCtrl, ssAlt]);
  STATIC_SHORTCUTS[16] := ShortCut(VK_F4, [ssAlt]);

  STATIC_SHORTCUTS[17] := ShortCut(VK_F, [ssAlt]);
  STATIC_SHORTCUTS[18] := ShortCut(VK_E, [ssAlt]);
  STATIC_SHORTCUTS[19] := ShortCut(VK_V, [ssAlt]);
  STATIC_SHORTCUTS[20] := ShortCut(VK_S, [ssAlt]);
  STATIC_SHORTCUTS[21] := ShortCut(VK_P, [ssAlt]);
  STATIC_SHORTCUTS[22] := ShortCut(VK_H, [ssAlt]);
end;

procedure TFormSettings.InitEvents;
begin
  //
end;

procedure TFormSettings.InitLogic;
begin
  // java
  if (FileExists('/usr/bin/java')) then begin
    lblJavaStatus.Caption:= '(exists)';
    lblJavaStatus.Font.Color:= clDefault;
  end else begin
    lblJavaStatus.Caption:= '(not exists)';
    lblJavaStatus.Font.Color:= clRed;
  end;
  edtJavaPath.Text:= GlobalConfig.JavaBinaryPath;

  // curl
  if (FileExists('/usr/bin/curl')) then begin
    lblCurlStatus.Caption:= '(exists)';
    lblCurlStatus.Font.Color:= clDefault;
  end else begin
    lblCurlStatus.Caption:= '(not exists)';
    lblCurlStatus.Font.Color:= clRed;
  end;
  edtCurlPath.Text := GlobalConfig.CurlBinaryPath;

  LoadShortcut();
end;

end.

