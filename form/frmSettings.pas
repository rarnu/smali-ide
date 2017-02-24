unit frmSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, frmBase, SynEdit, LCLType, LCLProc, Menus, synhighlightersmali,
  SynHighlighterXML, SynHighlighterHTML, SynHighlighterCss, SynHighlighterJScript,
  synhighlighterunixshellscript, IniFiles, CommandUtils, SynHighlighterJava, SynGutter,
  SynGutterBase, SynGutterLineNumber, SynGutterCodeFolding;

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
    btnAddFileType: TButton;
    btnAddTemplate: TButton;
    btnDeleteTemplate: TButton;
    edtJavaPath: TEdit;
    edtCurlPath: TEdit;
    gbJadx: TGroupBox;
    gbCodeEditor: TGroupBox;
    gbApktool: TGroupBox;
    gbHelp: TGroupBox;
    gbJava: TGroupBox;
    gbCurl: TGroupBox;
    gbCodeTree: TGroupBox;
    gbView: TGroupBox;
    gbPackage: TGroupBox;
    lblJadxStatus: TLabel;
    lblJadxlVersion: TLabel;
    lblApktoolVersionValue: TLabel;
    lblApktoolVersion: TLabel;
    lblJadxVersionValue: TLabel;
    lblchooseJava: TLabel;
    lblChooseCurl: TLabel;
    lblCodeEditorClassMethodCompletion: TLabel;
    lblCodeEditorJumpClassMethod: TLabel;
    lblApktoolStatus: TLabel;
    lblDefaultApktool: TLabel;
    lblJadxDefault: TLabel;
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
    lstTemplate: TListBox;
    lstStyles: TListBox;
    pnlApktoolVersion: TPanel;
    pnlJadxVersion: TPanel;
    pnlApktoolDefault: TPanel;
    pnlJadxDefault: TPanel;
    pnlTemplateOperation: TPanel;
    pnlTemplateList: TPanel;
    pnlFileTypeBtn: TPanel;
    pgStyles: TPageControl;
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
    sbxShortcut: TScrollBox;
    sbxFileType: TScrollBox;
    splTemplateList: TSplitter;
    splStyle: TSplitter;
    tsJava: TTabSheet;
    tsSmali: TTabSheet;
    tsXML: TTabSheet;
    tsHTML: TTabSheet;
    tsJs: TTabSheet;
    tsCSS: TTabSheet;
    tsShell: TTabSheet;
    tsTemplate: TTabSheet;
    tsFileType: TTabSheet;
    tsHighlight: TTabSheet;
    tsEnvironment: TTabSheet;
    tsShortcut: TTabSheet;
    procedure btnAddTemplateClick(Sender: TObject);
    procedure btnChooseCurlClick(Sender: TObject);
    procedure btnChooseJavaClick(Sender: TObject);
    procedure btnDeleteTemplateClick(Sender: TObject);
    procedure btnViewClassIndexClick(Sender: TObject);
    procedure btnAddFileTypeClick(Sender: TObject);
    procedure lstStylesClick(Sender: TObject);
    procedure lstTemplateClick(Sender: TObject);
    procedure synTemplateChange(Sender: TObject);
  private

    STATIC_SHORTCUTS: array of TShortCut;

    // synedit
    FSynSmali: TSynEdit;
    FSynXML: TSynEdit;
    FSynHTML: TSynEdit;
    FSynCSS: TSynEdit;
    FSynJS: TSynEdit;
    FSynShell: TSynEdit;
    FSynJava: TSynEdit;

    synTemplate: TSynEdit;

    // highlighters
    FHSmali: TSynSmaliSyn;
    FHXML: TSynXMLSyn;
    FHHtml: TSynHTMLSyn;
    FHCss: TSynCssSyn;
    FHJs: TSynJScriptSyn;
    FHShell: TSynUNIXShellScriptSyn;
    FHJava: TSynJavaSyn;

    procedure apktoolVersionComplete(Sender: TObject; ACmdType: TCommandType;
      AParam: array of string);
    procedure filetypeDeleteClick(Sender: TObject; AType: string; AEditorPath: string);
    function filetypeSelectClick(Sender: TObject; AType: string; ACurrentEditor: string
      ): string;
    function IsCanSetShortcut(AKey: string; AShortcut: TShortCut): Boolean;
    procedure jadxVersionComplete(Sender: TObject; ACmdType: TCommandType;
      AParam: array of string);
    procedure LoadShortcut();
    procedure LoadStyles();
    procedure LoadApktoolVersion();
    procedure LoadJadxVersion();
    procedure LoadFileTypes();
    procedure LoadTemplate();
  protected
    procedure InitSynEdit(AEditor: TSynEdit);
    procedure InitComponents; override;
    procedure InitEvents; override;
    procedure InitLogic; override;
  public

  end;

var
  FormSettings: TFormSettings;

implementation

uses
  config, frmShortcutAccept, baseData, fileTypeItemView;

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

procedure TFormSettings.btnDeleteTemplateClick(Sender: TObject);
var
  idx: Integer;
  p: string;
begin
  // delete template
  idx := lstTemplate.ItemIndex;
  if (idx = -1) then Exit;
  p := ExtractFilePath(ParamStr(0)) + 'template/custom/' + lstTemplate.Items[idx];
  lstTemplate.Items.Delete(idx);
  DeleteFile(p);
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

procedure TFormSettings.btnAddFileTypeClick(Sender: TObject);
var
  tp: string;
  view: TFileTypeItemView;
begin
  tp := InputBox('Add File Type', 'Input file extension', '').Trim;
  if (tp = '') then Exit;
  view := TFileTypeItemView.Create(sbxFileType);
  view.Parent := sbxFileType;
  view.FileType:= tp;
  view.Editor:= '';
  view.OnFileTypeDelete:=@filetypeDeleteClick;
  view.OnFileTypeSelect:=@filetypeSelectClick;
  GlobalConfig.AddFileType(tp, '');
end;

function IfThen(b: Boolean; trueValue: TFontStyles; falseValue: TFontStyles): TFontStyles;
begin
  if b then Result := trueValue else Result := falseValue;
end;

procedure TFormSettings.lstStylesClick(Sender: TObject);
var
  idx: Integer;
  path: string;
begin
  idx := lstStyles.ItemIndex;
  if (idx = -1) then Exit;
  path := ExtractFilePath(ParamStr(0)) + 'style/' + lstStyles.Items[idx];

  // load theme
  with TIniFile.Create(path) do begin
    FSynSmali.Color:= ReadInteger(SEC_SMALI, KEY_BACKGROUND, clWhite);
    with FHSmali do begin
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

    FSynXML.Color:= ReadInteger(SEC_XML, KEY_BACKGROUND, clWhite);
    with FHXML do begin
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

    FSynHTML.Color:= ReadInteger(SEC_HTML, KEY_BACKGROUND, clWhite);
    with FHHtml do begin
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

    FSynCss.Color:= ReadInteger(SEC_CSS, KEY_BACKGROUND, clWhite);
    with FHCss do begin
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

    FSynJs.Color:= ReadInteger(SEC_JS, KEY_BACKGROUND, clWhite);
    with FHJs do begin
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

    FSynShell.Color:= ReadInteger(SEC_SHELL, KEY_BACKGROUND, clWhite);
    with FHShell do begin
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

    FSynJava.Color:= ReadInteger(SEC_JAVA, KEY_BACKGROUND, clWhite);
    with FHJava do begin
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

  GlobalConfig.CodeTheme:= lstStyles.Items[idx];
end;

procedure TFormSettings.lstTemplateClick(Sender: TObject);
var
  idx: Integer;
  p: string;
begin
  idx := lstTemplate.ItemIndex;
  if (idx = -1) then Exit;
  p := ExtractFilePath(ParamStr(0)) + 'template/custom/' + lstTemplate.Items[idx];
  synTemplate.Lines.LoadFromFile(p);
  synTemplate.Hint:= p;
end;

procedure TFormSettings.synTemplateChange(Sender: TObject);
begin
  if (string(synTemplate.Hint).Trim <> '') then synTemplate.Lines.SaveToFile(synTemplate.Hint);
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

procedure TFormSettings.jadxVersionComplete(Sender: TObject;
  ACmdType: TCommandType; AParam: array of string);
begin
  // get jadx version
  lblJadxVersionValue.Caption:= AParam[0];
end;

procedure TFormSettings.filetypeDeleteClick(Sender: TObject; AType: string;
  AEditorPath: string);
begin
  // delete file type
  TFileTypeItemView(Sender).Free;
  GlobalConfig.RemoveFileType(AType);
end;

procedure TFormSettings.apktoolVersionComplete(Sender: TObject;
  ACmdType: TCommandType; AParam: array of string);
begin
  // get apktool version
  lblApktoolVersionValue.Caption:= AParam[0];
end;

function TFormSettings.filetypeSelectClick(Sender: TObject; AType: string;
  ACurrentEditor: string): string;
begin
  // select file editor
  Result := '';
  with TOpenDialog.Create(nil) do begin
    if Execute then begin
      Result := FileName;
    end;
    Free;
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

procedure TFormSettings.LoadStyles;
var
  src: TSearchRec;
  p: string;
begin
  lstStyles.Items.Clear;
  p := ExtractFilePath(ParamStr(0)) + 'style/';
  if (FindFirst(p + '*.style', faAnyFile, src) = 0) then begin
    repeat
      lstStyles.Items.Add(src.Name);
    until FindNext(src) <> 0;
    FindClose(src);
  end;
end;

procedure TFormSettings.LoadApktoolVersion;
begin
  // load apktool version
  with TCommandThread.Create(ctVersion, ['']) do begin
    OnCommandComplete:=@apktoolVersionComplete;
    Start;
  end;
end;

procedure TFormSettings.LoadJadxVersion;
begin
  // load jadx version
  with TCommandThread.Create(ctJadx, ['']) do begin
    OnCommandComplete:=@jadxVersionComplete;
    Start;
  end;
end;

procedure TFormSettings.LoadFileTypes;
var
  list: TStringList;
  view: TFileTypeItemView;
  tp: string;
  i: Integer;
begin
  // load file types
  list := GlobalConfig.FileTypes;
  for i := 0 to list.Count - 1 do begin
    view := TFileTypeItemView.Create(sbxFileType);
    view.Parent := sbxFileType;
    tp := list[i];
    tp := tp.Replace('filetype_', '', [rfIgnoreCase, rfReplaceAll]);
    view.FileType:= tp;
    view.Editor:= GlobalConfig.FileTypeEditor[tp];
    view.OnFileTypeDelete:= @filetypeDeleteClick;
    view.OnFileTypeSelect:= @filetypeSelectClick;
  end;
end;

procedure TFormSettings.LoadTemplate;
var
  path: string;
  src: TSearchRec;
begin
  path:= ExtractFilePath(ParamStr(0)) + 'template/custom/';
  if (FindFirst(path + '*.template', faAnyFile, src) = 0) then begin
    repeat
      if (src.Name = '.') or (src.Name = '..') then Continue;
      lstTemplate.Items.Add(src.Name);
    until FindNext(src) <> 0;
    FindClose(src);
  end;
end;

procedure TFormSettings.InitSynEdit(AEditor: TSynEdit);
var
  i: integer;
  part: TSynGutterPartBase;
begin
  // init synedit
  with AEditor do begin
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
  end;
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

procedure TFormSettings.btnAddTemplateClick(Sender: TObject);
var
  tn: string;
  p: string;
  idx: Integer;
begin
  // add template
  tn := InputBox('Add Template', 'Unput Template Name', '').Trim;
  if (tn = '') then Exit;
  p := ExtractFilePath(ParamStr(0)) + 'template/custom/' + tn + '.template';
  with TStringList.Create do begin
    SaveToFile(p);
    Free;
  end;
  synTemplate.Lines.LoadFromFile(p);
  idx := lstTemplate.Items.Add(ExtractFileName(p));
  lstTemplate.ItemIndex:= idx;
end;

procedure TFormSettings.InitComponents;
var
  p: string;
begin
  // settings
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

  FSynSmali:= TSynEdit.Create(Self);
  FSynSmali.Parent := tsSmali;
  InitSynEdit(FSynSmali);
  FSynXML:= TSynEdit.Create(Self);
  FSynXML.Parent := tsXML;
  InitSynEdit(FSynXML);
  FSynHTML:= TSynEdit.Create(Self);
  FSynHTML.Parent := tsHTML;
  InitSynEdit(FSynHTML);
  FSynCSS:= TSynEdit.Create(Self);
  FSynCSS.Parent := tsCSS;
  InitSynEdit(FSynCSS);
  FSynJS:= TSynEdit.Create(Self);
  FSynJS.Parent := tsJs;
  InitSynEdit(FSynJS);
  FSynShell:= TSynEdit.Create(Self);
  FSynShell.Parent := tsShell;
  InitSynEdit(FSynShell);
  FSynJava:= TSynEdit.Create(Self);
  FSynJava.Parent := tsJava;
  InitSynEdit(FSynJava);

  FHSmali:= TSynSmaliSyn.Create(Self);
  FSynSmali.Highlighter := FHSmali;
  FHXML:= TSynXMLSyn.Create(Self);
  FSynXML.Highlighter := FHXML;
  FHHtml:= TSynHTMLSyn.Create(Self);
  FSynHTML.Highlighter := FHHtml;
  FHCss:= TSynCssSyn.Create(Self);
  FSynCSS.Highlighter := FHCss;
  FHJs:= TSynJScriptSyn.Create(Self);
  FSynJS.Highlighter := FHJs;
  FHShell:= TSynUNIXShellScriptSyn.Create(Self);
  FSynShell.Highlighter := FHShell;
  FHJava := TSynJavaSyn.Create(Self);
  FSynJava.Highlighter := FHJava;

  synTemplate := TSynEdit.Create(Self);
  synTemplate.Parent := tsTemplate;
  InitSynEdit(synTemplate);

  // load sample
  p := ExtractFilePath(ParamStr(0)) + 'template/';
  if (FileExists(p + 'sample_smali')) then FSynSmali.Lines.LoadFromFile(p + 'sample_smali');
  if (FileExists(p + 'sample_xml')) then FSynXML.Lines.LoadFromFile(p + 'sample_xml');
  if (FileExists(p + 'sample_html')) then FSynHTML.Lines.LoadFromFile(p + 'sample_html');
  if (FileExists(p + 'sample_css')) then FSynCSS.Lines.LoadFromFile(p + 'sample_css');
  if (FileExists(p + 'sample_js')) then FSynJS.Lines.LoadFromFile(p + 'sample_js');
  if (FileExists(p + 'sample_shell')) then FSynShell.Lines.LoadFromFile(p + 'sample_shell');
  if (FileExists(p + 'sample_java')) then FSynJava.Lines.LoadFromFile(p + 'sample_java');

end;

procedure TFormSettings.InitEvents;
begin
  //
end;

procedure TFormSettings.InitLogic;
var
  p: string;
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

  // apktool

  p := ExtractFilePath(ParamStr(0)) + 'bin/apktool.jar';
  if (FileExists(p)) then begin
    lblApktoolStatus.Caption:= '(exists)';
    lblApktoolStatus.Font.Color:= clDefault;
  end else begin
    lblApktoolStatus.Caption:= '(not exists)';
    lblApktoolStatus.Font.Color:= clRed;
  end;

  // jadx
  p := ExtractFilePath(ParamStr(0)) + 'bin/jadx';
  if (FileExists(p)) then begin
    lblJadxStatus.Caption:= '(exists)';
    lblJadxStatus.Font.Color:= clDefault;
  end else begin
    lblJadxStatus.Caption:= '(not exists)';
    lblJadxStatus.Font.Color:= clRed;
  end;

  LoadApktoolVersion();
  LoadJadxVersion();

  LoadStyles();
  lstStyles.ItemIndex:= lstStyles.Items.IndexOf(GlobalConfig.CodeTheme);
  lstStylesClick(lstStyles);
  LoadFileTypes();
  LoadTemplate();

end;

end.

