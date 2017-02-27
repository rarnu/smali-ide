unit config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, baseData, Menus, LCLType;

type

  { TSmaliIdeConfig }

  TSmaliIdeConfig = class
  private
    FIni: TIniFile;
    FFileTypes: TStringList;
    function GetAndroidSDKPath: string;
    function GetAndroidSDKVersion: string;
    function GetCloseAllOtherPages: TShortCut;
    function GetCloseAllPages: TShortCut;
    function GetCodeTheme: string;
    function GetCompile: TShortCut;
    function GetCurlBinaryPath: string;
    function GetDecompile: TShortCut;
    function GetDeleteFile: TShortCut;
    function GetFileTypeEditor(Atype: string): string;
    function GetFileTypes: TStringList;
    function GetHintClassMethod: TShortCut;
    function GetHintKeyword: TShortCut;
    function GetHintTemplate: TShortCut;
    function GetInstallFramework: TShortCut;
    function GetJavaBinaryPath: string;
    function GetJumpClassMethod: TShortCut;
    function GetJumpToJava: TShortCut;
    function GetNewAnnotation: TShortCut;
    function GetNewClass: TShortCut;
    function GetNewEnum: TShortCut;
    function GetNewInterface: TShortCut;
    function GetNewTextFile: TShortCut;
    function GetSettings: TShortCut;
    function GetShowClassIndex: TShortCut;
    function GetShowConsole: TShortCut;
    function GetShowSearchResult: TShortCut;
    function GetShowSSmali: Boolean;
    function GetShowSsmaliShortcut: TShortCut;
    procedure SetAndroidSDKPath(AValue: string);
    procedure SetAndroidSDKVersion(AValue: string);
    procedure SetCloseAllOtherPages(AValue: TShortCut);
    procedure SetCloseAllPages(AValue: TShortCut);
    procedure SetCodeTheme(AValue: string);
    procedure SetCompile(AValue: TShortCut);
    procedure SetCurlBinaryPath(AValue: string);
    procedure SetDecompile(AValue: TShortCut);
    procedure SetDeleteFile(AValue: TShortCut);
    procedure SetHintClassMethod(AValue: TShortCut);
    procedure SetHintKeyword(AValue: TShortCut);
    procedure SetHintTemplate(AValue: TShortCut);
    procedure SetInstallFramework(AValue: TShortCut);
    procedure SetJavaBinaryPath(AValue: string);
    procedure SetJumpClassMethod(AValue: TShortCut);
    procedure SetJumpToJava(AValue: TShortCut);
    procedure SetNewAnnotation(AValue: TShortCut);
    procedure SetNewClass(AValue: TShortCut);
    procedure SetNewEnum(AValue: TShortCut);
    procedure SetNewInterface(AValue: TShortCut);
    procedure SetNewTextFile(AValue: TShortCut);
    procedure SetSettings(AValue: TShortCut);
    procedure SetShowClassIndex(AValue: TShortCut);
    procedure SetShowConsole(AValue: TShortCut);
    procedure SetShowSearchResult(AValue: TShortCut);
    procedure SetShowSSmali(AValue: Boolean);
    procedure SetShowSsmaliShortcut(AValue: TShortCut);
  public
    constructor Create;
    destructor Destroy; override;
    function GetShortcut(Akey: string): TShortCut;
    procedure SetShortcut(Akey: string; Avalue: TShortCut);
    procedure AddFileType(AType: string; APath: string);
    procedure RemoveFileType(AType: string);

    property FileTypeEditor[Atype: string]: string read GetFileTypeEditor;
  published
    // path
    property JavaBinaryPath: string read GetJavaBinaryPath write SetJavaBinaryPath;
    property CurlBinaryPath: string read GetCurlBinaryPath write SetCurlBinaryPath;
    property AndroidSDKPath: string read GetAndroidSDKPath write SetAndroidSDKPath;
    property AndroidSDKVersion: string read GetAndroidSDKVersion write SetAndroidSDKVersion;

    // shortcut
    property HintKeyword: TShortCut read GetHintKeyword write SetHintKeyword;
    property HintClassMethod: TShortCut read GetHintClassMethod write SetHintClassMethod;
    property HintTemplate: TShortCut read GetHintTemplate write SetHintTemplate;
    property JumpClassMethod: TShortCut read GetJumpClassMethod write SetJumpClassMethod;
    property JumpToJava: TShortCut read GetJumpToJava write SetJumpToJava;

    // code files
    property NewClass: TShortCut read GetNewClass write SetNewClass;
    property NewInterface: TShortCut read GetNewInterface write SetNewInterface;
    property NewEnum: TShortCut read GetNewEnum write SetNewEnum;
    property NewAnnotation: TShortCut read GetNewAnnotation write SetNewAnnotation;
    property NewTextFile: TShortCut read GetNewTextFile write SetNewTextFile;
    property DeleteFile: TShortCut read GetDeleteFile write SetDeleteFile;

    // show
    property ShowClassIndex: TShortCut read GetShowClassIndex write SetShowClassIndex;
    property ShowSearchResult: TShortCut read GetShowSearchResult write SetShowSearchResult;
    property ShowConsole: TShortCut read GetShowConsole write SetShowConsole;
    property ShowSsmaliShortcut: TShortCut read GetShowSsmaliShortcut write SetShowSsmaliShortcut;
    property CloseAllPages: TShortCut read GetCloseAllPages write SetCloseAllPages;
    property CloseAllOtherPages: TShortCut read GetCloseAllOtherPages write SetCloseAllOtherPages;

    // forms
    property Decompile: TShortCut read GetDecompile write SetDecompile;
    property Compile: TShortCut read GetCompile write SetCompile;
    property InstallFramework: TShortCut read GetInstallFramework write SetInstallFramework;
    property Settings: TShortCut read GetSettings write SetSettings;

    // theme
    property CodeTheme: string read GetCodeTheme write SetCodeTheme;

    // file types
    property FileTypes: TStringList read GetFileTypes;

    // visibility
    property ShowSSmali: Boolean read GetShowSSmali write SetShowSSmali;

  end;

var
  GlobalConfig: TSmaliIdeConfig;

implementation

uses
  WindowsUtils;

{ TSmaliIdeConfig }

function TSmaliIdeConfig.GetJavaBinaryPath: string;
begin
  Result := FIni.ReadString(SEC_CONFIG, KEY_JAVA_BINARY_PATH, {$IFNDEF WINDOWS}'/usr/bin/java'{$ELSE}WindowsUtils.GetDefaultJavaPath(){$ENDIF});
end;

function TSmaliIdeConfig.GetJumpClassMethod: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_JUMP_CLASS_METHOD_SHORTCUT, ShortCut(VK_F2, []));
end;

function TSmaliIdeConfig.GetJumpToJava: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_JUMP_TO_JAVA_SHORTCUT, ShortCut(VK_RETURN, [ssAlt]));
end;

function TSmaliIdeConfig.GetNewAnnotation: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_NEW_ANNOTATION_SHORTCUT, 0);
end;

function TSmaliIdeConfig.GetNewClass: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_NEW_CLASS_SHORTCUT, 0);
end;

function TSmaliIdeConfig.GetNewEnum: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_NEW_ENUM_SHORTCUT, 0);
end;

function TSmaliIdeConfig.GetNewInterface: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_NEW_INTERFACE_SHORTCUT, 0);
end;

function TSmaliIdeConfig.GetNewTextFile: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_NEW_TEXTFILE_SHORTCUT, 0);
end;

function TSmaliIdeConfig.GetSettings: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_SETTINGS_SHORTCUT, 0);
end;

function TSmaliIdeConfig.GetShowClassIndex: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_SHOW_CLASSINDEX_SHORTCUT, 0);
end;

function TSmaliIdeConfig.GetShowConsole: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_SHOW_CONSOLE_SHORTCUT, 0);
end;

function TSmaliIdeConfig.GetShowSearchResult: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_SHOW_SEARCHRESULT_SHORTCUT, 0);
end;

function TSmaliIdeConfig.GetShowSSmali: Boolean;
begin
  Result := FIni.ReadBool(SEC_CONFIG, KEY_SHOW_SSMALI, False);
end;

function TSmaliIdeConfig.GetShowSsmaliShortcut: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_SHOW_SSMALI_SHORTCUT, 0);
end;

procedure TSmaliIdeConfig.SetAndroidSDKPath(AValue: string);
begin
  FIni.WriteString(SEC_CONFIG, KEY_ANDROID_SDK_PATH, AValue);
end;

procedure TSmaliIdeConfig.SetAndroidSDKVersion(AValue: string);
begin
  FIni.WriteString(SEC_CONFIG, KEY_ANDROID_SDK_VERSION, AValue);
end;

procedure TSmaliIdeConfig.SetCloseAllOtherPages(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_CLOSE_ALL_OTHER_PAGES_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetCloseAllPages(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_CLOSE_ALL_PAGES_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetCodeTheme(AValue: string);
begin
  FIni.WriteString(SEC_CONFIG, KEY_THEME, AValue);
end;

procedure TSmaliIdeConfig.SetCompile(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_COMPILE_SHORTCUT, AValue);
end;

function TSmaliIdeConfig.GetCurlBinaryPath: string;
begin
  Result := FIni.ReadString(SEC_CONFIG, KEY_CURL_BINARY_PATH, '/usr/bin/curl');
end;

function TSmaliIdeConfig.GetDecompile: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_DECOMPILE_SHORTCUT, 0);
end;

function TSmaliIdeConfig.GetCloseAllOtherPages: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_CLOSE_ALL_OTHER_PAGES_SHORTCUT, 0);
end;

function TSmaliIdeConfig.GetAndroidSDKPath: string;
begin
  Result := FIni.ReadString(SEC_CONFIG, KEY_ANDROID_SDK_PATH, '');
end;

function TSmaliIdeConfig.GetAndroidSDKVersion: string;
begin
  Result := FIni.ReadString(SEC_CONFIG, KEY_ANDROID_SDK_VERSION, '');
end;

function TSmaliIdeConfig.GetCloseAllPages: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_CLOSE_ALL_PAGES_SHORTCUT, 0);
end;

function TSmaliIdeConfig.GetCodeTheme: string;
begin
  Result := FIni.ReadString(SEC_CONFIG, KEY_THEME, 'Default.style');
end;

function TSmaliIdeConfig.GetCompile: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_COMPILE_SHORTCUT, 0);
end;

function TSmaliIdeConfig.GetDeleteFile: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_DELETE_FILE_SHORTCUT, 0);
end;

function TSmaliIdeConfig.GetFileTypeEditor(Atype: string): string;
begin
  Result := FIni.ReadString(SEC_CONFIG, 'filetype_' + Atype, '');
end;

function TSmaliIdeConfig.GetFileTypes: TStringList;
var
  i: Integer;
  secList: TStringList;
begin
  FFileTypes.Clear;
  secList := TStringList.Create;
  FIni.ReadSection(SEC_CONFIG, secList);
  for i := 0 to secList.Count - 1 do if (secList[i].StartsWith('filetype_')) then FFileTypes.Add(secList[i]);
  secList.Free;
  Result := FFileTypes;
end;

function TSmaliIdeConfig.GetHintClassMethod: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_HINT_CLASSMETHOD_SHORTCUT, ShortCut(VK_K, [ssCtrl]));
end;

function TSmaliIdeConfig.GetHintKeyword: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_HINT_KEYWORD_SHORTCUT, ShortCut(VK_J, [ssCtrl]));
end;

function TSmaliIdeConfig.GetHintTemplate: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_HINT_TEMPLATE_SHORTCUT, ShortCut(VK_L, [ssCtrl]));
end;

function TSmaliIdeConfig.GetInstallFramework: TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, KEY_INSTALL_FRAMEWORK_SHORTCUT, 0);
end;

procedure TSmaliIdeConfig.SetCurlBinaryPath(AValue: string);
begin
  FIni.WriteString(SEC_CONFIG, KEY_CURL_BINARY_PATH, AValue);
end;

procedure TSmaliIdeConfig.SetDecompile(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_DECOMPILE_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetDeleteFile(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_DELETE_FILE_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetHintClassMethod(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_HINT_CLASSMETHOD_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetHintKeyword(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_HINT_KEYWORD_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetHintTemplate(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_HINT_TEMPLATE_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetInstallFramework(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_INSTALL_FRAMEWORK_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetJavaBinaryPath(AValue: string);
begin
  FIni.WriteString(SEC_CONFIG, KEY_JAVA_BINARY_PATH, AValue);
end;

procedure TSmaliIdeConfig.SetJumpClassMethod(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_JUMP_CLASS_METHOD_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetJumpToJava(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_JUMP_TO_JAVA_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetNewAnnotation(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_NEW_ANNOTATION_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetNewClass(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_NEW_CLASS_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetNewEnum(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_NEW_ENUM_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetNewInterface(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_NEW_INTERFACE_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetNewTextFile(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_NEW_TEXTFILE_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetSettings(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_SETTINGS_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetShowClassIndex(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_SHOW_CLASSINDEX_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetShowConsole(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_SHOW_CONSOLE_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetShowSearchResult(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_SHOW_SEARCHRESULT_SHORTCUT, AValue);
end;

procedure TSmaliIdeConfig.SetShowSSmali(AValue: Boolean);
begin
  FIni.WriteBool(SEC_CONFIG, KEY_SHOW_SSMALI, AValue);
end;

procedure TSmaliIdeConfig.SetShowSsmaliShortcut(AValue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, KEY_SHOW_SSMALI_SHORTCUT, AValue);
end;

constructor TSmaliIdeConfig.Create;
var
  path: string;
begin
  path:= ChangeFileExt(ParamStr(0), '.ini');
  FIni := TIniFile.Create(path);
  FFileTypes := TStringList.Create;
end;

destructor TSmaliIdeConfig.Destroy;
begin
  FFileTypes.Free;
  FIni.Free;
  inherited Destroy;
end;

function TSmaliIdeConfig.GetShortcut(Akey: string): TShortCut;
begin
  Result := FIni.ReadInteger(SEC_CONFIG, Akey, 0);
end;

procedure TSmaliIdeConfig.SetShortcut(Akey: string; Avalue: TShortCut);
begin
  FIni.WriteInteger(SEC_CONFIG, Akey, Avalue);
end;

procedure TSmaliIdeConfig.AddFileType(AType: string; APath: string);
begin
  // add type
  FIni.WriteString(SEC_CONFIG, 'filetype_' + AType, APath);
end;

procedure TSmaliIdeConfig.RemoveFileType(AType: string);
begin
  // remove type
  FIni.DeleteKey(SEC_CONFIG, 'filetype_' + AType);
end;

initialization
  GlobalConfig := TSmaliIdeConfig.Create;
finalization
  GlobalConfig.Free;

end.

