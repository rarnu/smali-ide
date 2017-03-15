unit config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, baseData, Menus, LCLType, Graphics;

type

  { TSmaliIdeConfig }

  TSmaliIdeConfig = class
  private
    FAlpha: Integer;
    FAndroidSDKPath: string;
    FAndroidSDKVersion: string;
    FCloseAllOtherPages: TShortCut;
    FCloseAllPages: TShortCut;
    FCodeTheme: string;
    FColor: Integer;
    FCompile: TShortCut;
    FCurlBinaryPath: string;
    FDecompile: TShortCut;
    FDeleteFile: TShortCut;
    FFontAntiAliasing: Boolean;
    FFontColor: Integer;
    FFontName: string;
    FFontSize: Integer;
    FHintClassMethod: TShortCut;
    FHintKeyword: TShortCut;
    FHintTemplate: TShortCut;
    FIni: TIniFile;
    FFileTypes: TStringList;
    FInstallFramework: TShortCut;
    FJavaBinaryPath: string;
    FJumpClassMethod: TShortCut;
    FJumpToJava: TShortCut;
    FNewAnnotation: TShortCut;
    FNewClass: TShortCut;
    FNewEnum: TShortCut;
    FNewInterface: TShortCut;
    FNewTextFile: TShortCut;
    FSettings: TShortCut;
    FShowClassIndex: TShortCut;
    FShowConsole: TShortCut;
    FShowSearchResult: TShortCut;
    FShowSSmali: Boolean;
    FShowSsmaliShortcut: TShortCut;
    FTransparent: Boolean;
    function GetFileTypeEditor(Atype: string): string;
    function GetFileTypes: TStringList;
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
    property JavaBinaryPath: string read FJavaBinaryPath write FJavaBinaryPath;
    property CurlBinaryPath: string read FCurlBinaryPath write FCurlBinaryPath;
    property AndroidSDKPath: string read FAndroidSDKPath write FAndroidSDKPath;
    property AndroidSDKVersion: string read FAndroidSDKVersion write FAndroidSDKVersion;

    // shortcut
    property HintKeyword: TShortCut read FHintKeyword write FHintKeyword;
    property HintClassMethod: TShortCut read FHintClassMethod write FHintClassMethod;
    property HintTemplate: TShortCut read FHintTemplate write FHintTemplate;
    property JumpClassMethod: TShortCut read FJumpClassMethod write FJumpClassMethod;
    property JumpToJava: TShortCut read FJumpToJava write FJumpToJava;

    // code files
    property NewClass: TShortCut read FNewClass write FNewClass;
    property NewInterface: TShortCut read FNewInterface write FNewInterface;
    property NewEnum: TShortCut read FNewEnum write FNewEnum;
    property NewAnnotation: TShortCut read FNewAnnotation write FNewAnnotation;
    property NewTextFile: TShortCut read FNewTextFile write FNewTextFile;
    property DeleteFile: TShortCut read FDeleteFile write FDeleteFile;

    // show
    property ShowClassIndex: TShortCut read FShowClassIndex write FShowClassIndex;
    property ShowSearchResult: TShortCut read FShowSearchResult write FShowSearchResult;
    property ShowConsole: TShortCut read FShowConsole write FShowConsole;
    property ShowSsmaliShortcut: TShortCut read FShowSsmaliShortcut write FShowSsmaliShortcut;
    property CloseAllPages: TShortCut read FCloseAllPages write FCloseAllPages;
    property CloseAllOtherPages: TShortCut read FCloseAllOtherPages write FCloseAllOtherPages;

    // forms
    property Decompile: TShortCut read FDecompile write FDecompile;
    property Compile: TShortCut read FCompile write FCompile;
    property InstallFramework: TShortCut read FInstallFramework write FInstallFramework;
    property Settings: TShortCut read FSettings write FSettings;

    // theme
    property CodeTheme: string read FCodeTheme write FCodeTheme;

    // file types
    property FileTypes: TStringList read GetFileTypes;

    // visibility
    property ShowSSmali: Boolean read FShowSSmali write FShowSSmali;

    // common
    property FontName: string read FFontName write FFontName;
    property FontSize: Integer read FFontSize write FFontSize;
    property FontAntiAliasing: Boolean read FFontAntiAliasing write FFontAntiAliasing;
    property Transparent: Boolean read FTransparent write FTransparent;
    property Alpha: Integer read FAlpha write FAlpha;
    property Color: Integer read FColor write FColor;
    property FontColor: Integer read FFontColor write FFontColor;

  end;

var
  GlobalConfig: TSmaliIdeConfig;

implementation

uses
  WindowsUtils;

{ TSmaliIdeConfig }

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

constructor TSmaliIdeConfig.Create;
var
  path: string;
begin
  path:= ChangeFileExt(ParamStr(0), '.ini');
  FIni := TIniFile.Create(path);
  FFileTypes := TStringList.Create;

  FJavaBinaryPath := FIni.ReadString(SEC_CONFIG, KEY_JAVA_BINARY_PATH, {$IFNDEF WINDOWS}'/usr/bin/java'{$ELSE}WindowsUtils.GetDefaultJavaPath(){$ENDIF});
  FCurlBinaryPath := FIni.ReadString(SEC_CONFIG, KEY_CURL_BINARY_PATH, '/usr/bin/curl');
  FAndroidSDKPath := FIni.ReadString(SEC_CONFIG, KEY_ANDROID_SDK_PATH, '');
  FAndroidSDKVersion := FIni.ReadString(SEC_CONFIG, KEY_ANDROID_SDK_VERSION, '');
  FHintKeyword := FIni.ReadInteger(SEC_CONFIG, KEY_HINT_KEYWORD_SHORTCUT, ShortCut(VK_J, [ssCtrl]));
  FHintClassMethod := FIni.ReadInteger(SEC_CONFIG, KEY_HINT_CLASSMETHOD_SHORTCUT, ShortCut(VK_K, [ssCtrl]));
  FHintTemplate := FIni.ReadInteger(SEC_CONFIG, KEY_HINT_TEMPLATE_SHORTCUT, ShortCut(VK_L, [ssCtrl]));
  FJumpClassMethod := FIni.ReadInteger(SEC_CONFIG, KEY_JUMP_CLASS_METHOD_SHORTCUT, ShortCut(VK_F2, []));
  FJumpToJava := FIni.ReadInteger(SEC_CONFIG, KEY_JUMP_TO_JAVA_SHORTCUT, ShortCut(VK_RETURN, [ssAlt]));
  FNewAnnotation := FIni.ReadInteger(SEC_CONFIG, KEY_NEW_ANNOTATION_SHORTCUT, 0);
  FNewClass := FIni.ReadInteger(SEC_CONFIG, KEY_NEW_CLASS_SHORTCUT, 0);
  FNewEnum := FIni.ReadInteger(SEC_CONFIG, KEY_NEW_ENUM_SHORTCUT, 0);
  FNewInterface := FIni.ReadInteger(SEC_CONFIG, KEY_NEW_INTERFACE_SHORTCUT, 0);
  FNewTextFile := FIni.ReadInteger(SEC_CONFIG, KEY_NEW_TEXTFILE_SHORTCUT, 0);
  FDeleteFile := FIni.ReadInteger(SEC_CONFIG, KEY_DELETE_FILE_SHORTCUT, 0);
  FCloseAllOtherPages := FIni.ReadInteger(SEC_CONFIG, KEY_CLOSE_ALL_OTHER_PAGES_SHORTCUT, 0);
  FCloseAllPages := FIni.ReadInteger(SEC_CONFIG, KEY_CLOSE_ALL_PAGES_SHORTCUT, 0);
  FShowClassIndex := FIni.ReadInteger(SEC_CONFIG, KEY_SHOW_CLASSINDEX_SHORTCUT, 0);
  FShowConsole := FIni.ReadInteger(SEC_CONFIG, KEY_SHOW_CONSOLE_SHORTCUT, 0);
  FShowSearchResult := FIni.ReadInteger(SEC_CONFIG, KEY_SHOW_SEARCHRESULT_SHORTCUT, 0);
  FShowSsmaliShortcut := FIni.ReadInteger(SEC_CONFIG, KEY_SHOW_SSMALI_SHORTCUT, 0);
  FDecompile := FIni.ReadInteger(SEC_CONFIG, KEY_DECOMPILE_SHORTCUT, 0);
  FCompile := FIni.ReadInteger(SEC_CONFIG, KEY_COMPILE_SHORTCUT, 0);
  FInstallFramework := FIni.ReadInteger(SEC_CONFIG, KEY_INSTALL_FRAMEWORK_SHORTCUT, 0);
  FSettings := FIni.ReadInteger(SEC_CONFIG, KEY_SETTINGS_SHORTCUT, 0);
  FCodeTheme := FIni.ReadString(SEC_CONFIG, KEY_THEME, 'Default.style');
  FShowSSmali := FIni.ReadBool(SEC_CONFIG, KEY_SHOW_SSMALI, False);
  FAlpha := FIni.ReadInteger(SEC_COMMON, KEY_UI_ALPHA, 255);
  FColor := FIni.ReadInteger(SEC_COMMON, KEY_UI_COLOR, clWhite);
  FFontAntiAliasing := FIni.ReadBool(SEC_COMMON, KEY_FONT_ANTI_ALIASING, True);
  FFontColor := FIni.ReadInteger(SEC_COMMON, KEY_UI_FONT_COLOR, clBlack);
  FFontName := FIni.ReadString(SEC_COMMON, KEY_FONT_NAME, {$IFDEF WINDOWS}'Microsoft Yahei'{$ELSE}{$IFDEF DARWIN}'Monaco'{$ELSE}'DejaVu Sans Mono'{$ENDIF}{$ENDIF});
  FFontSize := FIni.ReadInteger(SEC_COMMON, KEY_FONT_SIZE, 10);
  FTransparent := FIni.ReadBool(SEC_COMMON, KEY_UI_TRANSPARENT, False);

end;

destructor TSmaliIdeConfig.Destroy;
begin

  FIni.WriteString(SEC_CONFIG, KEY_JAVA_BINARY_PATH, FJavaBinaryPath);
  FIni.WriteString(SEC_CONFIG, KEY_CURL_BINARY_PATH, FCurlBinaryPath);
  FIni.WriteString(SEC_CONFIG, KEY_ANDROID_SDK_PATH, FAndroidSDKPath);
  FIni.WriteString(SEC_CONFIG, KEY_ANDROID_SDK_VERSION, FAndroidSDKVersion);
  FIni.WriteInteger(SEC_CONFIG, KEY_HINT_KEYWORD_SHORTCUT, FHintKeyword);
  FIni.WriteInteger(SEC_CONFIG, KEY_HINT_CLASSMETHOD_SHORTCUT, FHintClassMethod);
  FIni.WriteInteger(SEC_CONFIG, KEY_HINT_TEMPLATE_SHORTCUT, FHintTemplate);
  FIni.WriteInteger(SEC_CONFIG, KEY_JUMP_CLASS_METHOD_SHORTCUT, FJumpClassMethod);
  FIni.WriteInteger(SEC_CONFIG, KEY_JUMP_TO_JAVA_SHORTCUT, FJumpToJava);
  FIni.WriteInteger(SEC_CONFIG, KEY_NEW_ANNOTATION_SHORTCUT, FNewAnnotation);
  FIni.WriteInteger(SEC_CONFIG, KEY_NEW_CLASS_SHORTCUT, FNewClass);
  FIni.WriteInteger(SEC_CONFIG, KEY_NEW_ENUM_SHORTCUT, FNewEnum);
  FIni.WriteInteger(SEC_CONFIG, KEY_NEW_INTERFACE_SHORTCUT, FNewInterface);
  FIni.WriteInteger(SEC_CONFIG, KEY_NEW_TEXTFILE_SHORTCUT, FNewTextFile);
  FIni.WriteInteger(SEC_CONFIG, KEY_DELETE_FILE_SHORTCUT, FDeleteFile);
  FIni.WriteInteger(SEC_CONFIG, KEY_CLOSE_ALL_OTHER_PAGES_SHORTCUT, FCloseAllOtherPages);
  FIni.WriteInteger(SEC_CONFIG, KEY_CLOSE_ALL_PAGES_SHORTCUT, FCloseAllPages);
  FIni.WriteInteger(SEC_CONFIG, KEY_SHOW_CLASSINDEX_SHORTCUT, FShowClassIndex);
  FIni.WriteInteger(SEC_CONFIG, KEY_SHOW_CONSOLE_SHORTCUT, FShowConsole);
  FIni.WriteInteger(SEC_CONFIG, KEY_SHOW_SEARCHRESULT_SHORTCUT, FShowSearchResult);
  FIni.WriteInteger(SEC_CONFIG, KEY_SHOW_SSMALI_SHORTCUT, FShowSsmaliShortcut);
  FIni.WriteInteger(SEC_CONFIG, KEY_DECOMPILE_SHORTCUT, FDecompile);
  FIni.WriteInteger(SEC_CONFIG, KEY_COMPILE_SHORTCUT, FCompile);
  FIni.WriteInteger(SEC_CONFIG, KEY_INSTALL_FRAMEWORK_SHORTCUT, FInstallFramework);
  FIni.WriteInteger(SEC_CONFIG, KEY_SETTINGS_SHORTCUT, FSettings);
  FIni.WriteString(SEC_CONFIG, KEY_THEME, FCodeTheme);
  FIni.WriteBool(SEC_CONFIG, KEY_SHOW_SSMALI, FShowSSmali);
  FIni.WriteInteger(SEC_COMMON, KEY_UI_ALPHA, FAlpha);
  FIni.WriteInteger(SEC_COMMON, KEY_UI_COLOR, FColor);
  FIni.WriteBool(SEC_COMMON, KEY_FONT_ANTI_ALIASING, FFontAntiAliasing);
  FIni.WriteInteger(SEC_COMMON, KEY_UI_FONT_COLOR, FFontColor);
  FIni.WriteString(SEC_COMMON, KEY_FONT_NAME, FFontName);
  FIni.WriteInteger(SEC_COMMON, KEY_FONT_SIZE, FFontSize);
  FIni.WriteBool(SEC_COMMON, KEY_UI_TRANSPARENT, FTransparent);

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

