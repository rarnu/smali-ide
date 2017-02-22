unit SmaliIdeAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HttpUtils, jsonparser, fpjson, jsonscanner, fgl;

const
  BASE_URL = 'http://rarnu.com/smaliide/';

const
  PLATFORM = {$IFDEF WINDOWS}'2'{$ELSE}{$IFDEF DARWIN}'1'{$ELSE}'0'{$ENDIF}{$ENDIF};
  VERSION = '1';

type

  { TUpdateHistory }

  TUpdateHistory = class
  private
    FdownloadUrl: string;
    FverCode: Integer;
    FversionDesc: string;
  public
    class function fromJson(json: TJSONObject): TUpdateHistory;
  published
    property verCode: Integer read FverCode write FverCode;
    property downloadUrl: string read FdownloadUrl write FdownloadUrl;
    property versionDesc: string read FversionDesc write FversionDesc;
  end;

  THistoryList = specialize TFPGList<TUpdateHistory>;


  { TUpdateInfo }

  TUpdateInfo = class
  private
    FApp: string;
    FDownloadURL: string;
    FHistory: THistoryList;
    FLastVersion: Integer;
    FPlatform: string;
    FRequestVersion: Integer;
    FVersionDesc: string;
  public
    constructor Create;
    destructor Destroy; override;
    class function fromJson(Astr: string): TUpdateInfo;
  published
    property Platform: string read FPlatform write FPlatform;
    property App: string read FApp write FApp;
    property RequestVersion: Integer read FRequestVersion write FRequestVersion;
    property LastVersion: Integer read FLastVersion write FLastVersion;
    property DownloadURL: string read FDownloadURL write FDownloadURL;
    property VersionDesc: string read FVersionDesc write FVersionDesc;
    property History: THistoryList read FHistory write FHistory;
  end;

  { TUpdateThread }
  TOnUpdateCallback = procedure(Sender: TObject; AInfo: TUpdateInfo) of object;

  TUpdateThread = class(TThread)
  private
    FApp: string;
    FUpdateInfo: TUpdateInfo;
    FOnUpdateCallback: TOnUpdateCallback;
    procedure updateTerminated(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(AApp: string);
    destructor Destroy; override;
  public
    property OnUpdateCallback: TOnUpdateCallback read FOnUpdateCallback write FOnUpdateCallback;
  end;

procedure UpdateCheck(app: string; callback: TOnUpdateCallback);

implementation

procedure UpdateCheck(app: string; callback: TOnUpdateCallback);
begin
  with TUpdateThread.Create(app) do begin
    OnUpdateCallback:= callback;
    Start;
  end;
end;

{ TUpdateHistory }

class function TUpdateHistory.fromJson(json: TJSONObject): TUpdateHistory;
begin
  Result := nil;
  if (json <> nil) then begin
    Result := TUpdateHistory.Create;
    Result.verCode:= json.Integers['verCode'];
    Result.downloadUrl:= json.Strings['downloadUrl'];
    Result.versionDesc:= json.Strings['versionDesc'];
  end;
end;

{ TUpdateInfo }

constructor TUpdateInfo.Create;
begin
  FHistory := THistoryList.Create;
end;

destructor TUpdateInfo.Destroy;
begin
  FHistory.Free;
  inherited Destroy;
end;

class function TUpdateInfo.fromJson(Astr: string): TUpdateInfo;
var
  json: TJSONObject;
  parser: TJSONParser;
  i: Integer;
begin
  Result := nil;
  if (Astr.Trim = '') then Exit;
  parser := TJSONParser.Create(Astr, [joUTF8]);
  json := TJSONObject(parser.Parse);
  if (json.Integers['result'] = 0) then begin
    Result := TUpdateInfo.Create;
    with json.Objects['info'] do begin
      Result.Platform:= Strings['platform'];
      Result.App:= Strings['app'];
      Result.RequestVersion:= Integers['reqVersion'];
      Result.LastVersion:= Integers['lastVersion'];
      Result.DownloadURL:= Strings['downloadUrl'];
      Result.VersionDesc:= Strings['versionDesc'];
    end;
    with json.Arrays['history'] do begin
      for i := 0 to Count - 1 do Result.History.Add(TUpdateHistory.fromJson(Objects[i]));
    end;
  end;
  parser.Free;
  json.Free;
end;

{ TUpdateThread }

procedure TUpdateThread.updateTerminated(Sender: TObject);
begin
  if (Assigned(FOnUpdateCallback)) then begin
    FOnUpdateCallback(Self, FUpdateInfo);
  end;
end;

procedure TUpdateThread.Execute;
var
  param: TParamMap;
  jsonStr: string;
begin
  param := TParamMap.Create;
  param.Add('p', PLATFORM);
  param.Add('v', VERSION);
  param.Add('a', FApp);
  jsonStr := HttpPost(BASE_URL + 'check_update.php', param);
  param.Free;
  FUpdateInfo := TUpdateInfo.fromJson(jsonStr);
end;

constructor TUpdateThread.Create(AApp: string);
begin
  inherited Create(True);
  FApp:= AApp;
  FUpdateInfo := nil;
  FreeOnTerminate:= True;
  OnTerminate:=@updateTerminated;
end;

destructor TUpdateThread.Destroy;
begin
  if (FUpdateInfo <> nil) then begin
    FUpdateInfo.Free;
  end;
  inherited Destroy;
end;

end.

