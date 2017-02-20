unit CommandUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;

type
  TCommandType = (ctDecompile, ctCompile, ctInstallFramework, ctVersion, ctUpdate, ctCommand);

  TOnCommandOutput = procedure(Sender: TObject; ACmdType: TCommandType; AOutput: string) of object;
  TOnCommandComplete = procedure (Sender: TObject; ACmdType: TCommandType; AParam: array of string) of object;

  { TCommandThread }

  TCommandThread = class(TThread)
  private
    FCmdType: TCommandType;
    FParam: array of string;
    FOnCommandComplete: TOnCommandComplete;
    FOnCommandOutput: TOnCommandOutput;
    // tmp
    FTmpOutput: string;
    FTmpProjectPath: string;
    FTmpDistPath: string;
    FTmpVersion: string;
    procedure commandComplete(Sender: TObject);
  protected
    procedure SendSync();
    procedure Execute; override;
  public
    constructor Create(ACmdType: TCommandType; AParam: array of string);
  published
    property OnCommandOutput: TOnCommandOutput read FOnCommandOutput write FOnCommandOutput;
    property OnCommandComplete: TOnCommandComplete read FOnCommandComplete write FOnCommandComplete;
  end;

implementation

uses
  config;

{ TCommandThread }

function ExtractPureFileName(path: string): string;
var
  r: string;
begin
  r := ExtractFileName(path);
  r := r.Replace('.apk', '', [rfIgnoreCase, rfReplaceAll]);
  Result := r;
end;

procedure TCommandThread.commandComplete(Sender: TObject);
begin
  if Assigned(FOnCommandComplete) then begin
    case FCmdType of
    ctDecompile: FOnCommandComplete(Self, FCmdType, [FTmpProjectPath]);
    ctCompile: FOnCommandComplete(Self, FCmdType, [FTmpDistPath]);
    ctInstallFramework: FOnCommandComplete(Self, FCmdType, []);
    ctVersion: FOnCommandComplete(Self, FCmdType, [FTmpVersion]);
    ctUpdate: ; // TODO: update apk tool callback
    ctCommand: FOnCommandComplete(Self, FCmdType, []);
    end;
  end;
end;

procedure TCommandThread.SendSync;
begin
  FOnCommandOutput(Self, FCmdType, FTmpOutput);
end;

procedure TCommandThread.Execute;
const
  BUF_SIZE = 2048;
var
  AProcess: TProcess;
  bytesRead: Integer;
  buffer: array[0..BUF_SIZE - 1] of byte;
  i: Integer;
  outputPath: string;
  lineCount: Integer = 0;
begin
  AProcess := TProcess.Create(nil);
  case FCmdType of
  ctDecompile:
    begin
      AProcess.Executable:= GlobalConfig.JavaBinaryPath;
      AProcess.Parameters.Add('-jar');
      AProcess.Parameters.Add(ExtractFilePath(ParamStr(0)) + 'bin/apktool.jar');
      AProcess.Parameters.Add('d');
      if (FParam[2] = '1') then AProcess.Parameters.Add('-r');
      if (FParam[3] = '1') then AProcess.Parameters.Add('-s');
      AProcess.Parameters.Add('-o');
      outputPath:= FParam[1];
      if (not outputPath.EndsWith('/')) then outputPath += '/';
      outputPath += ExtractPureFileName(FParam[0]) + '/';
      FTmpProjectPath := outputPath + 'apktool.yml';
      AProcess.Parameters.Add(outputPath);
      AProcess.Parameters.Add(FParam[0]);
    end;
  ctCompile:
    begin
      // compile
      AProcess.Executable:= GlobalConfig.JavaBinaryPath;
      AProcess.Parameters.Add('-jar');
      AProcess.Parameters.Add(ExtractFilePath(ParamStr(0)) + 'bin/apktool.jar');
      AProcess.Parameters.Add('b');
      AProcess.Parameters.Add(FParam[0]);
      FTmpDistPath:= FParam[0];
      if (not FTmpDistPath.EndsWith('/')) then FTmpDistPath += '/';
      FTmpDistPath += 'dist/';
    end;
  ctInstallFramework:
    begin
      AProcess.Executable:= GlobalConfig.JavaBinaryPath;
      AProcess.Parameters.Add('-jar');
      AProcess.Parameters.Add(ExtractFilePath(ParamStr(0)) + 'bin/apktool.jar');
      AProcess.Parameters.Add('if');
      AProcess.Parameters.Add(FParam[0]);
    end;
  ctVersion:
    begin
      AProcess.Executable:= GlobalConfig.JavaBinaryPath;
      AProcess.Parameters.Add('-jar');
      AProcess.Parameters.Add(ExtractFilePath(ParamStr(0)) + 'bin/apktool.jar');
      AProcess.Parameters.Add('-version');
    end;
  ctUpdate:
    begin
      // TODO: update apktool
    end;
  ctCommand:
    begin
      // common command
      AProcess.Executable:= FParam[0];
      for i := 1 to Length(FParam) - 1 do AProcess.Parameters.Add(FParam[i]);
    end;
  end;

  AProcess.Options:= [poUsePipes];
  AProcess.Execute;

  repeat
    FillChar(buffer, BUF_SIZE, 0);
    bytesRead:= AProcess.Output.Read(buffer, BUF_SIZE);
    FTmpOutput:= string(StringOf(buffer));
    if (FCmdType = ctVersion) and (lineCount = 0) then FTmpVersion:= FTmpOutput.Trim;
    lineCount += 1;
    if (Assigned(FOnCommandOutput)) then Synchronize(@SendSync);
  until bytesRead = 0;
  AProcess.Free;
end;

constructor TCommandThread.Create(ACmdType: TCommandType; AParam: array of string);
var
  i: Integer;
begin
  inherited Create(True);
  FreeOnTerminate:= True;
  OnTerminate:=@commandComplete;
  FCmdType:= ACmdType;

  // param sequence
  // Decompile:
  //     APKPath, OutputPath, isNoRes, isNoSrc
  //     isNoRes, isNoSrc: 0: decompile all  1: decompile without res/src
  // compile:
  //     ProjectPath
  // installFramework:
  //     <jar> path
  // version
  //     ['']
  // update apktool
  //     <url> <local file>
  // common:
  //     executable, param1, param2, ...

  SetLength(FParam, Length(AParam));
  for i := 0 to Length(AParam) - 1 do FParam[i] := AParam[i];
end;

end.

