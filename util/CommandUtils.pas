unit CommandUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;

type
  TCommandType = (ctDecompile, ctCompile, ctInstallFramework, ctUpdate, ctCommand);

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
    ctUpdate:;
    ctCommand:;
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

  outputPath: string;
begin
  AProcess := TProcess.Create(nil);
  case FCmdType of
  ctDecompile:
    begin
      AProcess.Executable:= '/usr/bin/java';
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
      AProcess.Executable:= '/usr/bin/java';
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
      AProcess.Executable:= '/usr/bin/java';
      AProcess.Parameters.Add('-jar');
      AProcess.Parameters.Add(ExtractFilePath(ParamStr(0)) + 'bin/apktool.jar');
      AProcess.Parameters.Add('if');
      AProcess.Parameters.Add(FParam[0]);
    end;
  ctUpdate:
    begin
      // TODO: update apktool
    end;
  ctCommand:
    begin
      // TODO: common command
    end;
  end;

  AProcess.Options:= [poUsePipes];
  AProcess.Execute;
  repeat
    FillChar(buffer, BUF_SIZE, 0);
    bytesRead:= AProcess.Output.Read(buffer, BUF_SIZE);
    if (Assigned(FOnCommandOutput)) then begin
      FTmpOutput:= string(StringOf(buffer));
      Synchronize(@SendSync);
    end;
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

  SetLength(FParam, Length(AParam));
  for i := 0 to Length(AParam) - 1 do FParam[i] := AParam[i];
end;

end.

