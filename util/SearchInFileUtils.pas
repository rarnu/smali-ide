unit SearchInFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TOnSearchInFileFound = procedure(Sender: TObject; AFilePath: string; AIndex: Integer; AShortcut: string) of object;

  { TSearchInFileThread }

  TSearchInFileThread = class(TThread)
  private
    FOnSearchInFileFound: TOnSearchInFileFound;
    FPath: string;
    FKey: string;

    FIsAborted: Boolean;

    // tmp
    FTmpPath: string;
    FTmpIndex: Integer;
    FTmpShortcut: string;

    procedure SendSync();
    procedure SearchFile(basePath: string);
    function IsFileForSearch(APath: string): Boolean;
    procedure GrepInFile(APath: string);
  protected
    procedure Execute; override;
  public
    constructor Create(APath: string; AKey: string);
    procedure AbortSearch();
  published
    property OnSearchInFileFound: TOnSearchInFileFound read FOnSearchInFileFound write FOnSearchInFileFound;
  end;

implementation

{ TSearchInFileThread }


procedure TSearchInFileThread.SendSync;
begin
  FOnSearchInFileFound(Self, FTmpPath, FTmpIndex, FTmpShortcut);
end;

function TSearchInFileThread.IsFileForSearch(APath: string): Boolean;
var
  ext: string;
begin
  ext := string(ExtractFileExt(APath)).ToLower;
  Result := (ext = '.smali') or (ext = '.xml') or (ext = '.txt');
end;

procedure TSearchInFileThread.GrepInFile(APath: string);
var
  sl: TStringList;
  AStart: Integer = 0;
  s: string;
begin
  sl := TStringList.Create;
  sl.LoadFromFile(APath);
  s := sl.Text;
  sl.Free;
  AStart:= s.IndexOf(FKey, AStart);
  while AStart > 0 do begin
    if (Assigned(FOnSearchInFileFound)) then begin
      FTmpPath:= APath;
      FTmpIndex:= AStart;
      FTmpShortcut:= s.Substring(AStart, FKey.Length + 20);
      Synchronize(@SendSync);
    end;
    AStart:= s.IndexOf(FKey, AStart + FKey.Length);
  end;
end;

procedure TSearchInFileThread.SearchFile(basePath: string);
var
  src: TSearchRec;
  p: string;
begin
  if (FindFirst(basePath + '*', faAnyFile, src) = 0) then begin
    repeat
      if FIsAborted then Break;
      if (src.Name = '.') or (src.Name = '..') then Continue;
      p := basePath + src.Name;
      if (DirectoryExists(p)) then begin
        p += '/';
        SearchFile(p);
      end else begin
        if (IsFileForSearch(p)) then begin
          GrepInFile(p);
        end;
      end;
    until FindNext(src) <> 0;
    FindClose(src);
  end;
end;

procedure TSearchInFileThread.Execute;
begin
  SearchFile(FPath);
end;

constructor TSearchInFileThread.Create(APath: string; AKey: string);
begin
  inherited Create(True);
  FPath:= APath;
  FKey:= AKey;
  FIsAborted := False;
  if (not FPath.EndsWith('/')) then FPath += '/';
  FreeOnTerminate:= True;
end;

procedure TSearchInFileThread.AbortSearch;
begin
  FIsAborted := True;
end;

end.

