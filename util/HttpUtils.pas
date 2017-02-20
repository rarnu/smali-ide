unit HttpUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fphttp, strutils, fgl;

type
  TParamMap = specialize TFPGMap<string, string>;

function HttpGet(AUrl: string; AParam: TParamMap = nil): string;
function HttpPost(AUrl: string; AParam: TParamMap = nil): string;
function HttpPostFile(AUrl: string; AParam: TParamMap = nil; AFiles: TParamMap = nil): string;

implementation

function HttpGet(AUrl: string; AParam: TParamMap): string;
var
  innerUrl: string;
  innerParam: string = '';
  i: Integer;
begin
  Result := '';
  if (AParam <> nil) then for i := 0 to AParam.Count - 1 do innerParam += Format('%s=%s&', [AParam.Keys[i], AParam.Data[i]]);
  innerParam:= innerParam.TrimRight(['&']);
  innerUrl:= AUrl + '?' + innerParam;
  with TFPHTTPClient.Create(nil) do begin
    try
      Result := Get(innerUrl);
    except
    end;
    Free;
  end;
end;

function HttpPost(AUrl: string; AParam: TParamMap): string;
var
  postParam: TStringList = nil;
  i: Integer;
begin
  Result := '';
  if (AParam <> nil) then begin
    postParam := TStringList.Create;
    for i := 0 to AParam.Count - 1 do postParam.Add(Format('%s=%s', [AParam.Keys[i], AParam.Data[i]]));
  end;
  with TFPHTTPClient.Create(nil) do begin
    try
      Result := IfThen(postParam <> nil, FormPost(AUrl, postParam), Post(AUrl));
    except
    end;
    Free;
  end;
  if (postParam <> nil) then postParam.Free;
end;

function HttpPostFile(AUrl: string; AParam: TParamMap; AFiles: TParamMap
  ): string;
var
  postParam: TStringList;
  i: Integer;
  ss: TStringStream;
begin
  Result := '';
  if (AParam <> nil) then begin
    postParam := TStringList.Create;
    for i := 0 to AParam.Count - 1 do postParam.Add(Format('%s=%s', [AParam.Keys[i], AParam.Data[i]]));
  end;
  with TFPHTTPClient.Create(nil) do begin
    ss := TStringStream.Create('');
    if (AFiles <> nil) and (AFiles.Count > 0) then begin
      try
        FileFormPost(AUrl, postParam, AFiles.Keys[0], AFiles.Data[0], ss);
        Result := ss.DataString;
      except
      end;
    end;
    ss.Free;
    Free;
  end;
  if (postParam <> nil) then postParam.Free;
end;

end.

