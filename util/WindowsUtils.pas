unit WindowsUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{$IFDEF WINDOWS}
function GetDefaultJavaPath(): string;
{$ENDIF}

implementation

uses
  baseData;

{$IFDEF WINDOWS}
function GetDefaultJavaPath: string;
var
  home: string;
begin
  // get java path
  home := GetEnvironmentVariable('JAVA_HOME');
  if (not home.EndsWith(SPLIT)) then home += SPLIT;
  Result := home + 'bin' + SPLIT + 'java.exe';
end;
{$ENDIF}

end.

