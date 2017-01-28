unit EncryptUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lockbox;

function md5EncryptString(str: string): string;

implementation

function md5EncryptString(str: string): string;
var
  d: TMD5Digest;
begin
  Result := '';
  try
    StringHashMD5(d, str);
    Result := BufferToHex(d, SizeOf(d));
  except
  end;
end;

end.

