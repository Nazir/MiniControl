unit WideGamma;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, LMessages, MD5;


function CodeWideGamma(buf: pointer; Len: integer; key: TMD5Digest): Integer;
function DeCodeWideGamma(buf: pointer; Len: integer; key: TMD5Digest): Integer;

implementation

procedure Code(buf: pointer; Len: integer; key: integer);
var
  i: Integer;
begin
  RandSeed := key;
  for i := 0 to Len - 1 do
    Byte(pointer(integer(buf) + i)^) := (Byte(pointer(integer(buf) + i)^) + random(255)) mod 256;
end;

procedure DeCode(buf: pointer; Len: integer; key: integer);
var
  i: Integer;
begin
  RandSeed := key;
  for i := 0 to Len - 1 do
    Byte(pointer(integer(buf) + i)^) := (Byte(pointer(integer(buf) + i)^) - random(255)) mod 256;
end;

{$WARNINGS OFF}
function CodeWideGamma(buf: pointer; Len: integer; key: TMD5Digest): Integer;
begin
  DeCode(buf, Len, key.A);
  DeCode(buf, Len, key.B);
  DeCode(buf, Len, key.C);
  DeCode(buf, Len, key.D);
end;

function DeCodeWideGamma(buf: pointer; Len: integer; key: TMD5Digest): Integer;
begin
  Code(buf, Len, key.A);
  Code(buf, Len, key.B);
  Code(buf, Len, key.C);
  Code(buf, Len, key.D);
end;
{$WARNINGS ON}

end.
