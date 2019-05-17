unit GammaUnit;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TSimpleGamma = class
  protected
    temp: TMemoryStream;
  public
    key: Integer;
    function Coder(str: string): string; overload;
    procedure Coder(var mem: TMemoryStream); overload;
    procedure Coder(var buffer; len_buf: Integer); overload;

    function DeCoder(str: string): string; overload;
    procedure DeCoder(var mem: TMemoryStream); overload;
    procedure DeCoder(var buffer; len_buf: Integer); overload;

    function HashString(APass, AHash: string; AMode: Integer = 16): string;
  end;

implementation

uses
  Math, StrUtils;

procedure TSimpleGamma.Coder(var mem: TMemoryStream);
var
  i: Integer;
  b: Byte;
  c: Byte;
  k: Byte;
  a: Byte;
begin
  temp := TMemoryStream.Create;
  temp.SetSize(mem.Size);
  temp.Seek(0,soFromBeginning);
  mem.Seek(0,soFromBeginning);
  RandSeed := key;
  for i := 0 to mem.Size - 1 do
  begin
    k := Random(255);
    mem.Read(a,1);
    b := Ord(a);
    c := (b+k) mod 256;
    temp.Write(c,1);
  end;
  mem.Clear;
  mem.LoadFromStream(temp);
  temp.Free;
end;

procedure TSimpleGamma.DeCoder(var mem: TMemoryStream);
var
  i: Integer;
  b: Byte;
  c: Byte;
  k: Byte;
  a: Byte;
begin
  temp := TMemoryStream.Create;
  temp.SetSize(mem.Size);
  temp.Seek(0,soFromBeginning);
  RandSeed := key;
  mem.Seek(0,soFromBeginning);
  for i := 0 to mem.Size - 1 do
  begin
    k := Random(255);
    mem.Read(a,1);
    b := Ord(a);
    c := (b-k) mod 256;
    temp.Write(c,1);
  end;
  mem.Clear;
  mem.LoadFromStream(temp);
  temp.Free;
end;

function TSimpleGamma.Coder(str: string): string;
var
  i: Integer;
  b: Byte;
  c: Byte;
  k: Byte;
begin
  Result := '';
  RandSeed := key;
  for i := 1 to Length(str) do
  begin
    k := Random(255);
    b := Ord(str[i]);
    c := (b+k) mod 256;
    Result := Result + Chr(c);
  end;
end;

function TSimpleGamma.DeCoder(str: string): string;
var
  i: Integer;
  b: Byte;
  c: Byte;
  k: Byte;
begin
  Result := '';
  RandSeed := key;
  for i := 1 to Length(str) do
  begin
    k := Random(255);
    b := Ord(str[i]);
    c := (b-k) mod 256;
    Result := Result + Chr(c);
  end;
end;

function TSimpleGamma.HashString(APass, AHash: string; AMode: Integer): string;
var
  n1, n2, nl: Integer;
  i, j, n: Integer;
  r: Real;
  s: string;
  tmp: string;
begin
  // Функция хэширования. Еле подобрал... за 4 часа...
  // AStr - хэширируемая сторока (пароль)
  // AHash - сторока для хэширования (логин)
  // AMode - Длина хэш-строки

  n1 := Length(APass);
  n2 := Length(AHash);

  RandSeed := 1024*AMode;
  if n1 < AMode then
    for i := 1 to 8 - n1 do
      APass := APass+ Chr(Ord(Random(256)));
  if n2 < AMode then
    for i := 1 to AMode - n2 do
      AHash := AHash + Chr(Ord(Random(256)));

  n1 := Length(APass);
  n2 := Length(AHash);

  if n1 < n2 then
    APass := APass+ Copy(AHash, n1 + 1, n2)
  else
    AHash := AHash + Copy(APass, n2 + 1, n1);

  nl := IfThen(n1 > n2, n1, n2);

  tmp := '';
  for i := 1 to nl do
  begin
    r := Sqrt(1000*Ord(APass[i]) + Ord(AHash[i]));
    r := r - Trunc(r);
    r := Sqrt(r);
    r := Sqrt(r);
    s := FloatToStr(r);
    s := Copy(s, 3, 3);
    tmp := tmp + Chr(Ord(StrToIntDef(s, 10)  mod 256));
  end;
  
  APass:= 'gfhjkm';
  s := '';
  for i := 1 to AMode do
  begin
    RandSeed := Ord(tmp[nl + 1 - i]);
    n := 0;
    for j := 1 to nl do
    begin
      if j <> Random(i) then
      begin
        n := n + Random(100*Ord(tmp[nl + 1 - j]));
      end;
    end;
    s := s + Chr(n mod 256);
  end;
  
  for i := 1 to AMode do
    if (s[i] = #13) or (s[i] = #10) then
      s[i] := Chr(Ord(s[i]) + Random(100));
  //Result := 'Купи что ли...: ' + s;
  Result := s;
end;

procedure TSimpleGamma.Coder(var buffer; len_buf: Integer);
var
  i: Integer;
  b: Byte;
  c: Char;
  k: Byte;
  p: Pointer;
begin
  RandSeed := key;
  p := @buffer;
  { TODO : ! }
  //for i := 1 to len_buf do
  //begin
  //  k := Random(255);
  //  Move(@b, p, 1);
  //  c := Chr((b + k) mod 256);
  //  Move(p, @c, 1);
  //  Inc(integer(p));
  //end;
end;

procedure TSimpleGamma.DeCoder(var buffer; len_buf: Integer);
var
  i: Integer;
  b: Byte;
  c: Char;
  k: Byte;
  p: Pointer;
begin
  RandSeed := key;
  p := @buffer;
  { TODO : ! }
  //for i := 1 to len_buf do
  //begin
  //  k := Random(255);
  //  Move(@b, p, 1);
  //  c := Chr((b - k) mod 256);
  //  Move(p, @c, 1);
  //  Inc(integer(p));
  //end;
end;

end.
