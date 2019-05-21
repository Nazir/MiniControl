{
Функции для защиты VMProtect

GetFirstHDD
FindCode
StartProtectModule
StopProtectModule
ClearCode
FillCode
ReadSpecBuf

}

unit Protect;

{$mode objfpc}{$H+}

interface

{$I Defines.inc}

{$IFDEF PROTECT}
procedure StartProtectModule;
procedure StopProtectModule;
{$ENDIF}
procedure FillCode(p_begin: pointer; sName: string);
procedure ClearCode(p_begin: pointer; sName: string);

implementation

uses
 {$IFDEF PROTECT}
 {WideGamma,} ModData, MD5, WMIHardwareID,
 {$ENDIF}
 {$IFDEF PROTECT_LITE}
  Registry,
 {$ENDIF}
 {$IFDEF PROTECT_HDD}
  SuperGetSN,
 {$ENDIF}
  LCLIntf, LCLType, LMessages, SysUtils, Classes;

{$IFDEF PROTECT}
type
  TRecCode = record
    hash: array [1..16] of Byte;
    len: Integer;
    p_code: pointer;
  end;
  PTRecCode = ^TRecCode;

var
  ListCode: TList;
  isFirst: boolean = true;
{$ENDIF}

function GetSerialNumber: string;
var
  iTemp: Integer;
 {$IFDEF PROTECT}
  HWID : THardwareId;
 {$ENDIF}
 {$IFDEF PROTECT_LITE}
  Registry: TRegistry;
  KeyName: string;
 {$ENDIF}
 {$IFDEF PROTECT_HDD}
  i, n: Integer;
  info: THDDInfo;
 {$ENDIF}
begin
  Result := '';
  {$IFDEF PROTECT}
  // <18.03.2013> WP Nazir
  try
    HWID:=THardwareId.Create(False);
    try
       HWID.GenerateHardwareId;
       Result := HWID.HardwareIdHex;
      //{$IFDEF Use_Jwscl}
       //Result := HWID.HardwareIdMd5
      //{$ENDIF}
       (*
       Writeln(Format('Hardware Id Generated in %s',[FormatDateTime('hh:mm:nn.zzz',dt)]));
       Writeln(Format('%s %s',['Buffer ',HWID.Buffer]));
       Writeln('');
       Writeln(Format('%s %s',['Hex  ',HWID.HardwareIdHex]));
      {$IFDEF Use_Jwscl}
       Writeln(Format('%s %s',['Md2  ',HWID.HardwareIdMd2]));
       Writeln(Format('%s %s',['Md4  ',HWID.HardwareIdMd4]));
       Writeln(Format('%s %s',['Md5  ',HWID.HardwareIdMd5]));
       Writeln(Format('%s %s',['SHA1 ',HWID.HardwareIdSHA]));
      {$ENDIF} *)
    finally
     HWID.Free;
    end;
 except
    on E:Exception do
    begin
        Writeln(E.Classname, ':', E.Message);
        Readln;
    end;
  end;
  // ^ <18.03.2013> WP Nazir
 {$ENDIF}
  {$IFDEF PROTECT_LITE}
  // <19.01.2012> WP Nazir
  KeyName := 'Software\Classes\CLSID\';
  KeyName := KeyName + '{F2D0F1A8-E89C-4203-AE97-DD0CDAC18625}';
  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey := HKEY_CURRENT_USER;

    if Registry.KeyExists(KeyName) then
    begin
      Registry.OpenKey(KeyName, False);
      Result := Registry.ReadString('VALUE');
    end;
  finally
    Registry.Free;
  end;
  // ^ <19.01.2012> WP Nazir
 {$ENDIF}
 {$IFDEF PROTECT_HDD}
  GetHDDInfo(info);
  n := 4;
  for i := 0 to 3 do
    if info[i].SerialNumber <> '' then
    begin
      n := i;
      Break;
    end;
  Result := info[n].SerialNumber;
 {$ENDIF}
end;

{$IFDEF PROTECT}
function FindCode(sName: string): PTRecCode;
var
  i: Integer;
  p: PTRecCode;
  hash: TMD5Digest;
begin
  Result := Pointer(random(1000));
  hash := MD5String(sName);
  for i := 0 to ListCode.Count - 1 do
  begin
    p := ListCode.Items[i];
    if CompareMem(@hash.v, @p^.hash, 16) then
    begin
      Result := p;
      Break;
    end;
  end;
end;

procedure ReadSpecBuf(var p_source: pointer; p_dist: pointer; len: Integer);
begin
  CopyMemory(p_dist, p_source, len);
  Inc(DWORD(p_source), len);
end;

procedure StartProtectModule;
var
  p: PTRecCode;
  iPos: Integer;
  p_buf: Pointer;
  sz: Integer;
begin
  // Открыте файла с кодом
  ListCode := TList.Create;
  p_buf := @FillVarData;
  ReadSpecBuf(p_buf, @sz, 4);
  iPos := 0;
  // Чтение из файла и запись в память
  while true do
  begin
    New(p);
    // Читаем длину кода

    ReadSpecBuf(p_buf, @p^.len, 4);
    GetMem(p^.p_code, p^.len);

    // Читаем хеш кода
    ReadSpecBuf(p_buf, @p^.hash, 16);
    // Читаем код
    ReadSpecBuf(p_buf, p^.p_code, p^.len);

    ListCode.Add(p);
    // Проверка на конец
    Inc(iPos, 4 + 16 + p^.len);
    if iPos >= sz then
      Break;
  end;
end;

procedure StopProtectModule;
var
  p: PTRecCode;
  i: Integer;
begin
  if isFirst then
    exit;
  for i := 0 to ListCode.Count - 1 do
  begin
    p := ListCode.Items[i];
    FreeMem(p^.p_code, p^.len);
    Dispose(p);
  end;
  ListCode.Clear;
  ListCode.Free;
end;
{$ENDIF}

{$WARNINGS OFF}
procedure ClearCode(p_begin: pointer; sName: string);
{$IFDEF PROTECT}
var
  OldProtect: DWORD;
  p: PTRecCode;
{$ENDIF}
begin
  {$IFDEF PROTECT}
  // Очистка машкодов в функции процесса после возврата
  // из защищаемой функции
  p := FindCode(sName);
  VirtualProtect(p_begin, p^.len, PAGE_EXECUTE_READWRITE, OldProtect);
  ZeroMemory(p_begin, p^.len);
 {$ELSE}
  // Давай защищай прогу!    Нет защиты

 {$IFDEF DEBUG}
 {$ELSE}
   {$IFDEF DEMO}
   {$ELSE}
  //  MessageBeep(MB_ICONERROR); // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   {$ENDIF}
 {$ENDIF}
//  MessageBox(0,'Давай защищай прогу!!','Нет защиты',MB_OK or MB_ICONEXCLAMATION);
  Exit;  //}
 {$ENDIF}
end;

procedure FillCode(p_begin: pointer; sName: string);
{$IFDEF PROTECT}
var
  p: PTRecCode;
  hash: TMD5Digest;
  OldProtect: DWORD;
{$ENDIF}
begin
  {$IFDEF PROTECT}
  if isFirst then
  begin
    StartProtectModule;
    isFirst := false;
  end;
  p := FindCode(sName);
  VirtualProtect(p_begin, p^.len, PAGE_EXECUTE_READWRITE, OldProtect);
  hash := MD5String(sName + GetSerialNumber);
  MoveMemory(p_begin, p^.p_code, p^.len);
  DeCodeWideGamma(p_begin, p^.len, hash);
  {$ELSE}
  // Давай защищай прогу!    Нет защиты
 {$IFDEF DEBUG}
 {$ELSE}
   {$IFDEF DEMO}
   {$ELSE}
  //  MessageBeep(MB_ICONERROR); // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   {$ENDIF}
 {$ENDIF}
//  MessageBox(0,'Давай защищай прогу!!','Нет защиты',MB_OK or MB_ICONEXCLAMATION);
  Exit;  //}
 {$ENDIF}
end;
{$WARNINGS ON}

initialization

finalization

  {$IFDEF PROTECT}
  StopProtectModule;
  {$ENDIF}

end.
