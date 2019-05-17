unit UtilsUnit;

{$mode objfpc}{$H+}

{******************************************************************************}
{                                                                              }
{ Unit: Utils                                                                  }
{                                                                              }
{  Copyright: Nazir © 2002-2019                                                }
{  Development: Nazir K. Khusnutdinov (aka Naziron or Wild Pointer)            }
{  Разработчик: Хуснутдинов Назир Каримович                                    }
{  Email: naziron@gmail.com                                                    }
{  Git: https://github.com/Nazir                                               }
{                                                                              }
{  Modified: 12.04.2006                                                        }
{  Modified: 15.05.2019                                                        }
{                                                                              }
{******************************************************************************}

interface

uses
  LCLIntf, LCLType, LMessages,
  // http://wiki.freepascal.org/Platform_defines
  {$IFDEF WINDOWS}windows,{$ENDIF}
  Forms, Classes, SysUtils, ActiveX,
  Variants, ComObj, DateUtils, ShlObj,
  Masks, Registry;

type
  TDynArray = array of variant;
  PDynArray=^TDynArray;

const
  //
  iKey_string = 413696;

  procedure CodeBuf(Buf: Pointer; Len: Integer; Key: Integer);
  procedure DeCodeBuf(Buf: Pointer; Len: Integer; Key: Integer);
  // Расшифровка строки по ключу
  function DeCoderString(str: string; key: integer): string;

  // Полное удаление директории с покаталогами и файлами
  function FullRemoveDir(Dir: string; DeleteAllFilesAndFolders,
                         StopIfNotAllDeleted, RemoveRoot: boolean): Boolean;

  // Определить, находится ли файл в использовании
  function IsFileInUse(FileName: TFileName): Boolean;
  // Проверка на необходимые файлы для программы
  function CheckNeededFiles(AHandle: Integer = 0): Boolean;

  // Заменяет в AText все строки из списка AOldPattern на ANewPattern
  function ReplaceTagsInText(AText: string;
    AOldPattern, ANewPattern: TStrings): string;

  //Название месяца
  function MonthName(ANumber: Integer; APlural: Boolean = True): string;
  // Длинная дата
  function LongDate(ADate: TDateTime): string;

  // Перевод типа Boolean в строку
  function BooleanToString(ABoolean: Boolean; AUseBoolStrs: Boolean = False): string;
  function IntToBoolean(AValue: Integer): Boolean;
  function BooleanToInt(AValue: Boolean): Integer;

  //// Получение списка GUID'ов по категории
  //function Get_CATID_List(ACATID: TGUID; AUseDisabled: Boolean = True): TStrings;

  // Сохранения текста в файл
  procedure SaveTextToFile(AFileName, AText: string; AMethod: Integer = 0; AMaxFileSize: Integer = 31457280 {30 Mb});
  // Загрузка текста из файла
  function LoadTextFromFile(AFileName: string): string;

  // Получение строки со случайными байтами
  function GetRandomString(ALength: Integer = 16): string;

  // Внутренний парсер настроек
  function SettingsParser(ASettings, ASection, AIdent: string): string;
  function GetFilesList(AFindMask: string): string;

  //// Переключение языков
  //procedure SetKeyboardLayoutRU;
  //procedure SetKeyboardLayoutEN;
  // имя, идентифицирующее компьютер в сети
  function GetComputerNetName: string;
  //Возвращает логин пользователя в текущем сеансе.
  function GetStrUserName: string;
  //
  procedure CastVariantToArray(const KeyValues:Variant; var VarArray: TDynArray);

  // Конвертация денежных сумм в строковое выражение
  function MoneyToStr(DD: string): string;

  // Объединиение строки ACount раз с раделителем ADivider    <15.10.2009>
  function MultipleConcat(AStr: string; ACount: Integer; ADivider: string = ''): string;

  function GetFileDate(AFileName: string; ADefault: TDateTime = 0): TDateTime;
  // Дата последнего изменения файла

  function SetFileDate(const AFileName : string; const AFileDate : TDateTime): Boolean;


  function InsSymToStr(AStr: string; ALength: Integer; ASymbol: string = '0';
    ATextDirectionLeft: Boolean = True): string;
  function GetMinuteFromStr(AStr: string): Integer;
  function GetStrFromMinute(AMinute: Integer): string;
  function MinutsToTime(AMinutes: Integer; ASeconds: Boolean = True): string;
  function DirSize(Dir: string): Integer;
  // Переменные среды
  function GetEnvironmentVariableString(AName: string): string;
  // Поиск файлов по маске по всему дереву каталогов, начиная с указанного
  procedure FindFiles(AStartFolder, AMask: string; AList: TStringList;
  AScanSubFolders: Boolean = True);
  function GetQuarter(AMonth: Integer): Integer;
  // Получение квартала по месяцу
  function arCyr2LatUp(AStr: string; ASpaceCharacter: string = ' ';
    APointCharacter: string = '.'): string;
  //Переводит строку на заглавные буквы, а далее с кирилицы на латиницу
  function FileCreationDate(const FileName: string): Integer;
  // Дата создания файла
  function FileLastWriteDate(const FileName: string): Integer;
  // Дата последнего изменения файла
  function FileLastAccessDate(const FileName: string): Integer;
  // Дата последнего открытия файла

  function GetSpecialPath(CSIDL: Word): string;

  procedure WritelnOEM(AText: string);

  function SysFolder(Param: string): string;
  procedure CreateLink(const PathObj, WorkDir, PathLink, Description, Arguments: string;
                       ShowCmd: Integer; AIconPath: string = ''; AIconIndex: Integer = 0);
  procedure CreateShortcut(ADescription: string = ''; APathLink: string = '';
                           AArguments: string = '';
                           AIconPath: string = ''; AIconIndex: Integer = 0);

  // Диалог выбора папки
  function BrowseDialog(const Title: string; const Flag: Integer; AHandle: THandle; AInitFolder: string = ''): string;
  function BrowseDialogExtra(AHandle: THandle; const ATitle: string; AInitFolder: string = ''): string;

implementation

uses SysConst;

{$I Defines.inc}

procedure CodeBuf(Buf: Pointer; Len: Integer; Key: Integer);
var
  i: Integer;
begin
  try
  RandSeed := Key;
  for i := 0 to Len - 1 do
    Byte(Pointer(Integer(Buf) + i)^) := (Byte(Pointer(Integer(Buf) + i)^) + Random(255)) mod 256;
  except
  end;
end;

procedure DeCodeBuf(Buf: Pointer; Len: Integer; Key: Integer);
var
  i: Integer;
begin
  try
  RandSeed := Key;
  for i := 0 to Len - 1 do
    Byte(Pointer(Integer(Buf) + i)^) := (Byte(Pointer(Integer(Buf) + i)^) - Random(255)) mod 256;
  except
  end;
end;

function DeCoderString(str: string; key: integer): string;
var
  i: integer;
  b: byte;
  c: byte;
  k: byte;
begin
  Result := '';
  RandSeed := key;
  for i := 1 to length(str) do
  begin
    k := random(255);
    b := ord(str[i]);
    c := (b-k) mod 256;
    Result := Result + chr(c);
  end;
end;

{$WARNINGS OFF}
function FullRemoveDir(Dir: string; DeleteAllFilesAndFolders,
  StopIfNotAllDeleted, RemoveRoot: boolean): Boolean;
var
  i: Integer;
  SRec: TSearchRec;
  FN: string;
begin
  Result := False;
  if not DirectoryExists(Dir) then
    exit;
  Result := True;
  // Добавляем слэш в конце и задаем маску - "все файлы и директории"
  Dir := IncludeTrailingBackslash(Dir);
  i := FindFirst(Dir + '*', faAnyFile, SRec);
  try
    while i = 0 do
    begin
      // Получаем полный путь к файлу или директорию
      FN := Dir + SRec.Name;
      // Если это директория
      if SRec.Attr = faDirectory then
      begin
        // Рекурсивный вызов этой же функции с ключом удаления корня
        if (SRec.Name <> '') and (SRec.Name <> '.') and (SRec.Name <> '..') then
        begin
          if DeleteAllFilesAndFolders then
            FileSetAttr(FN, faArchive);
          Result := FullRemoveDir(FN, DeleteAllFilesAndFolders,
            StopIfNotAllDeleted, True);
          if not Result and StopIfNotAllDeleted then
            exit;
        end;
      end
      else // Иначе удаляем файл
      begin
        if DeleteAllFilesAndFolders then
          FileSetAttr(FN, faArchive);
        Result := SysUtils.DeleteFile(FN);
        if not Result and StopIfNotAllDeleted then
          exit;
      end;
      // Берем следующий файл или директорию
      i := FindNext(SRec);
    end;
  finally
    SysUtils.FindClose(SRec);
  end;
  if not Result then
    exit;
  if RemoveRoot then // Если необходимо удалить корень - удаляем
    if not RemoveDir(Dir) then
      Result := false;
end;
{$WARNINGS ON}

function IsFileInUse(FileName: TFileName): Boolean;
// Определить, находится ли файл в использовании
var
 FS: TFileStream;
begin
  Result := False;
  try
    FS := TFileStream.Create(FileName, fmOpenReadWrite, fmShareExclusive);
  except
    Result := True;
  end;
end;

function CheckNeededFiles(AHandle: Integer = 0): Boolean;
// Проверка на наличие необходимых файлов для программы
var
  slTemp: TStrings;
  sTemp: string;
  iCounter: Integer;
begin
  Result := False;
  sTemp := ExtractFilePath(ParamStr(0)) + 'System\' +
           ExtractFileName(ParamStr(0)) + '.nf';

  if not FileExists(sTemp) then
    Exit;

  slTemp := TStringList.Create;
  slTemp.LoadFromFile(sTemp);
  sTemp := '';
  for iCounter := 0 to slTemp.Count - 1 do
  begin
    if not FileExists(slTemp.Strings[iCounter]) then
      sTemp := sTemp + slTemp.Strings[iCounter] + #13#10;
  end;
  slTemp.Free;

  if sTemp <> '' then
  begin
    MessageBox(AHandle, PChar('Отсутствуют необходимые файлы: '#13#10 +  sTemp),
               'Предупреждение', MB_OK or MB_ICONEXCLAMATION);
    Result := False;
  end;
end;

function ReplaceTagsInText(AText: string;
  AOldPattern, ANewPattern: TStrings): string;
// Заменяет в AText все строки из списка AOldPattern на ANewPattern
var
  iCounter, iTagsCount: Integer;
  sTemp: string;
begin
  iTagsCount := 0;
  sTemp := AText;
  if AOldPattern.Count = ANewPattern.Count then
    iTagsCount := AOldPattern.Count;
  if iTagsCount > 0 then
  for iCounter := 0 to iTagsCount - 1 do
    sTemp := StringReplace(sTemp,
                            AOldPattern.Strings[iCounter],
                            ANewPattern.Strings[iCounter],
                            [rfReplaceAll, rfIgnoreCase]);
  Result := sTemp;
end;

function MonthName(ANumber: Integer; APlural: Boolean = True): string;
//Название месяца
begin
  Result := EmptyStr;
  if APlural then
  case ANumber of
    1: Result := 'января';
    2: Result := 'февраля';
    3: Result := 'марта';
    4: Result := 'апреля';
    5: Result := 'мая';
    6: Result := 'июня';
    7: Result := 'июля';
    8: Result := 'августа';
    9: Result := 'сентября';
    10: Result := 'октября';
    11: Result := 'ноября';
    12: Result := 'декабря';
  end
  else
  case ANumber of
    1: Result := 'январь';
    2: Result := 'февраль';
    3: Result := 'март';
    4: Result := 'апрель';
    5: Result := 'май';
    6: Result := 'июнь';
    7: Result := 'июль';
    8: Result := 'август';
    9: Result := 'сентябрь';
    10: Result := 'октябрь';
    11: Result := 'ноябрь';
    12: Result := 'декабрь';
  end
end;

function LongDate(ADate: TDateTime): string;
//Длинная дата
begin
  Result := IntToStr(DayOf(ADate)) + ' ' + MonthName(MonthOf(ADate)) + ' ' + IntToStr(YearOf(ADate));
end;

function BooleanToString(ABoolean: Boolean; AUseBoolStrs: Boolean): string;
begin
  Result := '0';
  if AUseBoolStrs then
  begin
    if ABoolean then
      Result := 'True'
    else
      Result := 'False'
  end
  else
  begin
    if ABoolean then
      Result := '1'
    else
      Result := '0'
  end
end;

function IntToBoolean(AValue: Integer): Boolean;
begin
  if AValue = 1 then
    Result := True
  else
    Result := False
end;

function BooleanToInt(AValue: Boolean): Integer;
begin
  if AValue = True then
    Result := 1
  else
    Result := 0
end;

//function Get_CATID_List(ACATID: TGUID; AUseDisabled: Boolean): TStrings;
//var
//  EnumGUID: IEnumGUID;
//  Fetched: Cardinal;
//  Guid: TGUID;
//  Rslt: HResult;
//  CatInfo: ICatInformation;
//  slTemp: TStrings;
//  arrWinDir: array[0..MAX_PATH] of Char;
//begin
//  slTemp := TStringList.Create;
//  GetWindowsDirectory(arrWinDir, SizeOf(arrWinDir));
//  if FileExists(string(arrWinDir) + '\DB_DisabledPlugins.lst') then
//    slTemp.LoadFromFile(string(arrWinDir) + '\DB_DisabledPlugins.lst')
//  else
//    slTemp.Clear;
//    
//  Result := TStringList.Create;
//  Rslt := CoCreateInstance(CLSID_StdComponentCategoryMgr, nil,
//    CLSCTX_INPROC_SERVER, ICatInformation, CatInfo);
//  if Succeeded(Rslt) then
//  begin
//    OleCheck(CatInfo.EnumClassesOfCategories(1, @ACATID, 0, nil, EnumGUID));
//    while EnumGUID.Next(1, Guid, Fetched) = S_OK do
//    try
//      if AUseDisabled then
//      begin
//        if slTemp.IndexOf(GUIDToString(Guid)) = -1 then
//          Result.Add(GUIDToString(Guid));
//      end   
//      else
//        Result.Add(GUIDToString(Guid));
//    except
//      // Ignore
//    end;
//  end;
//
//  slTemp.Free;
//end;

procedure SaveTextToFile(AFileName, AText: string; AMethod: Integer = 0; AMaxFileSize: Integer = 31457280 {30 Mb});
// Сохранения текста в файл
var
  fFile: TextFile;
  bFile: file of Byte;
  liTemp: Longint;
begin
  {$I-}
  if FileExists(AFileName) then
  begin
    try
      AssignFile(bFile, AFileName);
      Reset(bFile);
      liTemp := FileSize(bFile);
      //MessageBox(0, PChar(IntToStr(FileSize(bFile))), 'FileSize', 0);
    finally
      CloseFile(bFile);
    end;
    if liTemp > AMaxFileSize then
      Erase(bFile);
  end;

  try
    AssignFile(fFile, AFileName);
    if AMethod = 0 then
      Rewrite(fFile)
    else
    begin
      if FileExists(AFileName) then
        Append(fFile)
      else
        Rewrite(fFile);
    end;

    case AMethod of
      0: // Запись AText в новый файл
      begin
        Write(fFile, AText);
      end;
      1: // Запись AText в открытый файл
      begin
        Write(fFile, AText);
      end;
      2: // Вставка AText в открытый файл
      begin
        WriteLn(fFile, AText);
      end;
    end;
  finally
    Flush(fFile);
    CloseFile(fFile);
  end;
  {$I+}
end;

function LoadTextFromFile(AFileName: string): string;
// Загрузка текста из файла
var
  slTemp: TStrings;
begin
  Result := EmptyStr;
  if not FileExists(AFileName) then
    Exit;
  slTemp := TStringList.Create;
  slTemp.LoadFromFile(AFileName);
  Result := slTemp.Text;
  slTemp.Free;
end;

function RandomInteger(iLow, iHigh: Integer): Integer;
begin
  Result := Trunc(Random(iHigh - iLow)) + iLow;
end;

function RandomString(iLength: Integer): string;
begin
  Result := '';
  Randomize;
  while Length(Result) < iLength do
    Result := Result + IntToStr(RandomInteger(0, High(Integer)));
  if Length(Result) > iLength then
    Result := Copy(Result, 1, iLength);
end;

function GetRandomString(ALength: Integer): string;
{var
  iCounter: Integer;//}
begin
  Result := RandomString(ALength);
 { Randomize;
  for iCounter := 1 to ALength do
    Result := Result + Chr(Random(150) + 16); //}
end;


function SettingsParser(ASettings, ASection, AIdent: string): string;
var
  sTemp: string;
  iPosIndex, iPosCount: Integer;
begin
  sTemp := '';
  Result := sTemp;

  sTemp := '<' + ASection + '>';
  iPosIndex := Pos(sTemp, ASettings) + Length(sTemp);
  sTemp := '</' + ASection + '>';
  iPosCount := Pos(sTemp, ASettings) - iPosIndex;

  sTemp := Copy(ASettings, iPosIndex, iPosCount);

  if AIdent <> '' then
  begin
    sTemp := sTemp + '|';
    iPosIndex := Pos(AIdent + '=', sTemp) + Length(AIdent + '=');
    Delete(sTemp, 1, iPosIndex - 1);
    iPosCount := Pos('|', sTemp) - 1;
    sTemp := Copy(sTemp, 1, iPosCount);
  end;
  Result := sTemp;
end;

function GetFilesList(AFindMask: string): string;
var
  srTemp: TSearchRec;
begin
  if FindFirst(AFindMask, faAnyFile, srTemp) = 0 then
  begin
    repeat
      if (srTemp.Attr and faAnyFile) = srTemp.Attr then
        Result := Result + srTemp.Name + #13#10;
    until FindNext(srTemp) <> 0;
    FindClose(srTemp);
  end;
end;

//procedure SetKeyboardLayoutRU;
//var
//  Layout: array [0.. KL_NAMELENGTH] of char;
//begin
//  LoadKeyboardLayout(StrCopy(Layout, '00000419'), KLF_ACTIVATE);
//end;
//
//procedure SetKeyboardLayoutEN;
//var
//  Layout: array [0.. KL_NAMELENGTH] of char;
//begin
//  LoadKeyboardLayout(StrCopy(Layout, '00000409'), KLF_ACTIVATE);
//end;

function GetComputerNetName: string;
// имя, идентифицирующее компьютер в сети
var
  {$if defined(Windows)}
  c: array[0..127] of Char;
  //computer: string;
  sz: dword;
  {$else}
  AProcess: TProcess;
  AStringList: TStringList;
  {$endif}
begin
  {$if defined(Windows)}
  sz := SizeOf(c);
  GetComputerName(c, sz);
  Result := c;
  {$else}
  AProcess := TProcess.Create(nil);
  AStringList := TStringList.Create;
  AProcess.CommandLine := 'echo $HOSTNAME';
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  AProcess.Execute;
  AStringList.LoadFromStream(AProcess.Output);
  Result:=AStringList.Strings[0];
  AStringList.Free;
  AProcess.Free;
  {$endif}
end;

function GetStrUserName : string;
//Возвращает логин пользователя в текущем сеансе.
var
  lwLen : Longword;
begin
  lwLen := 255;
  SetLength(Result, lwLen);
  if GetUserName(PChar(Result), lwLen) then
    SetLength(Result, lwLen - 1)
  else
    Result := ''
  ;
end;

procedure CastVariantToArray(const KeyValues:Variant; var VarArray: TDynArray);
begin
  if VarIsArray(KeyValues) then
   VarArray:=KeyValues
  else
  begin
    SetLength(VarArray,1);
    VarArray[0]:=KeyValues;
  end;
end;

{$WARNINGS OFF}
function MoneyToStr(DD: string): string;
// Конвертация денежных сумм в строковое выражение
type
  TTroyka = array[1..3] of Byte;
  TMyString = array[1..19] of string[12];
var
  S, OutS, S2: string;
  k, L, kk: Integer;
  Troyka: TTroyka;
  V1: TMyString;
  Mb: Byte;
const
  V11: TMyString =
  ('один', 'два', 'три', 'четыре', 'пять', 'шесть', 'семь', 'восемь', 'девять',
    'десять', 'одиннадцать',
    'двенадцать', 'тринадцать', 'четырнадцать', 'пятнадцать', 'шестнадцать',
      'семнадцать', 'восемнадцать', 'девятнадцать');
  V2: array[1..8] of string =
  ('двадцать', 'тридцать', 'сорок', 'пятьдесят', 'шестьдесят', 'семьдесят',
    'восемьдесят', 'девяносто');
  V3: array[1..9] of string =
  ('сто', 'двести', 'триста', 'четыреста', 'пятьсот', 'шестьсот', 'семьсот',
    'восемьсот', 'девятьсот');
  M1: array[1..13, 1..3] of string = (('тысяча', 'тысячи', 'тысяч'),
    ('миллион', 'миллиона', 'миллионов'),
    ('миллиард', 'миллиарда', 'миллиардов'),
    ('триллион', 'триллиона', 'триллионов'),
    ('квадриллион', 'квадриллиона', 'квадриллионов'),
    ('квинтиллион', 'квинтиллиона', 'квинтиллионов'),
    ('секстиллион', 'секстиллиона', 'секстиллионов'),
    ('сентиллион', 'сентиллиона', 'сентиллионов'),
    ('октиллион', 'октиллиона', 'октиллионов'),
    ('нониллион', 'нониллиона', 'нониллионов'),
    ('дециллион', 'дециллиона', 'дециллионов'),
    ('ундециллион', 'ундециллиона', 'ундециллионов'),
    ('додециллион', 'додециллиона', 'додециллионов'));
  R1: array[1..3] of string = ('рубль', 'рубля', 'рублей');
  R2: array[1..3] of string = ('копейка', 'копейки', 'копеек');
  function TroykaToStr(L: ShortInt; TR: TTroyka): string;
  var
    S: string;
  begin
    S := '';
    if Abs(L) = 1 then
    begin
      V1[1] := 'одна';
      V1[2] := 'две';
    end
    else
    begin
      V1[1] := 'один';
      V1[2] := 'два';
    end;
    if Troyka[2] = 1 then
    begin
      Troyka[2] := 0;
      Troyka[3] := 10 + Troyka[3];
    end;
    if Troyka[3] <> 0 then
      S := V1[Troyka[3]];
    if Troyka[2] <> 0 then
      S := V2[Troyka[2] - 1] + ' ' + S;
    if Troyka[1] <> 0 then
      S := V3[Troyka[1]] + ' ' + S;
    if (L > 0) and (S <> '') then
      case Troyka[3] of
        1: S := S + ' ' + M1[L, 1] + ' ';
        2..4: S := S + ' ' + M1[L, 2] + ' ';
      else
        S := S + ' ' + M1[L, 3] + ' ';
      end;
    TroykaToStr := S;
  end;
begin
  V1 := V11;
  L := 0;
  OutS := '';
  kk := Pos(',', DD);
  if kk = 0 then
    S := DD
  else
    S := Copy(DD, 1, kk - 1);
  if S = '0' then
    S2 := ''
  else
    S2 := S;
  repeat
    for k := 3 downto 1 do
      if Length(S) > 0 then
      begin
        Troyka[k] := StrToInt(S[Length(S)]);
        Delete(S, Length(S), 1);
      end
      else
        Troyka[k] := 0;
    OutS := TroykaToStr(L, Troyka) + OutS;
    if L = 0 then
      Mb := Troyka[3];
    Inc(L);
  until Length(S) = 0;
  case Mb of
    0: if Length(S2) > 0 then
        OutS := OutS + ' ' + R1[3] + ' ';
    1: OutS := OutS + ' ' + R1[1] + ' ';
    2..4: OutS := OutS + ' ' + R1[2] + ' ';
  else
    OutS := OutS + ' ' + R1[3] + ' ';
  end;
  S2 := '';
  if kk <> 0 then
  begin
    DD := Copy(DD, kk + 1, 2);
    if Length(DD) = 1 then
      DD := DD + '0';
    k := StrToInt(DD);
    Troyka[1] := 0;
    Troyka[2] := k div 10;
    Troyka[3] := k mod 10;
    S2 := TroykaToStr(-1, Troyka);
    case Troyka[3] of
      0: if Troyka[2] = 0 then
          S := ''
        else
          S := R2[3];
      1: S := R2[1];
      2..4: S := R2[2];
    else
      S := R2[3];
    end;
  end;
  // MoneyToStr:=OutS+IntToStr(k)+' '+S; // если копейки нужны цифрой-эту строку раскоментировать
  MoneyToStr := OutS + S2 + ' ' + S; // а эту закоментировать
end;
{$WARNINGS ON}

function MultipleConcat(AStr: string; ACount: Integer; ADivider: string = ''): string;
// Объединиение строки ACount раз с раделителем ADivider    <15.10.2009>
var
  iCounter: Integer;
begin
  Result := EmptyStr;
  if ACount = 0 then
    ACount := 1;
  for iCounter := 1 to ACount do
  begin
    Result := Concat(Result, AStr);
    if ADivider <> EmptyStr then
      if iCounter < ACount then
        Result := Concat(Result, ADivider);
  end;
end;

function GetFileDate(AFileName: string; ADefault: TDateTime = 0): TDateTime;
// Дата последнего изменения файла <19.10.2009> WP
var
  FHandle: Integer;
begin
  if ADefault = 0 then
    ADefault := Now;
  Result := ADefault;
  if not FileExists(AFileName) then
    Exit;
  FHandle := FileOpen(AFileName, 0);
  try
    if FHandle > -1 then
      Result := FileDateToDateTime(FileGetDate(FHandle))
    else
    // Result := FileDateToDateTime(FileAge(AFileName));  OLD!!!
    // <30.05.2012> WP Nazir
      Result := FileDateToDateTime(FileCreationDate(AFileName));
    // ^<30.05.2012> WP Nazir
  except
    Result := ADefault;
  end;
  FileClose(FHandle);
end;

function SetFileDate(const AFileName : string; const AFileDate : TDateTime): Boolean;
var
  FileHandle        : THandle;
  FileSetDateResult : Integer;
begin
  Result := False;
  if not FileExists(AFileName) then
    Exit;
  FileHandle := 0;
  try
    try
     FileHandle := FileOpen(AFileName, fmOpenWrite or fmShareDenyNone);
     if FileHandle > 0 then  begin
      FileSetDateResult :=
        FileSetDate(FileHandle, DateTimeToFileDate(AFileDate));
        Result := (FileSetDateResult = 0);
      end;
    except
     Result := False;
    end;
  finally
    FileClose (FileHandle);
  end;
end;

function InsSymToStr(AStr: string; ALength: Integer; ASymbol: string = '0';
  ATextDirectionLeft: Boolean = True): string;
// Insert Symbol To Str
var
  iCounter: Integer;
begin
  Result := AStr;
  ALength := ALength - Length(Result);
  for iCounter := 1 to ALength do
    if ATextDirectionLeft then
      Result := ASymbol + Result
    else
      Result := Result + ASymbol;
end;

function GetMinuteFromStr(AStr: string): Integer;
var
  sTemp: string;
//  eTemp: Extended;
  iTemp: Integer;
begin
  sTemp := AStr;
//  AStr := StringReplace(AStr, ':', '.', []);
  sTemp := StringReplace(sTemp, ' ', '0', [rfReplaceAll]);
  sTemp := InsSymToStr(sTemp, 5);
  //  4,17 ОШИБКА 4,18
  //Insert(DecimalSeparator, sTemp, 4);
  //eTemp := StrToFloatDef(sTemp, 0.0);
  iTemp := StrToIntDef(sTemp, 0);
  Result := (iTemp div 100) * 60;
  Result := Result + iTemp mod 100;
 // Result := Trunc(eTemp) * 60;
 // Result := Result + Trunc(eTemp * 100) mod 100;

  //MessageBox(0, PChar(IntToStr(Result)), '', 0);
end;

function GetStrFromMinute(AMinute: Integer): string;
begin
  if AMinute = 0 then
    Result := ' 0000'
  else
    Result := InsSymToStr(IntToStr(AMinute div 60), 3, ' ') + InsSymToStr(IntToStr(AMinute mod 60), 2);
end;

function MinutsToTime(AMinutes: Integer; ASeconds: Boolean = True): string;
// <08.12.2009> WP Nazir
var
  iTimeHour, iTimeMinute: Integer;
begin
//  iTimeHour := 0;
//  iTimeMinute := 0;
  iTimeHour := AMinutes div 60;
  iTimeMinute := AMinutes - iTimeHour * 60;
  Result := '';
  if (iTimeHour < 10) then
    Result := Result + '0';
  Result := Result + IntToStr(Trunc(Abs(iTimeHour)));
  Result := Result + ':';
  if (iTimeMinute < 10) then
    Result := Result + '0';
  Result := Result + IntToStr(Trunc(Abs(iTimeMinute)));
  if (ASeconds) then
    Result := Result + ':00';
end;

function DirSize(Dir: string): Integer;
// Как подсчитать занимаемое директорией место
var
  SearchRec: TSearchRec;
  Separator: string;
  DirBytes: integer;
begin
  if Copy(Dir, Length(Dir), 1) = '\' then Separator := ''
  else Separator := '\';
  if FindFirst(Dir + Separator + '*.*', faAnyFile, SearchRec) = 0 then begin
    if FileExists(Dir + Separator + SearchRec.Name) then begin
      DirBytes := DirBytes + SearchRec.Size;
      {Memo1.Lines.Add(Dir+Separator+SearchRec.Name);}
    end
    else
      if DirectoryExists(Dir + Separator + SearchRec.Name) then begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          DirSize(Dir + Separator + SearchRec.Name);
      end;
    while FindNext(SearchRec) = 0 do begin
      if FileExists(Dir + Separator + SearchRec.Name) then begin
        DirBytes := DirBytes + SearchRec.Size;
        {Memo1.Lines.Add(Dir+Separator+SearchRec.Name);}
      end
      else
        if DirectoryExists(Dir + Separator + SearchRec.Name) then begin
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then begin
            DirSize(Dir + Separator + SearchRec.Name);
          end;
        end;
    end;
  end;
  FindClose(SearchRec);
end;


function GetEnvironmentVariableString(AName: string): string;
// Переменные среды
var
  cResult: Cardinal;
begin
  // Например AName = 'USERPROFILE'
  Result := EmptyStr;
  if AName = EmptyStr then
    Exit;
  AName := Trim(UpperCase(StringReplace(AName, '%', '', [rfReplaceAll, rfIgnoreCase])));
  if AName = EmptyStr then
    Exit;
  try
    {$if defined(Windows)}
    cResult := GetEnvironmentVariableA(PChar(AName), nil, 0);
    if cResult > 0 then
    begin
      SetLength(Result, cResult - 1);
      GetEnvironmentVariableA(PChar(AName), PChar(Result), cResult);
    end;
    {$else}
    Result := EmptyStr;
    {$endif}
  except
    Result := EmptyStr;
  end;
end;

procedure FindFiles(AStartFolder, AMask: string; AList: TStringList;
  AScanSubFolders: Boolean = True);
{
Зависимости: Windows, SysUtils, Classes, Masks
Автор:       Dimka Maslov, mainbox@endimus.ru, ICQ:148442121, Санкт-Петербург
Copyright:   Dimka Maslov
Дата:        29 апреля 2002 г.
}
var
  SearchRec: TSearchRec;
  FindResult: Integer;
begin
  if AStartFolder[Length(AStartFolder)] <> '\' then
      AStartFolder := AStartFolder + '\';
  if AMask = '' then
    AMask := '*.*';

  AList.BeginUpdate;
  try
    AStartFolder := IncludeTrailingBackslash(AStartFolder);
    FindResult := FindFirst(AStartFolder + '*.*', faAnyFile, SearchRec);
    try
      while FindResult = 0 do
        with SearchRec do
        begin
          if (Attr and faDirectory) <> 0 then
          begin
            if AScanSubFolders and (Name <> '.') and (Name <> '..') then
              FindFiles(AStartFolder + Name, AMask, AList, AScanSubFolders);
          end
          else
          begin
            if MatchesMask(Name, AMask) then
              AList.Add(AStartFolder + Name);
          end;
          FindResult := FindNext(SearchRec);
        end;
    finally
      FindClose(SearchRec);
    end;
  finally
    AList.EndUpdate;
  end;
end;

function GetQuarter(AMonth: Integer): Integer;
// Получение квартала по месяцу
begin
  case AMonth of
    1..3: Result := 1;
    4..6: Result := 2;
    7..9: Result := 3;
    10..12: Result := 4;
  else
    Result := 1
  end;
end;

function arCyr2LatUp(AStr: string; ASpaceCharacter: string = ' ';
  APointCharacter: string = '.'): string;
//Переводит строку на заглавные буквы, а далее с кирилицы на латиницу
// http://www.sql.ru/forum/actualutils.aspx?action=gotomsg&tid=749043&msg=8594543
const
  Rus= 'АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ';
  Lat= 'ABVGDEYZZIYKLMNOPRSTUFKTCSS Y EYY';
  Lat2='      OH              HSHHH    UA';
  Lat3='                          C      ';
  Lat4='                          H      ';
var
  i, j: Integer;
  Res: string;
begin
  AStr := AnsiUpperCase(AStr);
  Res := '';
  for i:=1 to Length(AStr) do
  begin
    j := Pos(AStr[i], Rus);
    if j > 0 then
      Res := Res + Trim(Lat[j] + Lat2[j] + Lat3[j] + Lat4[j])
    else
      Res := Res + AStr[i]
  end;
  Result := StringReplace(Res, ' ', ASpaceCharacter, [rfReplaceAll]);
  Result := StringReplace(Result, '.', APointCharacter, [rfReplaceAll]);
end;

function FileCreationDate(const FileName: string): Integer;
// Дата создания файла
var
  Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
begin
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      FileTimeToLocalFileTime(FindData.ftCreationTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
        LongRec(Result).Lo) then Exit;
    end;
  end;
  Result := -1;
end;

function FileLastWriteDate(const FileName: string): Integer;
// Дата последнего изменения файла
// <18.07.2012> WP Nazir
var
  Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
begin
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
        LongRec(Result).Lo) then Exit;
    end;
  end;
  Result := -1;
end;

function FileLastAccessDate(const FileName: string): Integer;
// Дата последнего открытия файла
// <18.07.2012> WP Nazir
var
  Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
begin
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      FileTimeToLocalFileTime(FindData.ftLastAccessTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
        LongRec(Result).Lo) then Exit;
    end;
  end;
  Result := -1;
end;

function GetSpecialPath(CSIDL: word): string;
// Специальные (стандартные) папки ОС
var s:  string;
//<04.12.2012> WP Nazir
// uses ShlObj;
// Snowy   http://www.delphilab.ru/content/view/160/85/
begin
  SetLength(s, MAX_PATH);
  if not SHGetSpecialFolderPath(0, PChar(s), CSIDL, true)
  then s := GetSpecialPath(CSIDL_APPDATA);
  result := PChar(s);
end;

procedure WritelnOEM(AText: string);
var
  NewStr: string;
begin
  SetLength(NewStr, Length(AText));
  if Trim(AText) <> '' then
    CharToOem(PChar(AText), PChar(NewStr));
  Writeln(NewStr);
end;

function SysFolder(Param: string): string;
var
  reg: TRegistry;
begin
  reg:=TRegistry.Create;
  reg.RootKey := HKEY_CURRENT_USER;
  reg.OpenKey('SoftWare\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', false);
  Result := reg.ReadString(Param);
  reg.Free;
end;

procedure CreateLink(const PathObj, WorkDir, PathLink, Description, Arguments: string;
                     ShowCmd: Integer; AIconPath: string = ''; AIconIndex: Integer = 0);
var
  NewLink: IShellLink;
//  obj: IUnknown; ShellFile: IPersistFile;
begin
  NewLink := CreateComObject(CLSID_ShellLink) as IShellLink;
  with NewLink do
  begin
    SetPath(PChar(PathObj));
    SetWorkingDirectory(PChar(WorkDir));
    SetDescription(PChar(Description));
    SetArguments(PChar(Arguments));
    SetShowCmd(ShowCmd);
    if FileExists(AIconPath) then
      SetIconLocation(PChar(Trim(AIconPath)), AIconIndex);
  end;
  (NewLink as IPersistFile).Save(PWChar(WideString(PathLink)), false);

//  ShellFile := obj as IPersistFile;
//  ShellFile.Save(PWChar(WideString(PathLink)), false);
end;

procedure CreateShortcut(ADescription: string = ''; APathLink: string = '';
                         AArguments: string = '';
                         AIconPath: string = ''; AIconIndex: Integer = 0);
var
  PathObj, WorkDir: string;
  ShowCmd: Integer;
begin
  GetDir(0, PathObj);
  PathObj := PathObj + '\' + ExtractFileName(Application.ExeName);
  PathObj := Trim(PathObj);
  if PathObj = '' then
    PathObj := ParamStr(0);
  WorkDir := ExtractFilePath(PathObj);
  if APathLink = '' then
    APathLink := SysFolder('Desktop');
  if (APathLink <> '') and (APathLink[Length(APathLink)] <> '\') then
    APathLink := APathLink + '\';
  if ADescription = '' then
  begin
    APathLink := APathLink + ExtractFileName(PathObj);
    ADescription := 'Ярлык ' + ExtractFileName(PathObj);
  end
  else
    APathLink := APathLink + ADescription;
  APathLink := APathLink + '.lnk';
  CreateLink(PathObj, WorkDir, APathLink, ADescription, AArguments, ShowCmd, AIconPath, AIconIndex);
end;

function BrowseDialogCallBack (Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
var
  wa, rect : TRect;
  dialogPT : TPoint;
begin
  //center in work area
  if uMsg = BFFM_INITIALIZED then
  begin
    wa := Screen.WorkAreaRect;
    GetWindowRect(Wnd, Rect);
    dialogPT.X := ((wa.Right-wa.Left) div 2) -
                  ((rect.Right-rect.Left) div 2);
    dialogPT.Y := ((wa.Bottom-wa.Top) div 2) -
                  ((rect.Bottom-rect.Top) div 2);
    MoveWindow(Wnd,
               dialogPT.X,
               dialogPT.Y,
               Rect.Right - Rect.Left,
               Rect.Bottom - Rect.Top,
               True);
  end;

  Result := 0;
end; (*BrowseDialogCallBack*)

function BrowseDialog(const Title: string; const Flag: Integer; AHandle: THandle; AInitFolder: string = ''): string;
// Диалог выбора папки
var
  lpItemID : PItemIDList;
  BrowseInfo : TBrowseInfo;
  DisplayName : array[0..MAX_PATH] of char;
  TempPath : array[0..MAX_PATH] of char;
begin
  Result := '';
  FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
  with BrowseInfo do
  begin
    //hwndOwner := Application.Handle
    hwndOwner := AHandle;
    pszDisplayName := @DisplayName;
    lpszTitle := PChar(Title);
    ulFlags := Flag;
//    lpfn := BrowseDialogCallBack;
  end;
  lpItemID := SHBrowseForFolder(BrowseInfo);
  if lpItemId <> nil then begin
    SHGetPathFromIDList(lpItemID, TempPath);
    Result := TempPath;
    GlobalFreePtr(lpItemID);
  end;

  {Sample usage:
  procedure TForm1.Button1Click(Sender: TObject);
  var
    sFolder : string;
  begin
    sFolder := BrowseDialog('Select a folder',
                            BIF_RETURNONLYFSDIRS);

    if sFolder <> '' then
      ShowMessage('Selected: ' + #13#10 + sFolder);
  end; //}
 // if SelectDirectory('My message','d:\!gajba',sFolder) then !!!!!!!!!!!!!
end;

function BrowseDialogExtra(AHandle: THandle; const ATitle: string; AInitFolder: string = ''): string;
begin
//  Result := EmptyStr;
  Result := BrowseDialog(ATitle, BIF_RETURNONLYFSDIRS, AHandle, AInitFolder);
end;

end.

