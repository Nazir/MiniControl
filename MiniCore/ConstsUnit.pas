unit ConstsUnit;

{$mode objfpc}{$H+}

{******************************************************************************}
{                                                                              }
{ Unit: Projects Group Global Constants Module                                 }
{                                                                              }
{  Copyright: Nazir © 2002-2019                                                }
{  Development: Nazir K. Khusnutdinov (aka Naziron or Wild Pointer)            }
{  Разработчик: Хуснутдинов Назир Каримович                                    }
{  Email: naziron@gmail.com                                                    }
{  Git: https://github.com/Nazir                                               }
{                                                                              }
{  Modified: 09.12.2006                                                        }
{  Modified: 09.05.2009, 17.05.2009, 07.09.2009, 06.10.2009                    }
{  Modified: 04.04.2010                                                        }
{  Modified: 15.05.2019                                                        }
{                                                                              }
{******************************************************************************}


interface

{$I 'Defines.inc'}

uses
  {$if defined(Windows)}windows,{$endif}
  LCLIntf, LCLType, LMessages, Messages, Graphics, DateUtils, SysUtils, Classes,
  IniFiles, ClientDiff;

type
  // тип данных UpdateTransaction
  TUpdateTransaction = (utCommit, utRollback, utStart);

  TCopyDataStruct = packed record
    dwData: DWORD;   // anwendungsspezifischer Wert
    cbData: DWORD;   // Byte-Lдnge der zu ьbertragenden Daten
    lpData: Pointer; // Adresse der Daten
  end;

  TServiceMsgProtocol = record
    MsgType: string;
    MsgText: string;
  end;

var
  AppTitle: string;
  AppName: string;
  AppCaption: string;
  bMainFormActivate: Boolean = False;

  ServiceMsg: TServiceMsgProtocol;
  // For Plugins
  Plugin_ID: Integer;

  // Rights
  CurrentCompanyID: Integer = 1;
  CurrentUserName: string;
  CurrentUserNameDBMS: string;
  CurrentUserNameDBMSpw: string;
  CurrentUserID: Integer;
  CurrentSubdivisionID: Integer;
  CurrentSubdivisionCaption: string;
  CurrentUserRole: string;
  CurrentUserRoleCaption: string = 'Нет'; //'Пользователь';
  AvailableRoles: TStringList;
  bRoleAdmin: Boolean = False;
  bRoleDispatcher: Boolean = False;
  bRoleSupervisor: Boolean = False;
  //ChangeUserPassword: Boolean;

  // DB
  DBVersion: Integer; // Версия текущей структуры БД. Используется для проверки приложением.
  ConnectedDB: Boolean = False;
  CurrentDBConfig: string;
  CurrentDBName: string;
  CurrentServerHostName: string;
  CurrentServerPort: Integer;
  CurrentDBCharset: string;
  CurrentSQLName: string;
  CurrentSQL: string;
  // Errors
  LastErrorCode: Integer;
  LastErrorTime: TDateTime;


  // Pathes
  PathApp: string;
  PathHelp: string;
  PathTemplates: string;
  PathReports: string;
  PathSys: string;
  PathImages: string;
  PathMRU: string;
  PathSQL: string;
  // PathTemp: string;  смотри function PathTemp: string;
  // PathLog: string;   смотри function PathLog: string;

  ifConfig: TIniFile;
  ifDB: TIniFile;
  
  // GLOBAL VARs <13.10.2009>
  CurrentPeriodInfo: string; // Для периода дат
  CurrentDateBegin, CurrentDateEnd: TDateTime; // Для периода дат
  CurrentTimeBegin, CurrentTimeEnd: TDateTime; // Для периода времени
  CurrentTimestampBegin, CurrentTimestampEnd: TDateTime; // Для периода Timestamp
  CurrentWorkYear, CurrentWorkMonth, CurrentWorkDay: Integer; // Для рабочего периода

  // Основные цвета
  clMain: TColor;
  clMainBack: TColor;
  clMainFore: TColor;
  clMainFont: TColor;
  
const
{$IFDEF MSWINDOWS}
  CRLF = #13 + #10;
{$ENDIF}
{$IFDEF LINUX}
  CRLF = #10;
{$ENDIF}
  CR   = #13;
  LF   = #10;
  TAB  = #9;
  NULL_TERMINATOR = #0;
  // Boolean
  bTrue = 1;
  bFalse = 0;
  //
  GlobalProductID = '0004-ERP1-ERPS-2009-0603';
  GlobalClientID = ClientDiff.SClientID;
  GlobalDemoEndDate = '01.08.2012';
  {$IFDEF NKES}
  GlobalDeveloperEmailAddress = 'KhusnutdinovNK@gridcom-rt.ru';
  GlobalDeveloperSiteAddress = 'www.gridcom-rt.ru';
  {$ELSE}
  GlobalDeveloperEmailAddress = 'support@ovvio.pro';
  GlobalDeveloperSiteAddress = 'www.Ovvio.pro';
  {$ENDIF}
  //GlobalProgramYear = '2010';


  {$IFDEF LANG_RU_RU}
//  GlobalVersionPostfix = '';
//  GlobalVersionPostfix = ' РК'; // Релиз-кандидат
  GlobalVersionPostfix = ' бета';
//  SVersionPostfix = ' альфа';
  {$ELSE}
//  GlobalVersionPostfix = '';
//  GlobalVersionPostfix = ' RC';
  GlobalVersionPostfix = ' beta';
//  GlobalVersionPostfix = ' alpha';
  {$ENDIF}


  {$IFDEF LANG_RU_RU}
  // MenuItem.Caption
  GlobalSessionCaption = '&Сеанс';
 // SOrderCaption = '&Заявка';
  GlobalParameterCaption = '&Параметры';
  GlobalPluginsCaption = 'П&лагины';
  GlobalHelpCaption = 'П&омощь';
  {$ELSE}
  // MenuItem.Caption
  GlobalSessionCaption = '&Session';
 // SOrderCaption = '&Order';
  GlobalParameterCaption = '&Parameters';
  GlobalPluginsCaption = 'P&lugins';
  GlobalHelpCaption = '&Help';
  {$ENDIF}

  //
  GlobalConfigFileExt = '.ini';
  GlobalColumnsLayoutIni = 'ColumnsLayout' + GlobalConfigFileExt;
  GlobalDBIniFile = 'DB' + GlobalConfigFileExt;

// Реестр
//  GlobalRegistryKey = 'Software\Ovvio Systems\MiniSystem';

// Цвета 
  clNavyBlue = $00775649;   //73 86 119 (Navy Blue - CMYK)
  clOrange = $001778E7;   //231 120 23 (Orange - CMYK)

  // Цвета таблицы
  clGrid = clWindow;
  clLightLineBack = $00E5FFE5; //$00C0DCC0; // $00E5FFE5; //$00EACAB6;
  clLightLineBackBright = $00FDF1EA;
  clLightLineFont = clBlack;
  clDeletedBack = $00CACAFF; //clYellow;
  clDeletedFont = clRed;
  clObligatoryFont = clRed; // обязательный
  clObligatoryBack = clYellow; //$00CACAFF;
  //

// Com константы
  GlobalDllIcon = 'COMICON';

  // Extensions
  ExtMRU = '.mru';

// Сообщения
  MSG_Information         = 'Информация';
  MSG_Warning             = 'Предупреждение';
  MSG_WarningConfirm      = 'Предупреждение: %s';
  MSG_WarningRecordExists = 'Предупреждение: Запись существует.';
  MSG_RecordDelete        = 'Удалить запись?';
  MSG_WarningEmptyField   = 'Предупреждение: Не заполнено поле.';
  MSG_WarningAlreadyExist = '%s уже существует!';
  MSG_WarningEmptyExists  = 'Остались незаполненные поля.';
  MSG_Confirmation        = 'Подтверждение';
  MSG_Error               = 'Ошибка';
  MSG_FillFieldNotZero    = 'Значение должно быть больше нуля: "%s"!';
  MSG_FillField           = 'Заполните поле: "%s"!';
  MSG_ErrorFillField      = 'Некорректное заполнение поля: "%s"!';
  MSG_NeedToFill          = 'Небходимо заполнить поле %s';
  MSG_NeedToChoose        = 'Небходимо выбрать %s';
  MSG_NeedAdminRights     = 'Необходимы права администратора!';
  MSG_IndexOutOfRange     = 'Индекс вне диапазона!';
  MSG_WrongFormatString   = 'Неверный формат в строке %d. ';
  MSG_InternalError       = 'Внутренняя ошибка. Обратитесь к разработчикам для решения этой проблемы';
  MSG_NoAccessToRegistry  = 'Невозможно получить доступ к реестру...';
  MSG_SaveOverpatching    = 'Сохранить изменения?';
  MSG_DataSaved           = 'Данные сохранены!';
  MSG_DataNotSaved        = 'Данные не сохранены!';
  MSG_OnlyAdminFunc       = 'Эта функция доступна только Администратору.';
  MSG_CurrentPeriod       = 'Текущий период';

// Иконка сообщения + кнопки
  MB_OK_INFO = MB_OK or MB_ICONINFORMATION;
  MB_OK_EXCL = MB_OK or MB_ICONEXCLAMATION;
  MB_OK_WARN = MB_OK or MB_ICONWARNING;
  MB_YESNO_QUEST = MB_YESNO or MB_ICONQUESTION;
  MB_YESNOCANCEL_QUEST = MB_YESNOCANCEL or MB_ICONQUESTION;

// Пользовательские сообщения
  WM_NAZIR_UPDATEBASE = WM_USER + 1;
  WM_NAZIR_PROGRESS   = WM_USER + 2;
  WM_NAZIR_SERVICE    = WM_USER + 3;

//
function AppExeName: string;
function PathTemp: string;
function PathLog: string;
// System Pathes
function PathOSWindows: string;
function PathOSTempUser: string;
function PathOSAppData: string;
function PathSysOS: string;

// ----------------------------------------------------------
function SOranizationEssentialElements: string;
// ----------------------------------------------------------

// Эбаут
function GetShortProjectName: string;
function GetFullProjectName: string;
function GetProductName: string;
function GetProductVersion: string;
function GetBuildVersion: string;
function GetBuildDate: string;
function GetFileVersion: string;
function GetFullFileVersion: string; // Полная версия
function GetFullProductVersion: string; // Полная версия
function GetAuthor(AName: string): string;
function GetLegalCopyright: string;

procedure SaveLogToFile(ALogName, ALogText, ALogStatus: string);
procedure SendCopyData(AhWind: HWND; AMessID: Integer; AMess: string);
procedure SendMessToService(AName, AMess: string);
procedure SendMessToForm(AFormName, AName, AMess: string);

implementation

uses
  UtilsUnit, StrUtils;

function GetShortProjectName: string;
// Получение полного названия проекта
begin
  Result := EmptyStr;
   //Result := viAppInfo.FileDescription;
end;

function GetFullProjectName: string;
// Получение полного названия проекта
begin
  Result := EmptyStr;
  //with viAppInfo do
  //  Result := Format('%s™ %s "%s"', [ProductName, ProductVersion, FileDescription]);
end;

function GetProductName: string;
begin
  Result := EmptyStr;
  //Result := viAppInfo.ProductName;
end;

function GetProductVersion: string;
begin
  Result := EmptyStr;
//  //'%d.%d.%d'
//  Result := viAppInfo.ProductVersion + GlobalVersionPostfix;
////  viAppInfo.CompleteVersionInformation;
end;

function GetBuildVersion: string;
// Версия билда
begin
  Result := EmptyStr;
  //Result := viAppInfo.FileVersionBuild;
  {$IFDEF LANG_RU_RU}
  Result := 'Cборка: ' + Result;
  {$ELSE}
  Result := 'Build: ' + Result;
  {$ENDIF}
end;

function GetBuildDate: string;
// Дата билда
begin
// Result := DateToStr(FileDateToDateTime(FileAge(ParamStr(0))));  OLD!!!
// <30.05.2012> WP Nazir
//  Result := DateToStr(FileDateToDateTime(FileCreationDate(ParamStr(0))));
// ^<30.05.2012> WP Nazir
// <18.07.2012> WP Nazir
//  Result := DateToStr(GetFileDate(ParamStr(0)));
  Result := DateToStr(FileDateToDateTime(FileLastWriteDate(ParamStr(0))));
// ^<18.07.2012> WP Nazir
end;

function GetFileVersion: string;
begin
  Result := EmptyStr;
  //Result := Trim(viAppInfo.FileVersion);
end;

function GetFullFileVersion: string;
// Полная версия
begin
  Result := EmptyStr;
  //<04.04.2010> WP Nazir
  //Result :=  viAppInfo.FileVersionWithoutBuild + ' (' + GetBuildVersion + ' от ' + GetBuildDate + ')';
  {$IFDEF LANG_RU_RU}
  Result := 'Версия: ' + Result;
  {$ELSE}
  Result := 'Version: ' + Result;
  {$ENDIF}
end;

function GetFullProductVersion: string;
// Полная версия
begin
  Result := EmptyStr;
  //<04.04.2010> WP Nazir
  //Result :=  viAppInfo.ProductVersion;
  {$IFDEF LANG_RU_RU}
  Result := 'Версия: ' + Result;
  {$ELSE}
  Result := 'Version: ' + Result;
  {$ENDIF}
end;

function GetAuthor(AName: string): string;
begin
  if AName = 'Nazir' then
  // 'Хуснутдинов Назир Каримович'
  begin
    Result := DeCoderString(#149#135#109#160#135#44#10#114#79#235#68#113#187#70#231#151#70#210#70#16#112#59#72#157#190#170#65, iKey_string);
    Result := Result + ' (Wild Pointer)'
  end;
  if AName = 'Alex' then
  // 'Карпов Алексей Николаевич'
  begin
    Result := 'Карпов А.Н.';
    Result := Result + ''
  end;
end;

function GetLegalCopyright: string;
// Полная версия
begin
  Result := EmptyStr;
  //Result :=  viAppInfo.LegalCopyright;
end;

function AppExeName: string;
begin
  //Result := Application.ExeName;
  Result := ExtractFileName(ParamStr(0));
end;

function PathUserConfig: string;
begin
  //<04.12.2012> WP Nazir
  Result := PathOSAppData + 'Ovvio Systems\MiniSystem\';
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function PathTemp: string;
begin
  Result := PathOSTempUser + 'Ovvio Systems\MiniSystem';
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function PathLog: string;
begin
  Result := PathSysOS + 'logs\';
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

// System Pathes
function PathOSWindows: string;
var
  arrWinDir: array[0..MAX_PATH] of Char;
begin
  GetWindowsDirectory(arrWinDir, SizeOf(arrWinDir));
  Result := arrWinDir;
  Result := Result + '\';
end;

function PathOSTempUser: string;
var
  arrTempPath: array[0..MAX_PATH] of Char;
begin
  GetTempPath(SizeOf(arrTempPath), arrTempPath);
  Result := arrTempPath;
end;

function PathOSAppData: string;
begin
  //<04.12.2012> WP Nazir
  Result := GetSpecialPath($1c);
  Result := Result + '\';
end;

function PathSysOS: string;
begin
  //<06.12.2012> WP Nazir
  PathSysOS := PathUserConfig + 'system\';
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;


function SOranizationEssentialElements: string;
begin
  Result := ClientDiff.SOranizationEssentialElements;
end;

procedure SaveLogToFile(ALogName, ALogText, ALogStatus: string);
// Запись журналов регистраций
const
  iMaxFileSize = 10485760; // 10 Mb
var
  sLogTime: string;
  sFileName: string;
  sLine: string;
begin
  sLogTime := DateToStr(Date) + ' ' + TimeToStr(Time) + ' ';
  if MonthOf(Now) < 10 then
    sFileName := ALogName + '_' + IntToStr(YearOf(Now)) + '0' + IntToStr(MonthOf(Now))
  else
    sFileName := ALogName + '_' + IntToStr(YearOf(Now)) + '' + IntToStr(MonthOf(Now));

  sLine := sLogTime + ALogName + ' ' + ALogStatus + ' ' + AppExeName + ' ' + ALogText;

  if not DirectoryExists(PathSysOS + 'logs') then
    ForceDirectories(PathSysOS + 'logs');

  sFileName := PathLog + sFileName + '.log';
  SaveTextToFile(sFileName, sLine, 2, iMaxFileSize);

  // Полный лог событий
  sFileName := PathLog + 'Full.log';
  SaveTextToFile(sFileName, sLine, 2, iMaxFileSize);

end;

procedure SendCopyData(AhWind: HWND; AMessID: Integer; AMess: string);
var
  cdsMess: TCopyDataStruct;
begin
  if AhWind <> 0 then
  begin
    with cdsMess do
    begin
      dwData := 0; // may use a value do identify content of message
      cbData := StrLen(PChar(AMess)) + 1;  //Need to transfer terminating #0 as well
      lpData := PChar(AMess)
    end;
    SendMessage(AhWind, WM_COPYDATA, 0, Longint(@cdsMess));
  end;
end;

procedure SendMessToService(AName, AMess: string);
  {$IFDEF SERVICE}
var
  handWnd: HWND;
  {$ELSE}
  {$ENDIF}
begin
  {$IFDEF SERVICE}
  handWnd := FindWindow(PChar('TServiceForm'),
                        PChar('Nazir Service Control'));
  SendCopyData(handWnd, 0, AName + '|' + AMess)
  {$ELSE}
  {$ENDIF}
end;

procedure SendMessToForm(AFormName, AName, AMess: string);
var
  handWnd: HWND;
begin
  handWnd := FindWindow(PChar(AFormName), nil);
  SendCopyData(handWnd, 0, AName + '|' + AMess)
end;

initialization

// =================== Инициализация глобальных переменных  ====================
  PathApp := ExtractFilePath(ParamStr(0));  // не убирать!!!!
  PathHelp := PathApp + 'Help\';
  PathSys := PathApp + 'system\';
  PathImages := PathSys + 'images\';
  PathMRU := PathSysOS + 'MRU\';
  // Путь к паке с отчетами
  PathReports := PathUserConfig + 'Reports\';
  PathTemplates := PathApp + 'Templates\';
  PathSQL := PathApp + 'SQL\';

  SendMessToService('MessageFormClose', StringReplace(AppExeName,
                    ExtractFileExt(AppExeName), EmptyStr, [rfIgnoreCase,rfReplaceAll]));

  if not FileExists(PathSysOS + AppExeName + GlobalConfigFileExt) then
    CopyFile(PChar(PathSys + AppExeName + GlobalConfigFileExt), PChar(PathSysOS + AppExeName + GlobalConfigFileExt), True);
  ifConfig := TIniFile.Create(PathSysOS + AppExeName + GlobalConfigFileExt); // 16.09.2009
  ifDB := TIniFile.Create(PathSys + GlobalDBIniFile);

  //viAppInfo := TostVersionInfo.Create(nil);

  CurrentDateBegin := Date;
  CurrentTimeBegin := StrToTime('00:00:00');
  CurrentTimestampBegin := StrToDateTimeDef(DateToStr(CurrentDateBegin) + ' ' + TimeToStr(CurrentTimeBegin), Now);
  CurrentDateEnd := Date;
  CurrentTimeEnd := StrToTime('23:59:59');
  CurrentTimestampEnd := StrToDateTimeDef(DateToStr(CurrentDateEnd) + ' ' + TimeToStr(CurrentTimeEnd), Now);
  CurrentWorkYear := YearOf(CurrentDateEnd);
  CurrentWorkMonth := MonthOf(CurrentDateEnd);
  {$IFDEF DEBUG}
  MessageBox(0, 'ВНИМАНИЕ!!! ' + CRLF +'Отладочная сборка!' + CRLF +'Только для разработчиков!', MSG_Warning, MB_OK_EXCL);

  {$ENDIF}
 // MessageBox(0, PChar(GetStrUserName), MSG_Warning, MB_OK_EXCL);
//  MessageBox(0, PChar(PathUserConfig), MSG_Warning, MB_OK_EXCL);

  // Основные цвета по умолчанию
  clMain := clWhite;
  clMainBack := clWhite;
  clMainFore := clBlack;
  clMainFont := clBlack;

  AvailableRoles := TStringList.Create;

finalization

  ifConfig.Free;
  ifDB.Free;

  //viAppInfo.Free;

  AvailableRoles.Free;

end.

