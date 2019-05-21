unit DBUnit;

{$mode objfpc}{$H+}

{******************************************************************************}
{                                                                              }
{  Unit: Database control module                                               }
{                                                                              }
{  Copyright: Nazir © 2002-2019                                                }
{  Development: Nazir K. Khusnutdinov (aka Naziron or Wild Pointer)            }
{  Разработчик: Хуснутдинов Назир Каримович                                    }
{  Email: naziron@gmail.com                                                    }
{  Git: https://github.com/Nazir                                               }
{                                                                              }
{  Modified: 14.01.2007, 29.01.2008, 09.05.2009, 06.07.2009                    }
{  Modified: 31.07.2009, 04.08.2009, 11.08.2009, 14.08.2009                    }
{  Modified: 14.09.2009, 23.09.2009, 06.10.2009, 14.10.2009                    }
{  Modified: 24.11.2009                                                        }
{  Modified: 01.04.2010, 18.05.2010, 13.10.2010                                }
{  Modified: 01.03.2011, 25.07.2011, 19.12.2011                                }
{  Created: 15.05.2019 (Lazarus)                                               }
{  Modified: 16.05.2019                                                        }
{                                                                              }
{******************************************************************************}

interface

uses
  LCLType, Controls, Classes, SysUtils, Forms, Variants,
  pqconnection,
  db, sqldb,
  DBCtrls, DBGrids;

type

  { TConnectingThread }

  TConnectingThread = class(TThread)
  private
  protected
    procedure Execute; override;
    function Connect: Boolean; virtual; abstract;
  public
    constructor Create();
  end;

  { TConnectThread }

  TConnectThread = class(TConnectingThread)
  protected
    function Connect: Boolean; override;
  end;

  { TdmDB }

  TdmDB = class(TDataModule)
    DataSetConditions: TSQLQuery;
    dsMain: TDataSource;
    DatabaseMain: TPQConnection;
    DataSetTemp: TSQLQuery;
    DataSetApp: TSQLQuery;
    DataSetUsers: TSQLQuery;
    DataSetMain: TSQLQuery;
    DataSetParams: TSQLQuery;
    DataSetSQL: TSQLQuery;
    TransactionExec: TSQLTransaction;
    TransactionMain: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    //SplashCopyBase: TSplashForm;
    procedure ConnectingThreadDone(Sender: TObject);
    procedure RegisterAllClasses;
    procedure UnRegisterAllClasses;

  public
    Connecting: Boolean;

    //
    SQL_Main: string;
    SQL_Extra: string;
    SQL_Temp: string;
    SQL_Edit: string;
    SQL_View: string;
    SQL_DragDrop: string;

    MaxColumnWidth, MinColumnWidth, DefaultColumnWidth: Integer;

    // Подлючение к серверу базы
    procedure LoadDBSettings;
    function Connect(AReconnect: Boolean = False): Boolean;
    function ConnectToBase(ADatabase: TPQConnection): Boolean;

    // Отключение от сервера базы
    function Disconnect: Boolean;
    function DisconnectBase: Boolean;

    function CheckConnected: Boolean;

    //
    function LoadSQLToQuery(ASQLText: string; var ADataSet: TSQLQuery;
      AConditions: string = ''; AGroupByClause: string = ''; AOrderClause: string = ''): string;

    //
    function LoadSQLToQueryByName(ASQLName: string; var ADataSet: TSQLQuery;
      AConditions: string = ''; AGroupByClause: string = ''; AOrderClause: string = ''): string;

    function _LoadSQLToQueryByName(ASQLName: string; var ADataSet: TSQLQuery;
      AConditions: string = ''; AGroupByClause: string = ''; AOrderClause: string = ''): string;
    // Получение значения поля AFieldName из таблицы ATableName по условию ACondition
    function GetFieldValueFromTable(AFieldName, ATableName, ACondition: string): string;

    // Получение значения поля с именем AFieldName из SYS_SQL
    function GetFieldValueFromSQLs(ASQLName, AFieldName: string): string;

    // Получение запроса на выборку из таблицы SQls в TIBDataSet
    function GetSQL(ASQLName: string; ASQLType: Integer = 1): string;

    // Получение запроса на выборку из таблицы SQls в TIBUpdateSQL
    function SetSQLs(ASQLName: string; var ADataSet: TSQLQuery): string;

    // Загрузка ширин столбцов
    //procedure LoadColumnsWidths(AGridName: string; AColumns: TDBGridColumnsEh);
    // Сохранение ширин столбцов
    procedure SaveColumnsWidths(AGridName: string; AColumns: TDBGridColumns);

    // Установка настроек по умолчанию для DBGrid
    procedure SetDBGridSettings(ASQLName: string; var ADBGrid: TDBGrid;
      ADataSource: TDataSource; AOnlySave: Boolean = False; AReadOnly: Boolean = True);

    // Получение имение поля
    function GetFieldName(AFieldName: string): string;

    // Получение имени таблицы поля
    function GetTableName(AFieldName: string): string;

    // Получение описания поля
    function GetFieldsDescriptions(AFieldName, ATable: string): string;

    // Получение строки с данными текущей записи, исключая BLOB поля
    function GetRecordData(ADataSet: TDataSet; ADivider: string = '|'): string;

    //
    procedure SetDBName(const Value: string);

    // Получение кода пользователя по хэндлу приложения (или др.)
    procedure GetMessageFromDB(ACode: Integer; AException: Boolean = True);

    // Получение параметра с БД // 22.07.2009
    function GetValueForType(AValue: Variant; AType: Variant; AFormat: Variant): Variant;
    function GetGlobalParam(AName: string = 'DatabaseStructureVersion'): Variant;
    function GetParam(AName: string; ACategory: Integer = 1; AForUser: Integer = 1): Variant;
    function SetParam(AName: string; AValue: Variant; ACategory: Integer = 1; AForUser: Integer = 1): Boolean;

    function GetParamSession(AName: string; ACategory: Integer = 1): Variant;
    function SetParamSession(AName: string; AValue: Variant; ACategory: Integer = 1): Boolean;

    //
    function AutoincIntegerFieldValue(ATable, AField: string): Integer;

    // Получение маски TEditMask по имени маски из БД
    function GetMaskFromDB(AMaskName: string; var ACaption, AHint: string): string;

    procedure BackupDataBase;

    procedure RestoreDataBase;

    function StoredProc(const AStoredProcName, AInpParams: string;
      const AInpParamsValues: Variant; AOutParam: string = '';
      ATransaction: TSQLTransaction = nil; ACommit: Boolean = True): Variant;

    procedure QueryExec(ASQLText: string; const AParamsValues: Variant;
      ATransaction: TSQLTransaction = nil; ACommit: Boolean = True);

    procedure FillDataToDataSet(AFields, AValues : string; ADataSet : TDataSet);

    procedure SpecifySettingsDataset(ADataset: TSQLQuery);

    procedure SpecifySettingsDBLookupComboBox(
      ADBLookupComboBox: TDBLookupCombobox; AListSource: TDataSource;
      AListField : string = 'name'; AShowTitles: Boolean = False;
      AListFieldIndex: Integer = 0; AKeyField : string = 'id');

    procedure SetDatasetConditions(ADataset: TSQLQuery; AName: string;
      AActive: Boolean);

    function UserHasPrivileges(ARelationName: string): Boolean;

    procedure SaveLogToDB(ALogName, ALogText, ALogStatus: string);

    procedure ExtractionDataFromDataSet(AFields, AValues: string;
      ADataSet: TDataSet);

    function GetCorrespondence(ATable1, AFieldName1: string;
      AFieldValue1: Variant; ATable2, AFieldName2: string): Variant;

    function ChangeUserPassword(ADBMSUserName, AOldPassword, ANewPassword: string): Boolean;
    // Загрузка файла из БД
    function LoadFileFromDB(AID: Integer; AFileName: string = ''; AFilePath: string = '';
      ADBTableName: string = 'SYS_FILE'; ADBFieldName: string = 'FILE_BODY'): Boolean;

    // Получение значения генератора
    function GetGenID(const AGeneratorName:string; AStep:Int64; ATransaction: TSQLTransaction = nil): Int64;

    procedure AddEventAlert(AValue: string);

    function ReplaceTagsInTextDB(ADS: TDataSource; ATemplateFile: string): string;
    //
  end;

var
  dmDB: TdmDB;

implementation

{$R *.lfm}

{ TdmDB }

uses
  Protect, UtilsUnit, ConstsUnit, ExecutionFormUnit, MessageFormUnit;

{ TConnectingThread }

procedure TConnectingThread.Execute;
begin
  //inherited;
  Execute;
  if Terminated then Exit;
  Connect;
end;

constructor TConnectingThread.Create();
begin
  //
end;

{ TConnectThread }

function TConnectThread.Connect: Boolean;
begin
  Result := dmDB.ConnectToBase(dmDB.DatabaseMain);
end;

procedure TdmDB.DataModuleCreate(Sender: TObject);
begin
  DBVersion := 4; // Версия текущей структуры БД. Используется для проверки приложением.
  //
  SQL_Main := '';
  SQL_Extra := '_Extra';
  SQL_Temp := '_Temp';
  SQL_Edit := '_Edit';
  SQL_View := '_View';
  SQL_DragDrop := '_DragDrop';
  // Файл сохранения ширины столбцов таблиц
//  ifColumnsWidths := TIniFile.Create(SysPath + SColumnsLayoutIni);
end;

procedure TdmDB.DataModuleDestroy(Sender: TObject);
begin
  DisconnectBase;
end;

procedure TdmDB.ConnectingThreadDone(Sender: TObject);
begin
  ExecutionFormClose;
//  MessageBeep(MB_OK);
//  MessageBox(0, 'Поток закончил выполнение', 'Инфо', MB_OK_WARN);
end;

procedure TdmDB.RegisterAllClasses;
begin
  //RegisterClasses([TUserForm]);
end;

procedure TdmDB.UnRegisterAllClasses;
begin
  //UnRegisterClasses([TUserForm])
end;

procedure TdmDB.LoadDBSettings;
begin
  if Trim(CurrentDBConfig) = EmptyStr then
  begin
    ShowMessageForm('Не выбрана конфигурация БД', 0,
       'Необходимо выбрать конфигурацию базы данных из предложенного списка.');
    Exit;
  end;
  // Основной конфиг программы
  try
    with ifDB do
    begin
      // Путь к БД и к копии БД
      CurrentDBName := ReadString(CurrentDBConfig, 'DBName', 'minidb');
      CurrentDBCharset := ReadString(CurrentDBConfig, 'CharSet', 'utf8');

      CurrentServerHostName := ReadString(CurrentDBConfig, 'ServerNameDB', '127.0.0.1');
      CurrentServerPort := ReadInteger(CurrentDBConfig, 'ServerPortDB', 5432);
    end;
  except
    // журнал регистрации ошибок
    on E: Exception do
    begin
      SaveLogToFile('System', MSG_Warning + ': ' + E.Message, 'err');
      Application.MessageBox(PChar('Exception: '#13#10 + E.Message), MSG_Warning, MB_OK_EXCL);
    end;
  end;
end;

function TdmDB.Connect(AReconnect: Boolean): Boolean;
begin
  if CheckConnected then
    Exit;

  if AReconnect then
  begin
    if Application.MessageBox(PChar('Потеряно соединение c базой данных "' + CurrentDBConfig + '"' +
                              CRLF + 'Попытаться восстановить соединение?'), MSG_Confirmation,
      MB_YESNO_QUEST) <> IDYES then
      Exit;
  end;

  Screen.Cursor := crSQLWait;
  Result := False;
  if Result then
    Exit;

//  ExecutionFormShow;
 // try
  with TConnectThread.Create() do
    OnTerminate := @ConnectingThreadDone;

  ExecutionFormShow('Соединение с сервером БД.', True);

 // finally
   // ExecutionFormClose;
  //end; //}
  dmDB.Connecting := True;

  Screen.Cursor := crDefault;

  Result := DatabaseMain.Connected;
end;

function TdmDB.ConnectToBase(ADatabase: TPQConnection): Boolean;
var
  sErrorMsg: string;
//  sDB: string;
  iCounter: Integer;
begin
  Result := False;

  Connecting := True;
  //Sleep(5000); //  Тест

  DatabaseMain.Close;

  LoadDBSettings;   // Загрузка настроек БД

  with ADatabase do
  begin
    LoginPrompt := False;

    CloseDataSets;
    Close;

    // <24.11.2009>
    HostName := CurrentServerHostName;
    DatabaseName := CurrentDBName;

    {$IFDEF LOGIN}
    UserName := CurrentUserNameDBMS;
    Password := CurrentUserNameDBMSpw;
    Role := CurrentUserRole;
    {$ELSE}
    UserName := 'postgres';
    Password := 'postgres';
      {$IFDEF DEBUG}
       Role := 'role_dev';
      {$ENDIF}
    {$ENDIF}
    CharSet := CurrentDBCharset;
    Params.Clear;
    Params.Add('port=' + IntToStr(CurrentServerPort));

    try
      if Connected then
        Close;

      KeepConnection := True;
      Open;
      if Connected then
      begin
        //iCounter := 4;
        iCounter := GetGlobalParam();
        if DBVersion <> iCounter then
        begin
          sErrorMsg := 'Версия серверной и клиентской структуры баз данных не совпадают.' + CRLF +
                       'Необходимо обновить клиентское приложение!' + CRLF +
                        CRLF +
                       'Версия клиентского приложения: ' + GetProductVersion + ' (' + GetBuildVersion + ')' + CRLF +
                       'Версия клиентской БД: ' + IntToStr(DBVersion) + CRLF +
                       'Версия серверной БД: ' + IntToStr(iCounter);
          ShowMessageForm('Версия базы данных не совпадает.', 1, sErrorMsg);
          SaveLogToDB('DB', sErrorMsg, 'wrn');
          ConnectedDB := False;
          Connecting := False;
          Application.Terminate;
          Halt(0);
          Exit;
        end;

        ConnectedDB := True;
      end;
    except
      on E: Exception do
      begin
        sErrorMsg := 'Проверьте параметры в основных настройках программы:';
        sErrorMsg := sErrorMsg + CRLF + 'Регистрация базы данных или подключение.';
        sErrorMsg := sErrorMsg + CRLF;
        sErrorMsg := sErrorMsg + CRLF + '______________________';
        sErrorMsg := sErrorMsg + CRLF + 'Отладочная информация:';
        sErrorMsg := sErrorMsg + CRLF + 'ClassName: ' + E.ClassName;
        sErrorMsg := sErrorMsg + CRLF + E.Message;
        SaveLogToDB('DB', sErrorMsg, 'err');
        ShowMessageForm('Исключение!', 2, sErrorMsg);

        ConnectedDB := False;
        Connecting := False;
        Application.Terminate;
        Halt(0);
      end;
    end;
  end;

  if not CheckConnected then
    Exit;

  if not ConnectedDB then
    Exit;

  for iCounter := 0 to ComponentCount - 1 do
  begin
    if (Components[iCounter] is TSQLQuery) then
    begin
      if (Components[iCounter] as TSQLQuery).Tag = 0 then
        SpecifySettingsDataset((Components[iCounter] as TSQLQuery));
    end;
    if (Components[iCounter] is TDataSource) then
    begin
      if (Components[iCounter] as TDataSource).Tag = 0 then
        (Components[iCounter] as TDataSource).AutoEdit := False;
    end;
  end;

  //  ConnectToSystemBase;

  // Проверка версии клиентского приложения <28.04.2010> WP Nazir
  if DataSetApp.Active then
  begin
    DataSetApp.Close;
    DataSetApp.Open;
  end
  else
    LoadSQLToQueryByName('sys.application' + SQL_Main, DataSetApp, 'ID in (select first 1 RESULT_OUT from P_SYS_APPLICATION)');

  if DataSetApp.Active then
  begin
    sErrorMsg := DataSetApp.FieldByName('FILE_VERSION').AsString;
    if Trim(sErrorMsg) <> EmptyStr then
    if sErrorMsg <> GetFileVersion then
    begin
      sErrorMsg := 'Необходимо обновить клиентское приложение!' + CRLF +
                    CRLF +
                   'Версия клиентского приложения: ' + CRLF +
                   #9'установленная: '#9 + GetFileVersion + CRLF +
                   #9'необходимая: '#9 + sErrorMsg;
      ShowMessageForm('Необходимо обновить клиентское приложение!', 1, sErrorMsg);
      SaveLogToDB('DB', sErrorMsg, 'wrn');
      if Application.MessageBox(PChar('Попытаться обновиться с сервера?'), PChar(MSG_Confirmation), MB_YESNOCANCEL_QUEST) = IDYES then
      try
        if LoadFileFromDB(DataSetApp.FieldByName('id_file').AsInteger) then
        begin
          LoadFileFromDB(50);
          if FileExists(PathApp + 'MiniUpdater.exe') then
          begin
            ConnectedDB := False;
            Connecting := False;
            Application.Terminate;
            //ShellExecute(0, 'open', PWideChar(PathApp + 'MiniUpdater.exe'), PWideChar('"' + PathApp + AppExeName + '" -start'), nil, SW_SHOW);
            ExecuteProcess(PathApp + 'MiniUpdater.exe',['-start']);
            Halt(0);
            Exit; //}
          end;
        end
        else
          Application.MessageBox(PChar('Загрузка файла не удалась!'), PChar(MSG_Information), MB_OK_INFO);
      except
      end;
      {ConnectedDB := False;
      Connecting := False;
      Application.Terminate;
      ExitProcess(0);
      Exit; //}
    end;
  end;
  // ^Проверка версии клиентского приложения <28.04.2010> WP Nazir

  // Загрузка параметров пользователя <02.03.2012> WP Nazir
  if DataSetParams.Active then
  begin
    DataSetParams.Close;
    DataSetParams.Open;
  end
  else
    LoadSQLToQueryByName('UserParams' + SQL_Main, DataSetParams);
  // ^Загрузка параметров пользователя <02.03.2012> WP Nazir


  TransactionMain.Active := False;
  TransactionMain.Active := True;//}

  Connecting := False;
  MaxColumnWidth := GetGenID('GEN_SYS_FIELD_MAX_COL_WIDTH', 0);
  MinColumnWidth := GetGenID('GEN_SYS_FIELD_MIN_COL_WIDTH', 0);
  DefaultColumnWidth := GetGenID('GEN_SYS_FIELD_DEFAULT_COL_WIDTH', 0);
  Result := True;
end;

function TdmDB.Disconnect: Boolean;
begin
  Result := DisconnectBase;
end;

function TdmDB.DisconnectBase: Boolean;
begin
  {if not CheckConnected then
    Exit; //}

  //TransactionMain.CloseAllQueryHandles;
//  TransactionMain.RollbackRetaining
  if TransactionMain.Active then
    TransactionMain.Active := False;
//  DatabaseMain.CloseDataSets;
//  DatabaseMain.DBParams.Clear;
  DatabaseMain.Connected := False;
  Result := not DatabaseMain.Connected;
end;

function TdmDB.CheckConnected: Boolean;
begin
  Result := False;
  if DatabaseMain.Connected then
    Result := True
  else
  begin
    ConnectedDB := False;
    Connecting := False;
  end;
end;

function TdmDB.LoadSQLToQuery(ASQLText: string; var ADataSet: TSQLQuery;
  AConditions: string; AGroupByClause: string; AOrderClause: string): string;
begin
  //if CheckConnected then
  //begin
  //  with DataSetMain do
  //  try
  //     SQL.Text := ASQLText;
  //     Active := True;
  //  except
  //    ShowMessage('Query error');
  //  end;
  //  if Active then
  //  begin
  //    with DBGridMain do
  //    try
  //      Options := Options + [dgCellEllipsis, dgCellHints, dgDblClickAutoSize,
  //      dgDisplayMemoText];
  //      //Options2 := Options2 + [goScrollToLastCol, goScrollToLastRow];
  //    except
  //      ShowMessage('Grid error');
  //    end;
  //  end;
  //end;

  Result := '';

  if not CheckConnected then
    Exit;

  if (Trim(ASQLText) = EmptyStr) or (Trim(ASQLText) = 'NULL') then
  begin
    SaveLogToDB('System', 'Запрос на выборку (select) пуст!', 'wrn');
    Exit;
  end;

  with ADataSet do
  try
    Close;
    Filter := '';
    SQL.Clear;
    if Trim(ASQLText) <> EmptyStr then
    begin
      SQL.Text := ASQLText;
      try
        //Conditions.Clear;
        //MainWhereClause
        if (Trim(AConditions) <> EmptyStr) then
          //Conditions.AddCondition('Conditions', AConditions, True);
          StringReplace(SQL.Text, 'WHERE TRUE', 'WHERE TRUE AND ' + AConditions, [rfIgnoreCase, rfReplaceAll]);

        if (Trim(AGroupByClause) <> EmptyStr) then
          //GroupByClause := AGroupByClause;
          StringReplace(SQL.Text, 'GROUP BY', 'GROUP BY ' + AGroupByClause, [rfIgnoreCase, rfReplaceAll]);

        if (Trim(AOrderClause) <> EmptyStr) then
          //OrderClause := AOrderClause;
          StringReplace(SQL.Text, 'ORDER BY', 'ORDER BY ' + AOrderClause, [rfIgnoreCase, rfReplaceAll]);

        {slTemp.Clear; // <OLD>
        slTemp.Text := AParams;
        for iCounter := 0 to slTemp.Count - 1 do
        begin
          sTemp := slTemp.Strings[iCounter];
          sParamName := Copy(sTemp, 1, Pos('=', sTemp) - 1);
          sParamValue := Copy(sTemp, Pos('=', sTemp) + 1, Length(sTemp));
          if ParamExist(sParamName, iTemp) then
            ParamByName(sParamName).AsString := sParamValue
          else
          begin
            if Pos('WHERE ', UpperCase(SelectSQL.Text)) = 0 then
            begin
              SelectSQL.Add('where ');
              SelectSQL.Add(sParamName + '=' + sParamValue);
            end
            else
              SelectSQL.Add(' and ' + sParamName + '=' + sParamValue);
          end;
        end; //}

        {if AConditions = '' then
          SetDatasetConditions(ADataSet, GetFieldValueFromSQLs(ASQLName, 'CONDITION_NAME'))
        else
          SetDatasetConditions(ADataSet, AConditions); //}

        //Conditions.Apply;
        CurrentSQL := SQL.Text;
        Open;
      except
        on E: Exception do
        begin
          Result := 'err';
          SaveLogToDB('SQL', 'Ошибка в LoadSQLToQueryByName загрузки ' + ASQLText + ': ' + E.Message + ' ClassName=' + E.ClassName, 'err');
        end;
      end;
      //Result := SQL.Text;
    end;
  except
    on E: Exception do
    begin
      Result := 'err';
      SaveLogToDB('SQL', 'Ошибка в LoadSQLToQuery загрузки: ' + E.Message, 'err');
    end;
  end;
end;

function TdmDB.LoadSQLToQueryByName(ASQLName: string;
  var ADataSet: TSQLQuery; AConditions: string; AGroupByClause: string;
  AOrderClause: string): string;
begin
  try
    FillCode(@TdmDB._LoadSQLToQueryByName, 'TdmDB._LoadSQLToQueryByName');
    Result := _LoadSQLToQueryByName(ASQLName, ADataSet, AConditions, AGroupByClause, AOrderClause);
    ClearCode(@TdmDB._LoadSQLToQueryByName, 'TdmDB._LoadSQLToQueryByName');
  except
  end; //}
end;

function TdmDB._LoadSQLToQueryByName(ASQLName: string;
  var ADataSet: TSQLQuery; AConditions: string; AGroupByClause: string;
  AOrderClause: string): string;
var
  sSQLText: string;
begin
  if not CheckConnected then
    Exit;

  Result := '';
  if Trim(ASQLName) = EmptyStr  then
    Exit;

  sSQLText := GetSQL(ASQLName);
  if (Trim(sSQLText) = EmptyStr) or (Trim(sSQLText) = 'NULL') then
  begin
    SaveLogToDB('System', 'Запрос на выборку (select) пуст для ASQLName=' + ASQLName, 'wrn');
    Exit;
  end;

  if SetSQLs(ASQLName, ADataset) = 'OK' then
  begin
    if LoadSQLToQuery(sSQLText, ADataSet, AConditions, AGroupByClause,
      AOrderClause) = 'err' then
      SaveLogToDB('SQL', 'Ошибка в LoadSQLToQueryByName загрузки SQLName=' + ASQLName, 'err');
  end;
end;

function TdmDB.GetFieldValueFromTable(AFieldName, ATableName, ACondition: string
  ): string;
// Получение значения поля AFieldName из таблицы ATableName по условию ACondition
var
  DataSetTmp: TSQLQuery;
begin
  if not CheckConnected then
    Exit;

  DataSetTmp := TSQLQuery.Create(Self);
  SpecifySettingsDataset(DataSetTmp);
  Result := '';
  with DataSetTmp do
  try
    SQL.Add('SELECT');
    SQL.Add(AFieldName);
    SQL.Add('FROM');
    SQL.Add(ATableName);
    if ACondition <> EmptyStr then
    begin
      SQL.Add('WHERE');
      SQL.Add(ACondition);
    end;
    Open;
    if RecordCount > 0 then
   // if RecordCountFromSrv > 0 then
      Result := FieldByName(AFieldName).AsString;
    Close;
  except
    on E: Exception do
    begin
      SaveLogToDB('SQL', 'Ошибка GetFieldValueFromTable ' + ATableName + '.' + AFieldName + ': ' + E.Message, 'err');
    end;
  end;
  DataSetTmp.Free;
end;

function TdmDB.GetFieldValueFromSQLs(ASQLName, AFieldName: string): string;
var
  sTemp: string;
begin
  Result := 'Error';
  if Trim(ASQLName) = EmptyStr then
    Exit;
  if Trim(AFieldName) = EmptyStr then
    Exit;

  if not CheckConnected then
    Exit;

  with DataSetSQL do
  try
    Close;
    SQL.Clear;
    SQL.Add('SELECT ss.NAME, ss.' + AFieldName);
    SQL.Add('FROM sys.sql AS ss');
    SQL.Add('WHERE ss.NAME = ''' + ASQLName + '''');
    //ParamByName('NAME').AsString := ASQLName;
    Open;
    Result := 'NULL';
    if RecordCount > 0 then
    //if RecordCountFromSrv > 0 then
      Result := FieldByName(AFieldName).AsString;
  except
    on E: Exception do
    begin
     //  if E is EAccessViolation then
      if not CheckConnected then
        Connect(True);
      sTemp := 'SQLName: ' + ASQLName;
      sTemp := sTemp +  CRLF + 'Запрос:' + CRLF + SQL.Text;
      sTemp := sTemp + CRLF + '___________';
      sTemp := sTemp + CRLF + 'Исключение: <<' + E.Message + '>> ClassName=' + E.ClassName;
      ShowMessageForm('Ошибка в GetFieldValueFromSQLs открытия таблицы SQLs:', 2, sTemp);
      SaveLogToDB('SQL', 'Ошибка в GetFieldValueFromSQLs открытия таблицы SQLs: ' + CRLF + sTemp, 'err');
    end;
  end;
end;

function TdmDB.GetSQL(ASQLName: string; ASQLType: Integer): string;
begin
  Result := '';
  case ASQLType of
    1: Result := GetFieldValueFromSQLs(ASQLName, 'sql_select');
    2: Result := GetFieldValueFromSQLs(ASQLName, 'sql_refresh');
    3: Result := GetFieldValueFromSQLs(ASQLName, 'sql_insert');
    4: Result := GetFieldValueFromSQLs(ASQLName, 'sql_update');
    5: Result := GetFieldValueFromSQLs(ASQLName, 'sql_delete');
  else
    Result := GetFieldValueFromSQLs(ASQLName, 'sql_select');
  end;
end;

function TdmDB.SetSQLs(ASQLName: string; var ADataSet: TSQLQuery): string;
begin
  Result := 'err';

  if not CheckConnected then
    Exit;

  Result := 'OK';
  with DataSetSQL do
  try
    ADataset.RefreshSQL.Text := GetFieldValueFromSQLs(ASQLName, 'sql_refresh');
    ADataset.InsertSQL.Text := GetFieldValueFromSQLs(ASQLName, 'sql_insert');
    ADataset.UpdateSQL.Text := GetFieldValueFromSQLs(ASQLName, 'sql_update');
    ADataset.DeleteSQL.Text := GetFieldValueFromSQLs(ASQLName, 'sql_delete');
  except
    on E: Exception do
    begin
      Result := 'err';
      SaveLogToDB('SQL', 'Ошибка в SetSQLs загрузки из БД ' + ASQLName + ': ' + E.Message, 'err');
    end;
  end;
end;

procedure TdmDB.SaveColumnsWidths(AGridName: string; AColumns: TDBGridColumns
  );
begin

end;

procedure TdmDB.SetDBGridSettings(ASQLName: string; var ADBGrid: TDBGrid;
  ADataSource: TDataSource; AOnlySave: Boolean; AReadOnly: Boolean);
begin

end;

function TdmDB.GetFieldName(AFieldName: string): string;
begin

end;

function TdmDB.GetTableName(AFieldName: string): string;
begin

end;

function TdmDB.GetFieldsDescriptions(AFieldName, ATable: string): string;
begin

end;

function TdmDB.GetRecordData(ADataSet: TDataSet; ADivider: string): string;
begin

end;

procedure TdmDB.SetDBName(const Value: string);
begin
  CurrentDBName := Value;
  try
    ifDB.WriteString(CurrentDBConfig, 'DBName', CurrentDBName);
  except
    // журнал регистрации ошибок
    on E: Exception do
      SaveLogToDB(GlobalDBIniFile, MSG_Warning + ': ' + E.Message, 'err');
  end;
end;


procedure TdmDB.GetMessageFromDB(ACode: Integer; AException: Boolean);
begin

end;

function TdmDB.GetValueForType(AValue: Variant; AType: Variant; AFormat: Variant
  ): Variant;
begin

end;

function TdmDB.GetGlobalParam(AName: string = 'DatabaseStructureVersion'): Variant;
begin
  if CheckConnected then
  try
    with DataSetTemp do
    begin
     Close;
     SQL.Clear;
     SQL.Add('SELECT public.fc_sys(name_inp => ''' +  AName + ''');');

     Open;

     if RecordCount > 0 then
     begin
       //Next;
       Result := DataSetTemp.Fields[0].AsInteger;
     end;

     Close;
    end;
  except
  end;
end;

function TdmDB.GetParam(AName: string; ACategory: Integer; AForUser: Integer
  ): Variant;
begin

end;

function TdmDB.SetParam(AName: string; AValue: Variant; ACategory: Integer;
  AForUser: Integer): Boolean;
begin

end;

function TdmDB.GetParamSession(AName: string; ACategory: Integer): Variant;
begin

end;

function TdmDB.SetParamSession(AName: string; AValue: Variant;
  ACategory: Integer): Boolean;
begin

end;

function TdmDB.AutoincIntegerFieldValue(ATable, AField: string): Integer;
begin

end;

function TdmDB.GetMaskFromDB(AMaskName: string; var ACaption, AHint: string
  ): string;
begin

end;

procedure TdmDB.BackupDataBase;
begin

end;

procedure TdmDB.RestoreDataBase;
begin

end;

function TdmDB.StoredProc(const AStoredProcName, AInpParams: string;
  const AInpParamsValues: Variant; AOutParam: string;
  ATransaction: TSQLTransaction; ACommit: Boolean): Variant;
begin

end;

procedure TdmDB.QueryExec(ASQLText: string; const AParamsValues: Variant;
  ATransaction: TSQLTransaction; ACommit: Boolean);
type
  TTempArray = array of array of Variant;
var
  VarArray: array of Variant;
  iCounter: Integer;
begin
  if not CheckConnected then
    Exit;

  if ATransaction = nil then
    ATransaction := TransactionExec;

  if AParamsValues <> Null then
    DynArrayFromVariant(Pointer(VarArray), AParamsValues, TypeInfo(TTempArray));
    //CastVariantToArray(AParamsValues, VarArray);
  with DataSetTemp do
  try
    Transaction := ATransaction;
    if not SQLTransaction.Active then
      DataSetTemp.SQLTransaction.StartTransaction;
    SQL.Text := ASQLText;
    Prepare;
    try
      if AParamsValues <> Null then
      for iCounter := 0 to Length(VarArray) - 1 do
        Params[iCounter].Value := VarArray[iCounter];
      ExecSQL;
    finally
      if ACommit then
        SQLTransaction.Commit;
      Close;
    end;
  except
    on E: Exception do
    begin
      SaveLogToDB('SQL', 'Ошибка в dmDB.QueryExec:' + E.Message, 'err');
    end;
  end;
end;

procedure TdmDB.FillDataToDataSet(AFields, AValues: string; ADataSet: TDataSet);
begin

end;

procedure TdmDB.SpecifySettingsDataset(ADataset: TSQLQuery);
begin

end;

procedure TdmDB.SpecifySettingsDBLookupComboBox(
  ADBLookupComboBox: TDBLookupCombobox; AListSource: TDataSource;
  AListField: string; AShowTitles: Boolean; AListFieldIndex: Integer;
  AKeyField: string);
begin

end;

procedure TdmDB.SetDatasetConditions(ADataset: TSQLQuery; AName: string;
  AActive: Boolean);
var
  sConditions: TStringList;
  iCounter: Integer;
begin
  if not CheckConnected then
    Exit;

  if AName =  EmptyStr then
    Exit;
  sConditions := TStringList.Create;
  //with ADataset do
  //begin
  //  Close;
  //  Conditions.Clear;
  //  sConditions.Text := StringReplace(AName, ';', #13#10, [rfIgnoreCase, rfReplaceAll]);
  //  if DataSetConditions.Active then
  //    DataSetConditions.Close;
  //  DataSetConditions.SelectSQL.Text :=
  //    GetFieldValueFromSQLs('Conditions' + SQL_Main, 'sql_select');
  //  DataSetConditions.Open;
  //  for iCounter := 0 to sConditions.Count - 1 do
  //  begin
  //    if DataSetConditions.Locate('name', sConditions[iCounter], []) then
  //      Conditions.AddCondition(sConditions[iCounter],
  //                 DataSetConditions.FieldValues['condition'], AActive);
  //  end;
  //  Conditions.Apply;
  //  Open;
  //end;
  sConditions.Free;
end;

function TdmDB.UserHasPrivileges(ARelationName: string): Boolean;
var
  vTemp: Variant;
begin
  if not CheckConnected then
    Exit;

  Result := False;
  //try
  //  // Вызов хранимой процедуры
  //  vTemp := StoredProc('P_SYS_USER_HAS_PRIVILEGES', ':RELATION_NAME_INP', ARelationName, 'RELATION_NAME_OUT');
  //  if vTemp = NULL then
  //    Exit;
  //  if ARelationName = Trim(vTemp) then
  //    Result := True;
  //except
  //  on E: Exception do
  //  begin
  //    Result := False;
  //    SaveLogToDB('SQL', 'Ошибка в функции UserHasPrivileges: ' + E.Message, 'err');
  //  end;
  //end;
end;

procedure TdmDB.SaveLogToDB(ALogName, ALogText, ALogStatus: string);
// Запись журналов регистраций
var
  iEventType: Integer;
  vTemp: Variant;
begin
  if not CheckConnected then
  begin
    SaveLogToFile('SQL', 'Нет соединения с БД в процедуре SaveLogToDB пишем в файл:', 'err');
    SaveLogToFile(ALogName, ALogText, ALogStatus);
    Exit;
  end;

  iEventType := -1;
  if ALogStatus = 'inf' then
    iEventType := 0;
  if ALogStatus = 'wrn' then
    iEventType := 1;
  if ALogStatus = 'err' then
    iEventType := 2;
  if ALogStatus = 'exl' then
    iEventType := 3;
  if ALogStatus = 'qst' then
    iEventType := 4;
  if ALogStatus = 'hnd' then
    iEventType := 5;
  if ALogStatus = 'ast' then
    iEventType := 6;
  if ALogStatus = 'stp' then
    iEventType := 7;
  if ALogStatus = 'msk' then
    iEventType := 8;

  vTemp := NULL;
  try
    //DatabaseMain.GetServerTime;
    // Вызов хранимой процедуры
    vTemp := StoredProc('P_SYS_LOG_INSERT', ':JOURNAL_INP, :COMPUTER_NAME_INP, :EVENT_TYPE_INP, :TEXT_INP',
       VarArrayOf([ALogName, GetComputerNetName, iEventType, ALogText]), 'RESULT_OUT');
    if vTemp = NULL then
      Exit;
  except
    on E: Exception do
    begin
      SaveLogToFile('SQL', 'Ошибка в процедуре SaveLogToDB (пишем в файл)' + CRLF + E.Message, 'err');
      SaveLogToFile(ALogName, ALogText, ALogStatus);
    end;
  end;
  if (vTemp = NULL) or (vTemp = 0) then
    SaveLogToFile(ALogName, ALogText, ALogStatus);
end;

procedure TdmDB.ExtractionDataFromDataSet(AFields, AValues: string;
  ADataSet: TDataSet);
begin

end;

function TdmDB.GetCorrespondence(ATable1, AFieldName1: string;
  AFieldValue1: Variant; ATable2, AFieldName2: string): Variant;
begin

end;

function TdmDB.ChangeUserPassword(ADBMSUserName, AOldPassword,
  ANewPassword: string): Boolean;
begin
  Result := False;
  LoadDBSettings;
  //with SecurityServiceMain do
  //try
  //  //ClearParams; // OLD
  //  Params.Clear;
  //  Params.Add('user_name=' + ADBMSUserName);
  //  Params.Add('password=' + AOldPassword); //}
  //
  //  SecurityAction := ActionModifyUser;
  //  Attach;
  //  UserName := ADBMSUserName;
  //  Password := ANewPassword;
  //  //SQlRole := CurrentUserRole;
  //  //DisplayUser(ADBMSUserName);
  //  ModifyUser;
  //  Detach;
  //  Result := True;
  //except
  //end;
end;

function TdmDB.LoadFileFromDB(AID: Integer; AFileName: string;
  AFilePath: string; ADBTableName: string; ADBFieldName: string): Boolean;
begin

end;

function TdmDB.GetGenID(const AGeneratorName: string; AStep: Int64;
  ATransaction: TSQLTransaction): Int64;
begin

end;

procedure TdmDB.AddEventAlert(AValue: string);
begin

end;

function TdmDB.ReplaceTagsInTextDB(ADS: TDataSource; ATemplateFile: string
  ): string;
begin

end;

end.

