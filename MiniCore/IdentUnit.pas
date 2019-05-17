unit IdentUnit;

{$mode objfpc}{$H+}

{******************************************************************************}
{                                                                              }
{ Unit: User Identification Module                                             }
{                                                                              }
{  Copyright: Nazir � 2002-2019                                                }
{  Development: Nazir K. Khusnutdinov (aka Naziron or Wild Pointer)            }
{  �����������: ����������� ����� ���������                                    }
{  �����������: ������ ������� ����������� (SnugForce)                         }
{  Email: naziron@gmail.com                                                    }
{  Git: https://github.com/Nazir                                               }
{                                                                              }
{  Modified: 14.01.2007                                                        }
{  Modified: 09.05.2009                                                        }
{  Modified: 03.06.2009 - ����������� �/� ���� Nazir                           }
{  Modified: 14.05.2012 - �� �������� � ��������                               }
{  Modified: 16.05.2019 (Lazarus)                                              }
{                                                                              }
{******************************************************************************}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Controls,
  Forms, Dialogs, StdCtrls, StrUtils, DB, GammaUnit;

//const
//  KEY = 314159;  // 465464;     //1325568
  {TODO:  ��� ������ ����� ���� ����}

type
  TSimpleIdent = class(TObject)
  protected
    GM: TSimpleGamma;
    procedure LoadUsersList;
    function LogIn: Boolean;
  public
    IsVisibleLoginForm: Boolean;
    MinPasswordLength: Integer;

    constructor Create;
    destructor Destroy; override;

    function CheckLogin(AUserName, APassword: string): Boolean;

    function GetFieldValueByUserName(ADBMSUserName, AFieldName: string): string;
    procedure ShowFormAdding;
    procedure ChangeLogin(AShowForm: Boolean = True);

    procedure FillUsersList(AList: TStrings; AShowPermis: Boolean = False);

    function LogOut: Integer;

    function CheckSimplePassword(APassword: string; AAdmin: Boolean = False): Boolean;
    function ChangePassword(AUserName, AOldPassword, ANewPassword: string;
      AOldPasswordProtected: Boolean = True): Boolean;
  end;

var
  siIdent: TSimpleIdent;

implementation

{$I Defines.inc}

uses
  LoginFormUnit, Math, ConstsUnit, UtilsUnit, DBUnit, IniFiles,
  ComCtrls, MessageFormUnit;

{ TSimpleIdent }

constructor TSimpleIdent.Create;
begin
  inherited;
  GM := TSimpleGamma.Create;
  MinPasswordLength := 8;
  // ��������� ���� � �������
  LogOut;
end;

destructor TSimpleIdent.Destroy;
begin
  GM.Free;
  inherited;
end;

function TSimpleIdent.CheckLogin(AUserName, APassword: string): Boolean;
begin
  // �������� ������ � ������
  Result := False;

  if Trim(APassword) = EmptyStr then
  begin
    MessageBox(Application.Handle, '��������� ������ ������!',
               MSG_Warning, MB_OK_EXCL); //}
    Exit;           
  end;

  // ������ ������ ����������� (������������ ����) <16.07.2009>:
  if (Trim(AUserName) <> EmptyStr) and (Trim(APassword) <> EmptyStr) then
  begin
    CurrentUserNameDBMS := AUserName;
    CurrentUserNameDBMSpw := APassword;
    {$IFDEF PASSWORD_PROTECT}
    CurrentUserNameDBMSpw := GM.HashString(CurrentUserNameDBMSpw, CurrentUserNameDBMS);
    {$ENDIF}
    // ������� �����������
    Result := dmDB.Disconnect;
    if Result then
      Result := dmDB.Connect;
  end;

  if not Result then
  begin
  //
  end;

  { ������ ������ ����������� (������������ ����): <03.06.2009>
  APassword := GM.HashString(APassword, AUserName);
  if GetFieldValueByUserName(AUserName, 'PASSWD_HASH') = APassword then
    Result := True; //}
end;

procedure TSimpleIdent.FillUsersList(AList: TStrings; AShowPermis: Boolean);
begin
  // ������� ������
  // ������ ������ ����������� (������������ ����) �� <16.07.2009>:
  with ifConfig do
  if SectionExists('Users') then
  begin
    ReadSection('Users', AList);
  end;
  
 { ������ ������ ����������� (������������ ����): <03.06.2009>
  with dmDB.ds_SYS_USER.DataSet do
  try
    First;
    while not Eof do
    begin
      if FieldByName('DELETED').AsInteger = bFalse then
      begin
        if AShowPermis then
          AList.Add(FieldByName('NAME').AsString +
                    ' [' + FieldByName('CAPTION').AsString + ']')
        else
          AList.Add(FieldByName('NAME').AsString);
      end;
      Next;
    end;
  except
    // ������ ����������� ������
    on E: Exception do
      SaveLog('DB', SError + ': ������ �������� ������ �������������: ' +
              E.Message, 'err');
  end; //}
end;

function TSimpleIdent.GetFieldValueByUserName(ADBMSUserName, AFieldName: string): string;
begin
  Result := '0';
  try
    if dmDB.DataSetUsers.Active then
    if dmDB.DataSetUsers.Locate('DBMS_USER', ADBMSUserName, [loCaseInsensitive]) then
      Result := dmDB.DataSetUsers.FieldByName(AFieldName).AsString;
  except

  end;
end;

procedure TSimpleIdent.LoadUsersList;
begin
  with dmDB do
    LoadSQLToQueryByName('sys.users' + SQL_Main, DataSetUsers);
end;
{$WARNINGS OFF}

function TSimpleIdent.LogIn: Boolean;
begin
  try
  // ���� �����. ���������� �������
  Result := False;
  with dmDB do
  if UserHasPrivileges(CurrentUserRole) then
  begin
    LoadUsersList;
    CurrentUserID := StrToInt(GetFieldValueByUserName(CurrentUserNameDBMS, 'ID'));

    if CurrentUserID > 0 then
    begin
      CurrentUserName := GetFieldValueByUserName(CurrentUserNameDBMS, 'NAME');
      CurrentSubdivisionID := StrToInt(GetFieldValueByUserName(CurrentUserNameDBMS, 'ID_SYS_SUBDIVISION'));

      //LoadSQLToQueryByName('SYS_SUBDIVISION' + SQL_Main, DataSet_SYS_SUBDIVISION, 'ID=' + IntToStr(CurrentSubdivisionID));
      //if DataSet_SYS_SUBDIVISION.Active then
      //if DataSet_SYS_SUBDIVISION.RecordCount > 0 then
      //  CurrentSubdivisionCaption := DataSet_SYS_SUBDIVISION.FieldByName('NAME').AsString;
      //DataSet_SYS_SUBDIVISION.Close;

      bRoleAdmin := False;
      if CurrentUserRole = 'MODE_ADMIN' then
        bRoleAdmin := True;
      if CurrentUserRole = 'RDB$ADMIN' then
        bRoleAdmin := True;

      bRoleDispatcher := False;
      if CurrentUserRole = 'MODE_DISPATCHER' then
        bRoleDispatcher := True;

      bRoleSupervisor := False;
      if CurrentUserRole = 'MODE_SUPERVISOR' then
        bRoleSupervisor := True;

      Result := True;
    end
    else
    begin  
      ShowMessageForm('������������ "' + CurrentUserNameDBMS + '" � ������� �� ���������������!', 0,
                 '������������ �� ����� ����� �������� � �������.' + CRLF +
                 '��� ��������� ��������.' + CRLF +
                 '���������� � ��������������.', Application);
    end;
  end
  else
  begin
    ShowMessageForm('������������ "' + CurrentUserNameDBMS +
                     '" �� ����� ����� �� ���� "' + CurrentUserRoleCaption + '"!', 1,
               '������������ �� ����� ����� �������� � �������' +
               ' c ����� "' + CurrentUserRoleCaption + '"!' + CRLF +
               '���������� � �������������� ���� ������.', Application);
  end;
  except
    on E: Exception do
      SaveLogToFile('Application', '������ � ������ TSimpleIdent.LogIn: ' + E.Message, 'err');
  end;
end;

function TSimpleIdent.LogOut: Integer;
begin
  try
  // ��������� � �������
  if (Trim(CurrentUserName) <> EmptyStr) then
    dmDB.SaveLogToDB('Session', 'Logout: ' + CurrentUserName + ' [' +
     CurrentUserNameDBMS + ']; ' + ' [' + IntToStr(CurrentUserID) + ']', 'inf');

  CurrentUserName := EmptyStr;
  CurrentUserNameDBMS := EmptyStr;
 // dmDB.Disconnect;   // ????????????????
  except
    on E: Exception do
      SaveLogToFile('Application', '������ � ������ TSimpleIdent.LogOut: ' + E.Message, 'err');
  end;
end;
{$WARNINGS ON}

procedure TSimpleIdent.ChangeLogin(AShowForm: Boolean);
var
  iCount: Integer;
  iModalResult: Integer;
  bIsHacker: Boolean;
  bLogIn: Boolean;
  sTemp: string;
begin
  sTemp := '';
  try
  {$IFDEF LOGIN}

  { �������� �� ������������?
  MessageBox(Application.Handle, PChar('' + CurrentUserName),
              MSG_Warning, MB_OK_EXCL); //}

  IsVisibleLoginForm := True;
  iCount := 0;
  bIsHacker := False;
  bLogIn := False;
  // ����� ����� �����, ���� ���� ������ AShowForm = True
  if AShowForm then
  with TLoginForm.Create(Application) do
  try
    // ���������� ��������
    cbxUserName.ItemIndex := -1;
    FillUsersList(cbxUserName.Items);
    if cbxUserName.Items.Count > 0 then
      cbxUserName.ItemIndex := 0;
    Caption := '������ �����. [' + AppTitle + ']';
    // ����
    repeat
      //Application.BringToFront;
      Inc(iCount);
      sPass := EmptyStr;
      lePassword.Text := sPass;
      iModalResult := ShowModal;
      CurrentUserRole := 'role_user';
      // ���� ������� �����, ���-�� ������� ����� ��� ���-�� �����, ��
      // ������� �� �����
      if (iModalResult = mrCancel) or (iCount >= 10) or
        (siIdent.isVisibleLoginForm) then
      begin
        bIsHacker := True;
        Break;
      end;

    {$IFDEF DEBUG}
    CurrentUserRole := string(PAnsiChar(Addr(cbxCurrentUserRole.ItemsEx[cbxCurrentUserRole.ItemIndex].Data^)));
    {$ELSE}
    CurrentUserRole := AvailableRoles.Strings[cbxCurrentUserRole.ItemIndex];
    {$ENDIF}

    sTemp := cbxUserName.Text;
    until CheckLogin(sUserNameDBMS, sPass);

    if bIsHacker then
      Application.Terminate;

    CurrentUserRoleCaption := cbxCurrentUserRole.ItemsEx[cbxCurrentUserRole.ItemIndex].Caption;

    // ��������� �������� �����
    bLogIn := LogIn;


    sPass := EmptyStr;
  finally
    Free;
  end;

  if not bLogIn then
  begin
    Application.Terminate;
  end;

  if bLogIn then
  begin
    ifConfig.WriteString('Users', sTemp, CurrentUserRole);
    ifConfig.WriteString('Defaults', 'CurrentDB', CurrentDBConfig);
    ifConfig.WriteString('Defaults', 'CurrentUserName', sTemp);
    //ifConfig.WriteString('Defaults', 'CurrentUserNameDBMS', CurrentUserNameDBMS);
  end;
  
  AppCaption := GetFullProjectName + ' [' + CurrentUserName + '] [' +
                                    CurrentSubdivisionCaption + ']';
  {$ELSE}
  AppCaption := GetShortProjectName + ' [������] [' + GetComputerNetName + ']';
  {$ENDIF}

  with dmDB do
  begin
    SaveLogToDB('Session', 'Login: ' + CurrentUserName + ' [' +
      CurrentUserNameDBMS + ']; '  + ' [' + IntToStr(CurrentUserID) + ']; ' + GetFileVersion + ' - client version', 'inf');
  end;
  
  except
    on E: Exception do
      SaveLogToFile('Application', '������ � ������ TSimpleIdent.ChangeLogin: ' + E.Message, 'err');
  end;
end;

procedure TSimpleIdent.ShowFormAdding;
var
  res: Integer;
  fl: Boolean;
begin
  // ����� ����� ����������
  fl := False;
  with TLoginForm.Create(Application) do
    try
      Caption := '����������, ����������� ����� ������';
      FillUsersList(cbxUserName.Items);
      cbxUserName.Items.Text := cbxUserName.Items[0];
      res := ShowModal;
      if CheckLogin(cbxUserName.Text, sPass) then
        fl := True;
    finally
      Free;
    end;

  if (res <> mrOk) or (not fl) then
    Exit;

end;

function TSimpleIdent.CheckSimplePassword(APassword: string; AAdmin: Boolean = False): Boolean;
var
  iCounter1, iCounter2, iCount, iMaximum: Integer;
begin
  Result := False;
  if not AAdmin then
  begin
    if Length(APassword) < 1 then
      Exit;
    Result := True;
    Exit;
  end;

  // �������� �� ��������

  // �� �����
  if Length(APassword) < MinPasswordLength then
    Exit;

  // �� ������������� �����
  iMaximum := 0;
  for iCounter1 := 1 to Length(APassword) do
  begin
    iCount := 0;
    for iCounter2 := 1 to Length(APassword) do
      if APassword[iCounter1] = APassword[iCounter2] then
        Inc(iCount);
    if iCount > iMaximum then
      iMaximum := iCount;
  end;
  // ���� �� ������ ��������
  if ( iMaximum > (Length(APassword) div 2) ) then
    Exit;

  Result := True;
end;

function TSimpleIdent.ChangePassword(AUserName, AOldPassword,
  ANewPassword: string; AOldPasswordProtected: Boolean = True): Boolean;
begin
  {$IFDEF PASSWORD_PROTECT}
  if AOldPasswordProtected then
    AOldPassword := GM.HashString(AOldPassword, AUserName);
  ANewPassword := GM.HashString(ANewPassword, AUserName);
  {$ENDIF}
  Result := dmDB.ChangeUserPassword(AUserName, AOldPassword, ANewPassword);
end;

initialization
  // ������������ ������
  siIdent := TSimpleIdent.Create;

finalization
  // ��������������� �����
  siIdent.Free;
  
end.
