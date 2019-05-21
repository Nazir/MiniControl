unit LoginFormUnit;

{$mode objfpc}{$H+}

{******************************************************************************}
{                                                                              }
{  Unit: User Authentication Window Module                                     }
{                                                                              }
{  Copyright: Nazir © 2002-2019                                                }
{  Development: Nazir K. Khusnutdinov (aka Naziron or Wild Pointer)            }
{  Разработчик: Хуснутдинов Назир Каримович                                    }
{  Email: naziron@gmail.com                                                    }
{  Git: https://github.com/Nazir                                               }
{                                                                              }
{  Modified: 02.07.2006                                                        }
{  Modified: 16.07.2009                                                        }
{  Modified: 10.09.2010                                                        }
{  Modified: 28.07.2011, 15.11.2011                                            }
{  Modified: 16.02.2012, 19.10.2012                                            }
{  Modified: 15.05.2019 (Lazarus)                                              }
{                                                                              }
{******************************************************************************}


interface

{$I Defines.inc}

uses
  LCLIntf, LCLType, LMessages,
  {$IFDEF WINDOWS}windows,{$ENDIF}
  Forms, Graphics, SysUtils, Classes, ActnList, ImgList, Controls,
  StdCtrls, ExtCtrls, Buttons, ComboEx;

type

   { TLoginForm }

   TLoginForm = class(TForm)
    imLogin: TImage;
    lblDBList: TLabel;
    cbxDBList: TComboBox;
    sbSettingsDB: TSpeedButton;
    lblUserName: TLabel;
    cbxUserName: TComboBox;
    sbUserName: TSpeedButton;
    lblPassword: TLabel;
    lePassword: TEdit;
    sbPassword: TSpeedButton;
    lblCurrentUserRole: TLabel;
    cbxCurrentUserRole: TComboBoxEx;
    lblEmail: TLabel;
    ilRole: TImageList;
    chbxSavePassword: TCheckBox;
    btOK: TBitBtn;
    btCancel: TBitBtn;
    alLogin: TActionList;
    acRefreshDBList: TAction;
    acDeleteUserName: TAction;
    acSettingsDB: TAction;
    il16: TImageList;
    acChangeUserPassword: TAction;
    acGetRole: TAction;
    lblProtect: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbxUserNameKeyPress(Sender: TObject; var Key: Char);
    procedure lePasswordKeyPress(Sender: TObject; var Key: Char);
    procedure lePasswordKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbxDBListKeyPress(Sender: TObject; var Key: Char);
    procedure btOKClick(Sender: TObject);
    procedure cbxDBListChange(Sender: TObject);
    procedure acRefreshDBListExecute(Sender: TObject);
    procedure acSettingsDBExecute(Sender: TObject);
    procedure acDeleteUserNameExecute(Sender: TObject);
    procedure TextChange(Sender: TObject);
    procedure acChangeUserPasswordExecute(Sender: TObject);
    procedure cbxUserNameExit(Sender: TObject);
    procedure acGetRoleExecute(Sender: TObject);
  private
    procedure CheckFormVis;
  public
    sUserNameDBMS: string;
    sPass: string;
  end;

implementation

uses
  IniFiles, IdentUnit, Math, ConstsUnit, MessageFormUnit, UtilsUnit,
  SettingsDBUnit, ChangePasswordFormUnit, ImagesUnit;

{$R *.lfm}

procedure TLoginForm.FormCreate(Sender: TObject);
var
  iCounter: Integer;
  sTemp: string;
  imTemp: TImage;
  slRoles: TStringList;
  TempStream: TMemoryStream;
begin
  btOK.Images := dmImages.ilMainMenu16;
  btOK.ImageIndex := dmImages.ImadeIndex_OK;
  btCancel.ImageIndex := dmImages.ImadeIndex_Cancel;
  btCancel.Images := dmImages.ilMainMenu16;

  try
  slRoles := TStringList.Create;
  cbxCurrentUserRole.ItemIndex := -1;
  cbxCurrentUserRole.Clear;
  slRoles.Clear;
  with ifConfig do
  begin
    AvailableRoles.Clear;

    if ReadInteger('Defaults', 'role_user', 1) = 1 then
    begin
      AvailableRoles.Add('role_user');
      cbxCurrentUserRole.ItemsEx.AddItem('Пользователь', 0, 0, -1, -1, PAnsiChar('role_user'));
    end;

    if ReadInteger('Defaults', 'role_dispatcher', 1) = 1 then
    begin
      AvailableRoles.Add('role_dispatcher');
      cbxCurrentUserRole.ItemsEx.AddItem('Диспетчер', 1, 1, -1, -1, PAnsiChar('role_dispatcher'));
    end;

    if ReadInteger('Defaults', 'role_admin', 1) = 1 then
    begin
      AvailableRoles.Add('role_admin');
      cbxCurrentUserRole.ItemsEx.AddItem('Администратор', 2, 2, -1, -1, PAnsiChar('role_admin'));
    end;

    if ReadInteger('Defaults', 'role_read', 1) = 1 then
    begin
      AvailableRoles.Add('role_read');
      cbxCurrentUserRole.ItemsEx.AddItem('Только чтение', 3, 3, -1, -1, PAnsiChar('role_read'));
    end;

    if ReadInteger('Defaults', 'role_supervisor', 1) = 1 then
    begin
      AvailableRoles.Add('role_supervisor');
      cbxCurrentUserRole.ItemsEx.AddItem('Смотритель', 4, 4, -1, -1, PAnsiChar('role_supervisor'));
    end;

    if ReadInteger('Defaults', 'role_expert', 1) = 1 then
    begin
      AvailableRoles.Add('role_expert');
      cbxCurrentUserRole.ItemsEx.AddItem('Эксперт', 5, 5, -1, -1, PAnsiChar('role_expert'));
    end;

    if SectionExists('Roles') then
    begin
      ReadSection('Roles', slRoles);
      if slRoles.Count > 0 then
      begin
        imTemp := TImage.Create(Self);
        for iCounter := 0 to slRoles.Count - 1 do
        begin
          sTemp := slRoles.Strings[iCounter];
          AvailableRoles.Add(sTemp);
          if ValueExists('Roles', sTemp) then
          begin
            if ReadString('Roles', sTemp, EmptyStr) <> EmptyStr then
            begin

              if FileExists(PathImages + 'Roles\' + sTemp + '.bmp') then
                imTemp.Picture.LoadFromFile(PathImages + 'Roles\' + sTemp + '.bmp')
              else
                imTemp.Canvas.FloodFill(0, 0, clFuchsia, fsSurface);

              with cbxCurrentUserRole.ItemsEx.Add do
              begin
                Caption := ReadString('Roles', sTemp, EmptyStr);
                ImageIndex := ilRole.AddMasked(imTemp.Picture.Bitmap, clFuchsia);
                SelectedImageIndex := ImageIndex;
                Data := PAnsiChar(sTemp);
              end;
            end;
          end;
        end;
        imTemp.Free;
      end;
    end
  end;

//  MessageBox(0, PChar(slRoles.Text), '', 0)

  cbxCurrentUserRole.ItemIndex := 0;

  slRoles.Free;
//  lblDBList.Caption := string(Addr(cbxCurrentUserRole.ItemsEx.ComboItems[4].Data^));

  {try
    TempStream := TMemoryStream.Create;
    //Icon.SaveToStream(TempStream);
    dmImages.ilMainMenu32.GetIcon(10,
    //TempStream.Position := 0;
  //  imLogin.Picture.Graphic.LoadFromStream(Stream);
  finally
    TempStream.Free;
  end; //}
  except
    on E: Exception do
      SaveLogToFile('Application', 'Ошибка в методе TLoginForm.FormCreate: ' + E.Message, 'err');
  end;
end;

procedure TLoginForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  // Окно? закрыть? Хрен!
  if (ModalResult = mrOK) or (ModalResult = mrCancel) then
  begin
    if siIdent <> nil then
      siIdent.isVisibleLoginForm := False;
    CanClose := True;
  end
  else
    CanClose := False;
end;

procedure TLoginForm.FormShow(Sender:TObject);
var
  sTemp: string;
begin
  cbxCurrentUserRole.ItemIndex := 0;

  lePassword.Text := '';
  sPass := EmptyStr;

  acRefreshDBList.Execute;

  sTemp := ifConfig.ReadString('Defaults', 'CurrentUserName', EmptyStr);
  
  if cbxUserName.Items.Count > 0 then
    cbxUserName.ItemIndex := cbxUserName.Items.IndexOf(sTemp)
  else
    cbxUserName.Text := sTemp;

  sUserNameDBMS := EmptyStr;
  TextChange(cbxUserName);
  cbxUserNameExit(Sender);

  if Trim(cbxUserName.Text) = EmptyStr then
    cbxUserName.SetFocus
  else
    lePassword.SetFocus;


  lePassword.Text := EmptyStr;
  TextChange(lePassword);

  {$IFDEF PROTECT}
  {$ELSE}
  lblProtect.Show;
  lblProtect.Font.Color := clRed;
  lblProtect.Font.Style := [fsBold];
  lblProtect.Caption := MSG_NoProtection;
  {$ENDIF}

  lblEmail.Caption := 'Написать в службу тех.поддержки ' + GlobalDeveloperEmailAddress;
  lblEmail.Hint := GlobalDeveloperEmailAddress;
  //with lblEmail.Email do
  //begin
  //  Address := GlobalDeveloperEmailAddress;
  //  Subject := 'Техническая поддержка (' + viAppInfo.FileDescription + ')';
  //  with Body do
  //  begin
  //    Clear;
  //    Add('Уважаемые разработчики!');
  //    Add('');
  //    Add('Сводные данные о программе:');
  //    Add(GetShortProjectName);
  //    Add(GetFullFileVersion);
  //    Add('Номер продукта: ' + GlobalProductID);
  //    Add('Номер клиента: ' + GlobalClientID);
  //    Add('Имя, идентифицирующее компьютер в сети: ' + GetComputerNetName);
  //    Add('');
  //    Add('Информация о версии:');
  //    Add(viAppInfo.CompleteVersionInformation);
  //    Add('');
  //  end;
  //end;
end;

procedure TLoginForm.FormDeactivate(Sender: TObject);
begin
  // Попробуй скрыть окно программно :) Хрен!
  CheckFormVis;
end;

procedure TLoginForm.CheckFormVis;
begin
  if siIdent.isVisibleLoginForm then
  begin
    MessageBox(Self.Handle, PChar(MSG_UnauthorizedAccessAttemptWindow),
               PChar(MSG_Warning), MB_OK or MB_ICONERROR);
    // журнал сеансов
    SaveLogToFile('Session', MSG_UnauthorizedAccessAttemptWindow, 'wrn');
    {$IFDEF DEBUG}
    {$ELSE}
    ExitProcess(0);
    {$ENDIF}
  end;
end;

procedure TLoginForm.lePasswordKeyPress(Sender: TObject; var Key: Char);
begin
  {$IFDEF LOGIN_PROTECT}
  if sPass = EmptyStr then
    lePassword.Clear;
  // Псевдоввод в эдит
  if (Key in [#32, '0'..'9', 'A'..'Z', 'a'..'z', 'А'..'я']) then
  begin
    Insert(Key, sPass, lePassword.SelStart + 1);
    //sPass := sPass + Key;
    Key := #219;
  end
  else
  case Ord(Key) of
    VK_RETURN: btOK.Click;
    VK_BACK: Delete(sPass, lePassword.SelStart, 1);
    VK_DELETE: Delete(sPass, lePassword.SelStart + 1, lePassword.SelLength);
    VK_ESCAPE, VK_CLEAR: sPass := EmptyStr;
  end;
  if sPass = EmptyStr then
    lePassword.Clear;
  {if lePassword.Text = EmptyStr then
    sPass := EmptyStr; //}
  {$ELSE}
  sPass := lePassword.Text + Key;
  case Ord(Key) of
    VK_RETURN: btOK.Click;
  end;
  {$ENDIF}
end;

procedure TLoginForm.lePasswordKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  {$IFDEF LOGIN_PROTECT}
  case Ord(Key) of
    VK_DELETE: Delete(sPass, lePassword.SelStart + 1, IfThen(lePassword.SelLength = 0, 1, lePassword.SelLength));
  end;
  {$ELSE}
  {$ENDIF}
end;

procedure TLoginForm.btOKClick(Sender: TObject);
begin
  if Trim(cbxDBList.Text) = EmptyStr then
  begin
    ModalResult := mrRetry;
    Application.MessageBox(PChar(MSG_MustSelectDbConfig),
                           MSG_Information, MB_OK_INFO);
    cbxDBList.SetFocus;
    Exit;
  end;
  if Trim(sUserNameDBMS) = EmptyStr then
  begin
    ModalResult := mrRetry;
    Application.MessageBox(PChar(MSG_MustEnterUserName),
                           MSG_Information, MB_OK_INFO);
    cbxUserName.SetFocus;
    Exit;
  end;
  if Trim(sPass) = EmptyStr then
  begin
    ModalResult := mrRetry;
    Application.MessageBox(PChar(MSG_MustEnterPassword),
                           MSG_Information, MB_OK_INFO);
    lePassword.SetFocus;
    lePassword.Clear;
    Exit;
  end;
  if cbxCurrentUserRole.ItemIndex = 2 then
  if not siIdent.CheckSimplePassword(sPass, True) then
  begin
    ModalResult := mrRetry;
    ShowMessageForm('Пароль не удовлетворяет критерию сложности для роли "' +
                     cbxCurrentUserRole.Items.Strings[cbxCurrentUserRole.ItemIndex] + '"', 1,
                    'Пароль должен содержать не менее ' + IntToStr(siIdent.MinPasswordLength) + ' символов и ' +
                    'не должен состоять из повторяющихся символов!' + CRLF +
                    'Обратитесь к администратору базы данных для смены пароля!', Self);
    Exit;
  end;
end;

procedure TLoginForm.cbxUserNameKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    lePassword.SetFocus;
    {$IFDEF LOGIN_PROTECT}
    {$ELSE}
    lePassword.SelectAll;
    {$ENDIF}
  end;
{  with cbxUserName do
  begin
    Text := Text + Key;
    SelectAll;
  end;}
end;

procedure TLoginForm.cbxUserNameExit(Sender: TObject);
begin
  sUserNameDBMS := arCyr2LatUp(UpperCase(Trim(cbxUserName.Text)), '', '');
  lblUserName.Hint := sUserNameDBMS;
  acGetRole.Execute;
end;

procedure TLoginForm.cbxDBListKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    cbxUserName.SetFocus;
    cbxUserName.SelectAll;
  end;  
end;

procedure TLoginForm.acGetRoleExecute(Sender: TObject);
var
  iCounter: Integer;
  sRole, sCurrRole: string;
begin
  if cbxUserName.Items.Count = 0 then
    Exit;

  if Trim(cbxUserName.Text) = EmptyStr then
    Exit;

  with ifConfig do
  if SectionExists('Users') then
  begin
    if ValueExists('Users', Trim(cbxUserName.Text)) then
    begin
      sRole := ReadString('Users', Trim(cbxUserName.Text), 'role_user');
      for iCounter := 0 to cbxCurrentUserRole.Items.Count - 1 do
      try
        sCurrRole := string(PAnsiChar(Addr(cbxCurrentUserRole.ItemsEx.ComboItems[iCounter].Data^)));
        if sCurrRole = sRole then
        begin
          cbxCurrentUserRole.ItemIndex := iCounter;
          Break;
        end;
      except

      end;
    end;
  end;
end;

procedure TLoginForm.acRefreshDBListExecute(Sender: TObject);
begin
  if Trim(CurrentDBConfig) = EmptyStr then
    CurrentDBConfig := ifConfig.ReadString('Defaults', 'CurrentDB', 'localhost');
  with ifDB do
  begin
    ReadSections(cbxDBList.Items);
    if cbxDBList.Items.Count > 0 then
    begin
      cbxDBList.ItemIndex := 0;
      if cbxDBList.Items.IndexOf(CurrentDBConfig) > -1 then
        cbxDBList.ItemIndex := cbxDBList.Items.IndexOf(CurrentDBConfig);
    end;
  end;
  cbxDBListChange(Sender);
end;

procedure TLoginForm.cbxDBListChange(Sender: TObject);
begin
  TextChange(Sender);
  if cbxDBList.Items.Count > 0 then
    CurrentDBConfig := cbxDBList.Items.Strings[cbxDBList.ItemIndex];
  sPass := EmptyStr;
  lePassword.Text := sPass;
end;

procedure TLoginForm.acSettingsDBExecute(Sender: TObject);
begin
  if ShowSettingsDBForm(cbxDBList.Text, Self) then
    acRefreshDBList.Execute;
end;

procedure TLoginForm.acDeleteUserNameExecute(Sender: TObject);
begin
  if Trim(cbxUserName.Text) = EmptyStr then
  begin
    Application.MessageBox(PChar('Необходимо выбрать пользователя из списка.'),
                           MSG_Information, MB_OK_INFO);
    cbxUserName.SetFocus;
    Exit;
  end;
  if Application.MessageBox(PChar('Удалить пользователя из списка?'),
      MSG_Confirmation, MB_YESNO or MB_ICONQUESTION) <> IDYES then
    Exit;

  with ifConfig do
  if SectionExists('Users') then
  begin
    if ValueExists('Users', cbxUserName.Text) then
    begin
      DeleteKey('Users', Trim(cbxUserName.Text));
      if cbxUserName.Items.Count > 0 then
      begin
        cbxUserName.Items.Delete(cbxUserName.ItemIndex);
      end;
      cbxUserName.SetFocus;
    end;
  end;
  cbxUserName.Text := EmptyStr;
end;

procedure TLoginForm.TextChange(Sender: TObject);
begin
  if (Sender as TComponent).Name = 'lePassword' then
  begin
    if lePassword.Tag <> 1 then
      Exit; 
    with lePassword do
    if Trim(Text) = EmptyStr then
    begin
      Color := clObligatoryBack;
      //Font.Color := clObligatoryFont;
    end
    else
    begin
      Color := clWindow;
      //Font.Color := clWindowText;
    end;
    Exit;
  end;

  if not (Sender is TComboBox) then
    Exit;

  if (Sender as TComboBox).Tag <> 1 then
    Exit;

  with (Sender as TComboBox) do
  if Trim(Text) = EmptyStr then
  begin
    Color := clObligatoryBack;
    //Font.Color := clObligatoryFont;
  end
  else
  begin
    Color := clWindow;
    //Font.Color := clWindowText;
  end;
end;

procedure TLoginForm.acChangeUserPasswordExecute(Sender: TObject);
begin
  if Trim(cbxDBList.Text) = EmptyStr then
  begin
    Application.MessageBox(PChar(MSG_MustSelectDbConfig),
                          MSG_Information, MB_OK_INFO);
    cbxDBList.SetFocus;
    Exit;
  end;
  if Trim(cbxUserName.Text) = EmptyStr then
  begin
    Application.MessageBox(PChar(MSG_MustEnterUserName),
                           MSG_Information, MB_OK_INFO);
    cbxUserName.SetFocus;
    Exit;
  end;
  with TChangePasswordForm.Create(Application) do
  try
    il16.GetIcon(acChangeUserPassword.ImageIndex, Icon);
    leOldPassword.Text := sPass;

    if ShowModal = mrOK then
    begin                       
      if siIdent.ChangePassword(sUserNameDBMS, leOldPassword.Text, leNewPassword.Text, chbxProtected.Checked) then
        MessageBox(Self.Handle, 'Смена пароля прошла успешно!', MSG_Information, MB_OK_INFO)
      else
        MessageBox(Self.Handle, 'Смена пароля не удалась!', MSG_Warning, MB_OK_EXCL);
        
      lePassword.Clear;
      sPass := EmptyStr;
    end;
  finally
    Free;
  end;
end;

end.

