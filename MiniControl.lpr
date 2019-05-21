program MiniControl;

{$mode objfpc}{$H+}

{******************************************************************************}
{                                                                              }
{  Project: MiniControl                                                        }
{                                                                              }
{  Copyright: Nazir © 2002-2019                                                }
{  Development: Nazir K. Khusnutdinov (aka Naziron or Wild Pointer)            }
{  Разработчик: Хуснутдинов Назир Каримович                                    }
{  Email: naziron@gmail.com                                                    }
{  Git: https://github.com/Nazir                                               }
{                                                                              }
{  Create: 15.05.2019                                                          }
{  Modify: 15.05.2019                                                          }
{                                                                              }
{******************************************************************************}

uses
  {$IFDEF WINDOWS}windows,{$ENDIF}
  {$IFDEF UNIX}{$ifdef(UseCThreads)}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Graphics,
  lazcontrols,
  ConstsUnit, UtilsUnit, ImagesUnit, CommonUnit,
  IdentUnit, SplashUnit, MainUnit;

{$R *.res}
{$R Images.res}
{$I 'Defines.inc'}

//var
//  hMutex: THandle;

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;

  //hMutex := CreateMutex(nil, False, 'Nazir:MiniControl');
  {if  GetLastError = ERROR_ALREADY_EXISTS then
  begin
    MessageBox(0, 'Нельзя запускать вторую копию программы!',
               SWarning, MB_OK_EXCL);
    Halt(0);
  end;     //}

  // Основные цвета проекта
  clMain := clWhite;
  clMainBack := $00404080;
  clMainFore := $00000040;
  clMainFont := $0088C4FF;

  Application.Initialize;
  with TSplashForm.Create(Application) do
  try
    lbInitStatus.Caption := MSG_InitApp;
    Show;
    Application.HelpFile := '';
    Application.Title := 'MiniControl';
    AppTitle := 'MiniControl';
    AppCaption := GetFullProjectName;
    lbInitStatus.Caption := MSG_AvailReqFilesCheck;
    CheckNeededFiles(Handle);

    pbInitStatus.Position := pbInitStatus.Position + 10;

    lbInitStatus.Caption := MSG_LoadingImages;
    Application.CreateForm(TdmImages, dmImages);
    pbInitStatus.Position := pbInitStatus.Position + 10;

    lbInitStatus.Caption := MSG_LoadingCommonDecl;
    Application.CreateForm(TdmCommon, dmCommon);
    pbInitStatus.Position := pbInitStatus.Position + 10;

    lbInitStatus.Caption := MSG_InitModuleDBMS;
    pbInitStatus.Position := pbInitStatus.Position + 10;

    lbInitStatus.Caption := MSG_UserIdentification;
    CurrentUserID := 0;
    {$IFDEF LOGIN}
    siIdent.ChangeLogin;
    if CurrentUserID > 0 then
    begin
    {$ELSE}
      {$IFDEF DEBUG}
      CurrentUserRole := 'role_admin';
      {$ENDIF}
    {$ENDIF}
    pbInitStatus.Position := pbInitStatus.Position + 20;

     if (CurrentUserRole = 'role_admin') or (CurrentUserRole = 'role_owner') then
    begin
      lbInitStatus.Caption := MSG_InitMainWindow;
      Application.CreateForm(TMainForm, MainForm);
      pbInitStatus.Position := pbInitStatus.Position + 30;
      lbInitStatus.Caption := MSG_RunningApplication;
      pbInitStatus.Position := pbInitStatus.Max;
    end;
    {$IFDEF LOGIN}
    end
    else
      Application.Terminate;
    {$ENDIF}
  finally
    Free;
  end;
  if (CurrentUserRole = 'role_admin') or (CurrentUserRole = 'role_owner') then
    Application.Run
  else
    Application.MessageBox(PChar(MSG_YouHaveNoRights), MSG_Warning, MB_OK_EXCL);
  {$IFDEF LOGIN}
  siIdent.LogOut;
  {$ENDIF}
  //CloseHandle(hMutex);
end.

