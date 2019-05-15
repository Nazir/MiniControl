program MiniControl;

{$mode objfpc}{$H+}

{******************************************************************************}
{                                                                              }
{  Project MiniControl                                                         }
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
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainUnit, DataBaseUnit;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TdmDataBase, dmDataBase);
  Application.Run;
end.

