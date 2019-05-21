unit CommonUnit;

{$mode objfpc}{$H+}

{******************************************************************************}
{                                                                              }
{ Unit: Common declarations                                                    }
{                                                                              }
{  Copyright: Nazir © 2002-2019                                                }
{  Development: Nazir K. Khusnutdinov (aka Naziron or Wild Pointer)            }
{  Разработчик: Хуснутдинов Назир Каримович                                    }
{  Email: naziron@gmail.com                                                    }
{  Git: https://github.com/Nazir                                               }
{                                                                              }
{  Modified: 13.05.2006                                                        }
{  Modified: 07.08.2009                                                        }
{  Modified: 16.06.2010                                                        }
{  Modified: 30.11.2011                                                        }
{  Modified: 16.04.2012                                                        }
{                                                                              }
{******************************************************************************}

interface

uses
  Classes, LCLIntf, LCLType, LMessages,
  Forms, SysUtils, StdCtrls, ComCtrls, PopupNotifier;

type

  { TdmCommon }

  TdmCommon = class(TDataModule)
    PopupNotifierMain: TPopupNotifier;
    procedure DataModuleCreate(Sender: TObject);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure CallHelp;
    //procedure GetCopyData(var AMessage: TWMCopyData);
    procedure StatusBarFill(var AStatusBar: TStatusBar);
  private
  public
  end;

  procedure ShowNotepad(AFileName: string; AOwner: TComponent = nil);

var
  dmCommon: TdmCommon;

implementation

uses
  ConstsUnit, MessageFormUnit, Controls, UtilsUnit;

{$R *.lfm}
{$I Defines.inc}

{ TdmCommon }

procedure TdmCommon.DataModuleCreate(Sender: TObject);
begin
  PopupNotifierMain.Title := MSG_Information;
end;

procedure TdmCommon.ApplicationEventsException(Sender: TObject;
  E: Exception);
begin
  SaveLogToFile('Application', E.Message, 'err');
  if E is EAccessViolation then
  begin
    ShowMessageForm('Критическая ошибка', 2, 'Желательно перезапустить программу!'#13#10 + E.Message);
    //MessageBox(0, PChar(),, MB_OK or MB_ICONERROR);
  end
  else
    ShowMessageForm('Ошибка', 2, E.Message);
end;

procedure TdmCommon.CallHelp;
var
  sTemp: string;
begin
  sTemp := ifConfig.ReadString('Help', AppExeName, 'Help.hlp');
  if FileExists(PathHelp + sTemp) then
     OpenDocument(PChar(PathHelp + sTemp)) { *Converted from ShellExecute* }
  else
    MessageBox(Application.MainForm.Handle, 'Файл со справкой не обнаружен!',
               MSG_Information, MB_OK_INFO);
end;

//procedure TdmCommon.GetCopyData(var AMessage: TWMCopyData);
//const
//  MAX_BUFFER = 65535;
//var
//  arrText: array [0..MAX_BUFFER] of Char;
//  sText: string;
//  msTemp: TMemoryStream;
//begin
//  sText := '';
//  with AMessage do
//  case CopyDataStruct.dwData of
//    0: // Receive Text, Text empfangen
//      begin
//        StrLCopy(arrText, CopyDataStruct.lpData, CopyDataStruct.cbData);
//        sText := string(arrText);
//        ServiceMsg.MsgType := Copy(sText, 1 , Pos('|', sText) - 1);
//        ServiceMsg.MsgText := Copy(sText, Pos('|', sText) + 1, Length(sText));
//      end;
//    1: // Receive Image, Bild empfangen
//      begin
//        msTemp := TMemoryStream.Create;
//        try
//          with CopyDataStruct^ do
//           msTemp.Write(lpdata^, cbdata);
//           msTemp.Position := 0;
//   //         image1.Picture.Bitmap.LoadFromStream(msTemp);
//        finally
//          msTemp.Free;
//        end;
//      end;
//  end;
//end;


procedure ShowNotepad(AFileName: string; AOwner: TComponent = nil);
var
  NotepadForm: TForm;
  mmNotepad: TMemo;
begin
  if not FileExists(AFileName) then
    Exit;

  if AOwner = nil then
    AOwner := Application.MainForm;

  NotepadForm := TForm.Create(AOwner); //}
  with NotepadForm do
  try
    Name := 'NotepadForm';
    Caption := 'Блокнот - ' + AFileName;
    Position := poOwnerFormCenter;
    //BorderStyle := bsSizeable;
    Width := 750;
    Height := 550;
    Update;
    mmNotepad := TMemo.Create(NotepadForm);
    mmNotepad.Parent := NotepadForm;
    mmNotepad.Align := alClient;
    mmNotepad.Name := 'mmNotepad';
    mmNotepad.ReadOnly := True;
    mmNotepad.Lines.LoadFromFile(AFileName);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TdmCommon.StatusBarFill(var AStatusBar: TStatusBar);
begin
  // <16.04.2012> WP Nazir
  with AStatusBar do
  begin
    Panels.Clear;
    SizeGrip := False;
    with Panels.Add do
    begin
      Text := Trim(CurrentDBConfig);
      Width := Canvas.TextWidth(Text + '      ');
    end;
    with Panels.Add do
    begin
      Text := 'Роль: ' + Trim(CurrentUserRoleCaption);
      Width := Canvas.TextWidth(Text + '      ');
    end;
    with Panels.Add do
    begin
      Text := 'Имя компьютера: ' + Trim(GetComputerNetName);
      Width := Canvas.TextWidth(Text + '      ');
    end;
    with Panels.Add do
    begin
      Text := 'Версия клиента: ' + GetFileVersion;
      Width := Canvas.TextWidth(Text + '      ');
    end;
    with Panels.Add do
    begin
      Text := 'Версия БД: ' + Trim(IntToStr(DBVersion));
      Width := Canvas.TextWidth(Text + '      ');
    end;
  end;
  // ^<16.04.2012> WP Nazir
end;

initialization

end.

