unit SplashUnit;

{$mode objfpc}{$H+}

{******************************************************************************}
{                                                                              }
{ Unit: Splash Window Module                                                   }
{                                                                              }
{  Copyright: Nazir © 2002-2019                                                }
{  Development: Nazir K. Khusnutdinov (aka Naziron or Wild Pointer)            }
{  Разработчик: Хуснутдинов Назир Каримович                                    }
{  Email: naziron@gmail.com                                                    }
{  Git: https://github.com/Nazir                                               }
{                                                                              }
{  Created: 16.05.2019                                                         }
{  Modified: 16.05.2019                                                        }
{                                                                              }
{******************************************************************************}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, Classes, Forms, Controls, StdCtrls,
  ExtCtrls, SysUtils, Graphics, ComCtrls, ConstsUnit;

type
  TSplashForm = class(TForm)
    lbInitStatus: TLabel;
    imProgramLogo: TImage;
    lbCreators: TLabel;
    pbInitStatus: TProgressBar;
    lblProjectName: TLabel;
    lblVersion: TLabel;
    lblProductName: TLabel;
    Bevel: TBevel;
    procedure BevelChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure WMProgress(var Message: TMessage); message WM_NAZIR_PROGRESS;
    procedure FormShow(Sender: TObject);
  private
  public
    procedure SetInitStatus(AProgress: Integer; AText: string = '';
                            APositionStart: Integer = 0);
  end;

implementation

{$R *.lfm}
{$I Defines.inc}

procedure TSplashForm.FormCreate(Sender: TObject);
var
  rsTemp: TResourceStream;
//  Graph: TGraphic;
begin
  lblProjectName.Caption := GetFullProjectName;
  lblProductName.Caption := GetProductName + '™';
  lblVersion.Caption := GetFullFileVersion;

  with pbInitStatus do
  begin
    Color := clMain;
  end;

  try
    rsTemp := TResourceStream.Create(0,'PROGRAMLOGO', RT_RCDATA);
    imProgramLogo.Picture.PNG.LoadFromStream(rsTemp);
    {imProgramLogo.Canvas.LineTo(419, 0);
    imProgramLogo.Canvas.LineTo(419, 339);
    imProgramLogo.Canvas.LineTo(0, 339);
    imProgramLogo.Canvas.LineTo(0, 0);//}
    //imProgramLogo.Canvas.Rectangle(0, 0, imProgramLogo.Width, imProgramLogo.Height);
    //Graph := TGraphic.Create();
    //Graph.LoadFromStream(rsTemp);
    //imProgramLogo.Canvas.Draw(1, 1, Graph);
    //Graph.Free;
    rsTemp.Free;
  except
  end;
end;

procedure TSplashForm.BevelChangeBounds(Sender: TObject);
begin

end;

procedure TSplashForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
end;

procedure TSplashForm.WMProgress(var Message: TMessage);
begin
  if Message.WParam = -127 then
  begin
    SetInitStatus(0, string(Pointer(Message.LParam) { *Converted from Ptr* }^));
    Update;
  end
  else
  begin
    SetInitStatus(Message.WParam, string(Pointer(Message.LParam) { *Converted from Ptr* }^));
    Update;
  end;
end;

procedure TSplashForm.FormShow(Sender: TObject);
begin
  lbCreators.Caption := StringReplace(lbCreators.Caption, 'Copyright (c)', GetLegalCopyright, [rfReplaceAll, rfIgnoreCase]);
end;

procedure TSplashForm.SetInitStatus(AProgress: Integer; AText: string = '';
  APositionStart: Integer = 0);
begin
  lbInitStatus.Caption := Trim(AText);
  if APositionStart = 0 then
    pbInitStatus.Position := pbInitStatus.Position + AProgress
  else
    pbInitStatus.Position := APositionStart + AProgress;
  Update;
end;

end.
