unit ExecutionFormUnit;

{$mode objfpc}{$H+}

{******************************************************************************}
{                                                                              }
{  Unit: Execution module                                                      }
{                                                                              }
{  Copyright: Nazir © 2002-2019                                                }
{  Development: Nazir K. Khusnutdinov (aka Naziron or Wild Pointer)            }
{  Разработчик: Хуснутдинов Назир Каримович                                    }
{  Email: naziron@gmail.com                                                    }
{  Git: https://github.com/Nazir                                               }
{                                                                              }
{  Modified: 12.04.2010, 19.11.2010                                            }
{  Modified: 14.04.2011                                                        }
{  Modified: 20.02.2012, 12.04.2012                                            }
{  Created: 17.05.2019 (Lazarus)                                               }
{  Modified: 17.05.2019                                                        }
{                                                                              }
{******************************************************************************}


interface

uses
  Forms, Classes, Controls, ExtCtrls, StdCtrls;

type
  TExecutionForm = class(TForm)
    AnimateConnect: TImage;
    lblInfo: TLabel;
    ShapeRange: TShape;
  private
  public
  end;

  procedure ExecutionFormShow(ACaption: string = ''; AShowModal: Boolean = False;
    AColor: Integer = -1; AOwner: TComponent = nil);
  procedure ExecutionFormClose;
  procedure ExecutionFormCaption(ACaption: string = ''; AColor: Integer = -1);

var
  ExecutionForm: TExecutionForm;

implementation

{$R *.lfm}
{$I Defines.inc}

{uses
  ConstsUnit; //}

procedure ExecutionFormShow(ACaption: string = ''; AShowModal: Boolean = False;
  AColor: Integer = -1; AOwner: TComponent = nil);
var
  CaptW: Integer; //}
begin
  if AOwner = nil then
    AOwner := Application;
  {if ExecutionForm = nil then
    ExecutionForm := TExecutionForm.Create(Application); //}
  if Assigned(ExecutionForm) then
  begin
    if ExecutionForm.Owner <> AOwner then
      ExecutionForm.Free;
  end;

  if not Assigned(ExecutionForm) then
    ExecutionForm := TExecutionForm.Create(AOwner); //}

  if AColor < 0 then
    AColor := 0;

  //<12.04.2010> WP Nazir
  with ExecutionForm do
  try
    Caption := 'Выполнение';
    ShapeRange.Pen.Color := AColor;
    Font.Color := ShapeRange.Pen.Color;
    if ACaption = '' then
      lblInfo.Caption := Caption
    else
      lblInfo.Caption := ACaption;
    CaptW := Canvas.TextWidth(ACaption);
    ClientWidth := CaptW + AnimateConnect.Width + 4*8;
    //AnimateConnect. Animate := True;
    Refresh;
    Repaint;
    if AShowModal then
      ShowModal
    else
    begin
      Show;
      BringToFront;
    end;
    Refresh;
    Application.ProcessMessages;
    Refresh;
  finally
  end;       //}
end;

procedure ExecutionFormClose;
begin
  if Assigned(ExecutionForm) then
  begin
  //  ExecutionForm.AnimateConnect.Animate := False;
    if ExecutionForm.Visible then
      ExecutionForm.Close;
//    ExecutionForm.Free;
//    ExecutionForm := nil;  //}
  end;
end;

procedure ExecutionFormCaption(ACaption: string = ''; AColor: Integer = -1);
var
  CaptW: Integer; //}
begin
  if Assigned(ExecutionForm) then
  begin
    //<14.04.2011> WP Nazir
    if AColor < 0 then
      AColor := 0;
    with ExecutionForm do
    try
      Caption := 'Выполнение';
      Font.Color := AColor;
      ShapeRange.Pen.Color := Font.Color;
      if ACaption = '' then
        lblInfo.Caption := Caption
      else
        lblInfo.Caption := ACaption;
      CaptW := Canvas.TextWidth(ACaption);
      ClientWidth := CaptW + AnimateConnect.Width + 4*8;
      //AnimateConnect.Animate := True;
      if CanFocus then
      begin
        Show;
        BringToFront;
      end;
      Repaint;
      Refresh;
      Application.ProcessMessages;
      Refresh;
    finally
    end;       //}
  end;
end;

end.
