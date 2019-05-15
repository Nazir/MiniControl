unit MainUnit;

{$mode objfpc}{$H+}

{******************************************************************************}
{                                                                              }
{                                Main unit                                     }
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

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  ComCtrls, DBCtrls, DBGrids, StdCtrls, PopupNotifier, ExtCtrls, PairSplitter,
  DataBaseUnit;

type

  { TMainForm }

  TMainForm = class(TForm)
    actConnect: TAction;
    alMain: TActionList;
    ApplicationProperties: TApplicationProperties;
    DBGridMain: TDBGrid;
    DBGroupBox1: TDBGroupBox;
    DBNavigator1: TDBNavigator;
    DBText1: TDBText;
    gbTree: TGroupBox;
    gbData: TGroupBox;
    ilMain: TImageList;
    ilGridTitles: TImageList;
    MenuItem2: TMenuItem;
    miFile: TMenuItem;
    mmMain: TMainMenu;
    PairSplitterMain: TPairSplitter;
    PairSplitterSideLeft: TPairSplitterSide;
    PairSplitterSideRight: TPairSplitterSide;
    PopupNotifierMain: TPopupNotifier;
    StatusBar1: TStatusBar;
    TrayIconMain: TTrayIcon;
    tvMain: TTreeView;
    procedure actConnectExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrayIconMainClick(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.actConnectExecute(Sender: TObject);
begin
  with dmDataBase do
  try
     with PQConnectionMain do
     try
        DatabaseName := 'minidb';
        UserName := 'dev';
        Password := 'dev';
        Role := 'role_dev';
        Connected := True;
        PopupNotifierMain.Text := 'Connection successfully!';
        PopupNotifierMain.Show;
     except
       ShowMessage('Connection error');
     end;
  finally
    if PQConnectionMain.Connected then
    begin
      with SQLQueryMain do
      try
         SQL.Text := 'SELECT * FROM public.v_country;';
         Active := True;
         DBText1.DataSource := dsMain;
         DBText1.DataField := 'name';
         //ExecSQL;
         PopupNotifierMain.Text := 'Query executed successfully!';
         PopupNotifierMain.Show;
      except
        ShowMessage('Query error');
      end;
      if Active then
      begin
        with DBGridMain do
        try
          Options := Options + [dgCellEllipsis, dgCellHints, dgDblClickAutoSize,
          dgDisplayMemoText];
          //Options2 := Options2 + [goScrollToLastCol, goScrollToLastRow];
        except
          ShowMessage('Grid error');
        end;
      end;
    end;
  end;

end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PopupNotifierMain.Title := 'Информация';
  //PopupNotifierMain.ShowAtPos(Self.Left + Self.Width, Self.Height);
end;

procedure TMainForm.TrayIconMainClick(Sender: TObject);
begin

end;

end.

