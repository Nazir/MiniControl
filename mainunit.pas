unit MainUnit;

{$mode objfpc}{$H+}

{******************************************************************************}
{                                                                              }
{  Unit: Main                                                                   }
{                                                                              }
{  Copyright: Nazir © 2002-2019                                                }
{  Development: Nazir K. Khusnutdinov (aka Naziron or Wild Pointer)            }
{  Разработчик: Хуснутдинов Назир Каримович                                    }
{  Email: naziron@gmail.com                                                    }
{  Git: https://github.com/Nazir                                               }
{                                                                              }
{  Created: 15.05.2019                                                         }
{  Modified: 15.05.2019                                                        }
{                                                                              }
{******************************************************************************}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  ComCtrls, DBCtrls, DBGrids, StdCtrls, ExtCtrls, PairSplitter,
  LvlGraphCtrl, TreeFilterEdit, DBUnit;

type

  { TMainForm }

  TMainForm = class(TForm)
    actConnect: TAction;
    alMain: TActionList;
    ApplicationProperties: TApplicationProperties;
    DBGrid1: TDBGrid;
    DBGridMain: TDBGrid;
    DBGroupBox1: TDBGroupBox;
    DBLookupComboBox1: TDBLookupComboBox;
    DBNavigator1: TDBNavigator;
    DBText1: TDBText;
    gbTree: TGroupBox;
    gbData: TGroupBox;
    ilMain: TImageList;
    ilGridTitles: TImageList;
    ilTreeView: TImageList;
    ilTreeViewState: TImageList;
    ilTreeViewSelected: TImageList;
    MenuItem2: TMenuItem;
    miFile: TMenuItem;
    mmMain: TMainMenu;
    PairSplitterMain: TPairSplitter;
    PairSplitterSideLeft: TPairSplitterSide;
    PairSplitterSideRight: TPairSplitterSide;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    TrayIconMain: TTrayIcon;
    tfeMain: TTreeFilterEdit;
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

uses GlobalsUnit;

{ TMainForm }

procedure TMainForm.actConnectExecute(Sender: TObject);
begin
  with dmDB do
  try
    Connect();
  finally
    if CheckConnected then
    begin
      with DataSetMain do
      try
         SQL.Text := 'SELECT * FROM public.v_country;';
         Active := True;
         DBText1.DataSource := dsMain;
         DBText1.DataField := 'name';
         //ExecSQL;
         //dmGlobals.PopupNotifierMain.Text := 'Query executed successfully!';
         //dmGlobals.PopupNotifierMain.Show;
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
  //PopupNotifierMain.ShowAtPos(Self.Left + Self.Width, Self.Height);
end;

procedure TMainForm.TrayIconMainClick(Sender: TObject);
begin

end;

end.

