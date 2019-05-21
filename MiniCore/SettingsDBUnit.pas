unit SettingsDBUnit;

{$mode objfpc}{$H+}

{******************************************************************************}
{                                                                              }
{  Unit: Module of the DB settings window                                      }
{                                                                              }
{  Copyright: Nazir © 2002-2019                                                }
{  Development: Nazir K. Khusnutdinov (aka Naziron or Wild Pointer)            }
{  Разработчик: Хуснутдинов Назир Каримович                                    }
{  Email: naziron@gmail.com                                                    }
{  Git: https://github.com/Nazir                                               }
{                                                                              }
{  Modified: 26.05.2007                                                        }
{  Modified: 03.06.2009, 17.07.2009                                            }
{  Modified: 19.10.2011                                                        }
{  Modified: 16.02.2012                                                        }
{  Modified: 15.05.2019 (Lazarus)                                              }
{                                                                              }
{******************************************************************************}

interface

uses
  LCLIntf, LCLType, LMessages, Classes, ActnList, Controls, StdCtrls,
  Buttons, ExtCtrls, Forms, SysUtils, Dialogs, ButtonPanel, Spin;

type

  { TSettingsDBForm }

  TSettingsDBForm = class(TForm)
    bpSettingsDB: TButtonPanel;
    gbGeneralDB: TGroupBox;
    ActionList1: TActionList;
    acHelp: TAction;
    lblPort: TLabel;
    leDBName: TLabeledEdit;
    lbDBList: TListBox;
    acLoadConfig: TAction;
    acSaveConfig: TAction;
    pnDBConfig: TPanel;
    btSaveConfig: TBitBtn;
    leDBConfig: TLabeledEdit;
    btDeleteConfig: TBitBtn;
    acDeleteConfig: TAction;
    acRefreshDBList: TAction;
    cbxServerNameDB: TComboBox;
    lblServerNameDB: TLabel;
    OpenDialog: TOpenDialog;
    sePort: TSpinEdit;
    procedure btCancelClick(Sender: TObject);
    procedure btOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure acHelpExecute(Sender: TObject);
    procedure acDBExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbxServerDBChange(Sender: TObject);
    procedure cbxServerDBSelect(Sender: TObject);
    procedure leDBNameChange(Sender: TObject);
    procedure cbxServerNameDBChange(Sender: TObject);
    procedure sePortChange(Sender: TObject);
    procedure leDBBackUpPathChange(Sender: TObject);
    procedure lbDBListClick(Sender: TObject);
    procedure acLoadConfigExecute(Sender: TObject);
    procedure acSaveConfigExecute(Sender: TObject);
    procedure acDeleteConfigExecute(Sender: TObject);
    procedure acRefreshDBListExecute(Sender: TObject);
    procedure acDBClientLibraryExecute(Sender: TObject);
    procedure pnDBConfigClick(Sender: TObject);
  private
    sDB: string;
  public

  end;

  function ShowSettingsDBForm(ADBConfig: string; AOwner: TComponent): Boolean;

const
  CaptionsCount = 1;

var
  DBSettingsChanged: Boolean = False;

implementation

uses
  ConstsUnit, UtilsUnit, ImagesUnit;

{$R *.lfm}
{$I Defines.inc}

function ShowSettingsDBForm(ADBConfig: string; AOwner: TComponent): Boolean;
begin
  Result := False;
  with TSettingsDBForm.Create(AOwner) do
  try
    sDB := Trim(ADBConfig);
    if ShowModal = mrOK then
    begin
      Result := DBSettingsChanged;
    end;

  finally
    Free;
  end;
end;

procedure TSettingsDBForm.FormCreate(Sender: TObject);
begin
  dmImages.ilMainMenu16.GetIcon(84, Icon);

  with bpSettingsDB do
  begin
    HelpButton.Caption := '&Помощь';
    CloseButton.Caption := '&Закрыть';
    CancelButton.Caption := '&Отмена';
    ShowButtons := ShowButtons - [pbHelp, pbClose];
  end;
end;

procedure TSettingsDBForm.btOkClick(Sender: TObject);
begin

end;

procedure TSettingsDBForm.btCancelClick(Sender: TObject);
begin

end;

procedure TSettingsDBForm.FormShow(Sender: TObject);
begin
  cbxServerNameDB.Clear;
  with cbxServerNameDB.Items do
  begin
    //Add('localhost');
    //Add('127.0.0.1');
    Text := Trim(LoadTextFromFile(PathSys + 'DB_Servers.lst'));
  end;
  sePort.Value := 5432;
  acRefreshDBList.Execute;
  DBSettingsChanged := False;
end;

procedure TSettingsDBForm.acHelpExecute(Sender: TObject);
begin
//
end;

procedure TSettingsDBForm.acDBExecute(Sender: TObject);
begin
end;

procedure TSettingsDBForm.cbxServerDBChange(Sender: TObject);
begin
  cbxServerDBSelect(Sender);
end;

procedure TSettingsDBForm.cbxServerDBSelect(Sender: TObject);
begin
  leDBNameChange(Sender);
  DBSettingsChanged := True;
end;

procedure TSettingsDBForm.leDBNameChange(Sender: TObject);
begin
  DBSettingsChanged := True;
end;

procedure TSettingsDBForm.cbxServerNameDBChange(Sender: TObject);
begin
  DBSettingsChanged := True;
end;

procedure TSettingsDBForm.sePortChange(Sender: TObject);
begin
  DBSettingsChanged := True;
end;

procedure TSettingsDBForm.leDBBackUpPathChange(Sender: TObject);
begin
  DBSettingsChanged := True;
end;

procedure TSettingsDBForm.lbDBListClick(Sender: TObject);
begin
  if lbDBList.ItemIndex > -1 then
    sDB := lbDBList.Items.Strings[lbDBList.ItemIndex];
  leDBConfig.Text := sDB;
  acLoadConfig.Execute;
end;

procedure TSettingsDBForm.acLoadConfigExecute(Sender: TObject);
begin
  if Trim(sDB) = EmptyStr then
    Exit;
  //dmDB.LoadDBSettings;

  with ifDB do
  begin
    cbxServerNameDB.Text := ReadString(sDB, 'ServerNameDB', '127.0.0.1');
    sePort.Value := ReadInteger(sDB, 'PortDB', 5432);
    leDBName.Text := ReadString(sDB, 'DBName', EmptyStr);
  end;
  cbxServerDBChange(Sender);
end;

procedure TSettingsDBForm.acSaveConfigExecute(Sender: TObject);
begin
  sDB := leDBConfig.Text;
  with ifDB do
  begin
    WriteString(sDB, 'ServerNameDB', cbxServerNameDB.Text);
    WriteString(sDB, 'PortDB', sePort.Text);
    WriteString(sDB, 'DBName', leDBName.Text);
    UpdateFile;
  end;
  acRefreshDBList.Execute;
  DBSettingsChanged := True;
end;

procedure TSettingsDBForm.acDeleteConfigExecute(Sender: TObject);
begin
  if lbDBList.Items.Count < 2 then
  begin
    MessageBox(Self.Handle, PChar('Нельзя удалить последнюю конфигурацию!'#13#10 +
               'Минимальное количество конфигураций равно 1'),
               MSG_Information, MB_OK_INFO);
    Exit;
  end;

  if MessageBox(Self.Handle,'Удалить конфигурацию?', MSG_Confirmation,
    MB_YESNO or MB_ICONQUESTION) <> IDYES then
    Exit;

  ifDB.EraseSection(sDB);
  lbDBList.DeleteSelected;

  acRefreshDBList.Execute;
  DBSettingsChanged := True;
end;

procedure TSettingsDBForm.acRefreshDBListExecute(Sender: TObject);
begin
  if Trim(sDB) = EmptyStr then
    sDB := ifConfig.ReadString('Defaults', 'sDB', 'local');
  with ifDB do
  begin
    ReadSections(lbDBList.Items);
    if lbDBList.Count > 0 then
    begin
      lbDBList.ItemIndex := 0;
      if lbDBList.Items.IndexOf(sDB) > -1 then
        lbDBList.ItemIndex := lbDBList.Items.IndexOf(sDB);
    end;
    {if lbDBList.Count > 0 then
      lbDBList.ItemIndex := 0;}

    lbDBListClick(Sender);
  end;
end;

procedure TSettingsDBForm.acDBClientLibraryExecute(Sender: TObject);
begin
end;

procedure TSettingsDBForm.pnDBConfigClick(Sender: TObject);
begin

end;

end.
