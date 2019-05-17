unit MessageFormUnit;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ImgList, ExtCtrls, StdCtrls, ActnList, Buttons;

type
  TMessageForm = class(TForm)
    ilIcons: TImageList;
    ActionList1: TActionList;
    acIntelligenceClick: TAction;
    pnInfo: TPanel;
    mmInfo: TMemo;
    pnTop: TPanel;
    imIcon: TImage;
    lbMsg: TLabel;
    btIntelligence: TBitBtn;
    acCloseApplication: TAction;
    BevelInfo: TBevel;
    lbInfo: TLabel;
    cbWordWrap: TCheckBox;
    btCloseApplication: TBitBtn;
    pnInfoTop: TPanel;
    btCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure acIntelligenceClickExecute(Sender: TObject);
    procedure acCloseApplicationExecute(Sender: TObject);
    procedure cbWordWrapClick(Sender: TObject);
  private
    bIntelligence: Boolean;
    FullClientHeight: Integer;
  public
  end;

procedure ShowMessageForm(AMsg: string; AMsgType: Integer = 0; AMsgFull: string = '';
                          AOwner: TComponent = nil; AFormCaption: string = '');

implementation

uses ImagesUnit;

{$R *.lfm}

procedure ShowMessageForm(AMsg: string; AMsgType: Integer = 0; AMsgFull: string = '';
                          AOwner: TComponent = nil; AFormCaption: string = '');
begin
  if AOwner = nil then
    AOwner := Application;

  with TMessageForm.Create(AOwner) do
  try
    FormStyle := fsStayOnTop;
    bIntelligence := False;
    FullClientHeight := 0;
    acCloseApplication.Visible := False;
    case AMsgType of
      0:
      begin
        Caption := 'Информация';
        ilIcons.GetIcon(0, imIcon.Picture.Icon);
        ilIcons.GetIcon(0, Icon);
        mmInfo.ParentFont := True;
        beep();
      end;
      1:
      begin
        Caption := 'Предупреждение!';
        ilIcons.GetIcon(1, imIcon.Picture.Icon);
        ilIcons.GetIcon(1, Icon);
        acCloseApplication.Visible := True;
        beep();
      end;
      2:
      begin
        Caption := 'Ошибка!';
        ilIcons.GetIcon(2, imIcon.Picture.Icon);
        ilIcons.GetIcon(2, Icon);
        lbMsg.Font.Color := clRed;
        acCloseApplication.Visible := True;
        beep();
      end;
    else
      Caption := 'Сообщение';
      ilIcons.GetIcon(0, imIcon.Picture.Icon);
    end;

    lbMsg.Caption := AMsg;

    mmInfo.Text := Trim(AMsgFull);
    if mmInfo.Text = EmptyStr then
       btIntelligence.Hide;
    acIntelligenceClick.Execute;

    if Trim(AFormCaption) <> '' then
      Caption := AFormCaption;
    //Repaint;
    Constraints.MinWidth := 300;
    Constraints.MinHeight := pnTop.Height;

    Update;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMessageForm.FormCreate(Sender: TObject);
begin
  btCancel.ImageIndex := dmImages.ImadeIndex_Cancel;
  btCancel.Images := dmImages.ilMainMenu16;
end;

procedure TMessageForm.acIntelligenceClickExecute(Sender: TObject);
begin
  if mmInfo.Text = EmptyStr then
    bIntelligence := False
  else
    bIntelligence := not bIntelligence;
    
  if bIntelligence then
  begin
    //Constraints.MaxHeight := 0;
    BorderStyle := bsSizeable;  // <09.11.2009>
    if FullClientHeight = 0 then
      FullClientHeight := ClientHeight;
    ClientHeight := FullClientHeight;
    btIntelligence.Caption := 'Сведения <<';
    lbMsg.AutoSize := False;
    lbMsg.Anchors := lbMsg.Anchors + [akRight];
    Width := 400;
    pnInfo.Show;
    //BorderIcons := BorderIcons + [biMaximize];
    //BorderStyle := bsSizeable;
  end
  else
  begin
    FullClientHeight := ClientHeight;
    pnInfo.Hide;
    ClientHeight := pnTop.Height;
    //Constraints.MaxHeight := Height;
    BorderStyle := bsSingle; // <09.11.2009>
    btIntelligence.Caption := 'Сведения >>';

    lbMsg.Anchors := lbMsg.Anchors - [akRight];
    lbMsg.AutoSize := True;
    ClientWidth := lbMsg.Width + imIcon.Width + 30;
    if ClientWidth < 300 then
      ClientWidth := 300;
    if Width > 400 then
      Width := 400;
    //BorderIcons := BorderIcons - [biMaximize];
    //BorderStyle := bsDialog;
  end;
end;

procedure TMessageForm.acCloseApplicationExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMessageForm.cbWordWrapClick(Sender: TObject);
begin
  mmInfo.WordWrap := cbWordWrap.Checked;
  if mmInfo.WordWrap then
    mmInfo.ScrollBars := ssVertical
  else
    mmInfo.ScrollBars := ssBoth;
end;

end.
