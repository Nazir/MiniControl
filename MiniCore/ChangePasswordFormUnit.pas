unit ChangePasswordFormUnit;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TChangePasswordForm = class(TForm)
    leOldPassword: TLabeledEdit;
    leNewPassword: TLabeledEdit;
    leNewPasswordConfirm: TLabeledEdit;
    btOK: TBitBtn;
    btCancel: TBitBtn;
    chbxProtected: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TextChange(Sender: TObject);
  private
  public
  end;

implementation

{$I Defines.inc}

{$R *.lfm}

uses
  ConstsUnit, ImagesUnit;

procedure TChangePasswordForm.FormCreate(Sender: TObject);
begin
  btOK.Images := dmImages.ilMainMenu16;
  btOK.ImageIndex := dmImages.ImadeIndex_OK;
  btCancel.ImageIndex := dmImages.ImadeIndex_Cancel;
  btCancel.Images := dmImages.ilMainMenu16;

  chbxProtected.Hide;
  chbxProtected.Checked := False;
  {$IFDEF PASSWORD_PROTECT}
  chbxProtected.Show;
  chbxProtected.Checked := True;
  {$ENDIF}
end;

procedure TChangePasswordForm.btOKClick(Sender: TObject);
begin
  if Trim(leOldPassword.Text) = EmptyStr then
  begin
    MessageBox(Self.Handle, 'Необходимо ввести старый пароль.',
               MSG_Information, MB_OK_INFO);
    leOldPassword.SetFocus;
    leOldPassword.Clear;
    Exit;
  end;
  if Trim(leNewPassword.Text) = EmptyStr then
  begin
    MessageBox(Self.Handle, 'Необходимо ввести новый пароль.',
               MSG_Information, MB_OK_INFO);
    leNewPassword.SetFocus;
    leNewPassword.Clear;
    Exit;
  end;
  if Trim(leNewPasswordConfirm.Text) = EmptyStr then
  begin
    MessageBox(Self.Handle, 'Необходимо ввести подтверждение нового пароля.',
               MSG_Information, MB_OK_INFO);
    leNewPasswordConfirm.SetFocus;
    leNewPasswordConfirm.Clear;
    Exit;
  end;
  if leNewPassword.Text <> leNewPasswordConfirm.Text then
  begin
    MessageBox(Handle, 'Пароль и подтверждение не совпадают!',
               MSG_Information, MB_OK_WARN);
    Exit;
  end;

  ModalResult := mrOK;
end;

procedure TChangePasswordForm.FormShow(Sender: TObject);
begin
  if leOldPassword.Text <> EmptyStr then
    leNewPassword.SetFocus
  else
    leOldPassword.Clear;

  leNewPassword.Clear;
  leNewPasswordConfirm.Clear;
end;

procedure TChangePasswordForm.TextChange(Sender: TObject);
begin
  with TLabeledEdit(Sender) do
  begin
    if Trim(Text) = EmptyStr then
    begin
      Color := clObligatoryBack;
      Font.Color := clObligatoryFont;
    end
    else
    begin
      Color := clWindow;
      Font.Color := clWindowText;
    end;
  end;
end;

end.
