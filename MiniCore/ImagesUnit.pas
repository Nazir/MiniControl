unit ImagesUnit;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, ImgList, Controls;

type
  TdmImages = class(TDataModule)
    ilMainMenu16: TImageList;
    ilMainMenu32: TImageList;
    ilColumn: TImageList;
    procedure DataModuleCreate(Sender: TObject);
  private
  public
    ImadeIndex_OK, ImadeIndex_Cancel: Integer;
    ImadeIndex_Info, ImadeIndex_Warning, ImadeIndex_Error: Integer;
    ImadeIndex_Save, ImadeIndex_SaveAll: Integer;
    ImadeIndex_Refresh, ImadeIndex_Find, ImadeIndex_Edit, ImadeIndex_Delete: Integer;
  end;

var
  dmImages: TdmImages;

implementation

{$R *.lfm}
{$I Defines.inc}

procedure TdmImages.DataModuleCreate(Sender: TObject);
begin
  ImadeIndex_OK := 23;
  ImadeIndex_Cancel := 24;
  ImadeIndex_Info := 25;
  ImadeIndex_Warning := 26;
  ImadeIndex_Error := 27;
  ImadeIndex_Save := 13;
  ImadeIndex_SaveAll := 14;
  ImadeIndex_Edit := 6;
  ImadeIndex_Find := 41;
  ImadeIndex_Delete := 33;
  ImadeIndex_Refresh := 63;
end;

end.
