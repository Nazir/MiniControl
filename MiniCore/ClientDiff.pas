unit ClientDiff;

{$mode objfpc}{$H+}

{******************************************************************************}
{                                                                              }
{ Unit: Модуль индивидуальных констант группы проектов                         }
{                                                                              }
{  Copyright: Nazir © 2002-2019                                                }
{  Development: Nazir K. Khusnutdinov (aka Naziron or Wild Pointer)            }
{  Разработчик: Хуснутдинов Назир Каримович                                    }
{  Email: naziron@gmail.com                                                    }
{  Git: https://github.com/Nazir                                               }
{                                                                              }
{  Modified: 09.12.2006                                                        }
{  Modified: 03.06.2009                                                        }
{  Modified: 12.03.2012                                                        }
{                                                                              }
{******************************************************************************}

interface

{$I Defines.inc}

const
  SClientID = // id клиента
{$IF Defined(Nazir)}
  '0000-0000-0000-2008-1108'
  {$MESSAGE HINT 'Компилим проект для Хуснутдинова Назира Каримовича"'}
  {$MESSAGE HINT '  ID клиента: 0000-0000-0000-2008-1102'}
{$ELSEIF Defined(OVVIO)}
  '0000-OVVI-OSY-2012-0101'
  {$MESSAGE HINT 'Компилим проект для Ovvio'}
  {$MESSAGE HINT '  ID клиента: 0000-OVVI-OSY-2012-0101'}
{$ELSE}
{$MESSAGE ERROR 'Выбери вначале организацию!'}
{$IFEND};

function SOranizationEssentialElements: string;

implementation

function SOranizationEssentialElements: string;
// Реквизиты организаций
begin
  Result :=
  {$IF Defined(Nazir)}
    'Хуснутдинов Назир Каримович'#13#10'WEB: nazir.pro'#13#10'Для демонстрации!'
    {$MESSAGE HINT ' реквизиты организации: Хуснутдинов Н.К.'}
  {$ELSEIF Defined(OVVIO)}
    'Ovvio Systems'#13#10'WEB: www.ovvio.pro'#13#10'E-mail: support@ovvio.pro'
    {$MESSAGE HINT ' реквизиты организации: Ovvio Systems'}
  {$ELSE}
    {$MESSAGE ERROR 'Выбери вначале организацию!'}
  {$IFEND};
end;

end.
