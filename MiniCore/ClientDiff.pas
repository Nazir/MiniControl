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
{$ELSEIF Defined(Client1)}
  '0000-CLNT-OSY-2018-0101'
  {$MESSAGE HINT 'Компилим проект для Client1'}
  {$MESSAGE HINT '  ID клиента: 0000-CLNT-OSY-2018-0101'}
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
  {$ELSEIF Defined(Client1)}
      'Client1 name'#13#10'WEB: '#13#10'E-mail: '
      {$MESSAGE HINT ' реквизиты организации: Client1'}
  {$ELSE}
    {$MESSAGE ERROR 'Выбери вначале организацию!'}
  {$IFEND};
end;

end.
