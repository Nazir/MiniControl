unit DataBaseUnit;

{$mode objfpc}{$H+}

{******************************************************************************}
{                                                                              }
{                           Database control module                            }
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
  Classes, SysUtils, db, sqldb, pqconnection;

type

  { TdmDataBase }

  TdmDataBase = class(TDataModule)
    dsMain: TDataSource;
    PQConnectionMain: TPQConnection;
    SQLQueryMain: TSQLQuery;
    SQLTransactionMain: TSQLTransaction;
  private

  public

  end;

var
  dmDataBase: TdmDataBase;

implementation

{$R *.lfm}

{ TdmDataBase }

end.

