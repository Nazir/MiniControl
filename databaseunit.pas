unit DataBaseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pqconnection, sqldb, db, PQTEventMonitor;

type

  { TdmDataBase }

  TdmDataBase = class(TDataModule)
    dsMain: TDataSource;
    PQConnectionMain: TPQConnection;
    PQTEventMonitorMain: TPQTEventMonitor;
    SQLQueryMain: TSQLQuery;
    SQLTransactionMain: TSQLTransaction;
  private

  public

  end;

var
  dmDataBase: TdmDataBase;

implementation

{$R *.lfm}

end.

