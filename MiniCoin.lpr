program MiniCoin;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MC_Main, indylaz, master, CM_Crypto, commandlineparser, Protocol,
  blocks;

{$R *.res}

begin
  Application.Title:='Kreditz Wallet';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

