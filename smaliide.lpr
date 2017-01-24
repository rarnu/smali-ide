program smaliide;

{$mode objfpc}{$H+}

uses
  {$IFNDEF WINDOWS} cthreads, {$ENDIF}
  Interfaces,
  Forms, frmMain, projectUtils;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

