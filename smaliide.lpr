program smaliide;

{$mode objfpc}{$H+}

uses
  {$IFNDEF WINDOWS} cthreads, {$ENDIF}
  Interfaces,
  Forms, frmMain, smaliCodeView, TextUtils, CodeUtils,
ProjectUtils, EncryptUtils, textCodeView, codeViewIntf, imageView, 
SearchInFileUtils, CommandUtils, frmDecompile, baseData;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

