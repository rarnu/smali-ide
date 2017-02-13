unit codeViewIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit;

type

  { ICodeViewIntf }

  ICodeViewIntf = interface
  ['{4C5C0112-8B04-4E89-BAA3-58F25686A7D2}']
  function QueryClose(): Boolean;
  procedure Save();
  procedure SaveAs();
  procedure Find();
  procedure FindNext();
  procedure CancelFind();
  procedure Replace();
  procedure CancelReplace();
  procedure GotoLine(line: Integer);
  function GetEditor(): TSynEdit;
  function GetFileName(): string;
  procedure Free;   // hidden method from TObject
  end;

implementation

end.

