unit frmJava;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  frmBase, SynEdit, SynHighlighterJava;

type

  { TFormJava }

  TFormJava = class(TFormBase)
    miSaveToFile: TMenuItem;
    popJava: TPopupMenu;
    synJavaCode: TSynEdit;
    synJava: TSynJavaSyn;
    procedure miSaveToFileClick(Sender: TObject);
  private
    function GetCode: string;
    procedure SetCode(AValue: string);
  protected
    procedure InitComponents; override;
    procedure InitEvents; override;
    procedure InitLogic; override;
  public
    property Code: string read GetCode write SetCode;
  end;

var
  FormJava: TFormJava;

implementation

{$R *.lfm}

{ TFormJava }

procedure TFormJava.miSaveToFileClick(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do begin
    Filter:= 'java file|*.java';
    if Execute then synJavaCode.Lines.SaveToFile(FileName);
    Free;
  end;
end;

function TFormJava.GetCode: string;
begin
  Result := synJavaCode.Lines.Text;
end;

procedure TFormJava.SetCode(AValue: string);
begin
  synJavaCode.Lines.Text:= AValue;
end;

procedure TFormJava.InitComponents;
begin
  //
end;

procedure TFormJava.InitEvents;
begin
  //
end;

procedure TFormJava.InitLogic;
begin
  //
end;

end.

