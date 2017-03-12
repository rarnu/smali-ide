unit frmBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type

  { TFormBase }

  TFormBase = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
  protected
    procedure InitComponents; virtual; abstract;
    procedure InitEvents; virtual; abstract;
    procedure InitLogic; virtual; abstract;
  public

  end;

var
  FormBase: TFormBase;

implementation

{$R *.lfm}

{ TFormBase }

procedure TFormBase.FormCreate(Sender: TObject);
begin
  AlphaBlend:= True;
  AlphaBlendValue:= 216;
  InitComponents;
  InitEvents;
  InitLogic;
end;

end.

