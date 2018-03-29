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
    procedure InitTheme; virtual; abstract;
  public

  end;

var
  FormBase: TFormBase;

implementation

uses
  config, ThemeUtils;

{$R *.frm}

{ TFormBase }

procedure TFormBase.FormCreate(Sender: TObject);
begin
  InitComponents;
  InitEvents;
  InitLogic;
  AlphaBlend:= GlobalConfig.Transparent;
  AlphaBlendValue:= GlobalConfig.Alpha;
  Color:= GlobalConfig.Color;
  Font.Color:= GlobalConfig.FontColor;
  Font.Name:= GlobalConfig.FontName;
  Font.Size:= GlobalConfig.FontSize;
  InitTheme;
end;

end.

