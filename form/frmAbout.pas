unit frmAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, frmBase;

type

  { TFormAbout }

  TFormAbout = class(TFormBase)
  private
  protected
    procedure InitComponents; override;
    procedure InitEvents; override;
    procedure InitLogic; override;
  public

  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}

{ TFormAbout }

procedure TFormAbout.InitComponents;
begin
  // TODO: about
end;

procedure TFormAbout.InitEvents;
begin
  //
end;

procedure TFormAbout.InitLogic;
begin
  //
end;

end.

