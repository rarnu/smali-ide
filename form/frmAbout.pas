unit frmAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, frmBase;

type

  { TFormAbout }

  TFormAbout = class(TFormBase)
    Bevel1: TBevel;
    btnOK: TButton;
    imgIcon: TImage;
    lblAbout: TLabel;
    pnlButton: TPanel;
    pnlThanks: TPanel;
    pnlAbout: TPanel;
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

