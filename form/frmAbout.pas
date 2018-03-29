unit frmAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, frmBase, LCLIntf, Buttons;

type

  { TFormAbout }

  TFormAbout = class(TFormBase)
    bv: TBevel;
    btnOK: TBitBtn;
    imgIcon: TImage;
    lblSynEdit: TLabel;
    lblJadxGithub: TLabel;
    lblApkTool: TLabel;
    lblAck: TLabel;
    lblApkToolGithub: TLabel;
    lblAbout: TLabel;
    lblJadx: TLabel;
    lblsynEditGithub: TLabel;
    pnlApktool: TPanel;
    pnlJadx: TPanel;
    pnlButton: TPanel;
    pnlSynEdit: TPanel;
    pnlThanks: TPanel;
    pnlAbout: TPanel;
    procedure lblApkToolGithubClick(Sender: TObject);
    procedure lblJadxGithubClick(Sender: TObject);
    procedure lblsynEditGithubClick(Sender: TObject);
  private
  protected
    procedure InitComponents; override;
    procedure InitEvents; override;
    procedure InitLogic; override;
    procedure InitTheme; override;
  public

  end;

var
  FormAbout: TFormAbout;

implementation

uses
  ThemeUtils;

{$R *.frm}

{ TFormAbout }

procedure TFormAbout.lblApkToolGithubClick(Sender: TObject);
begin
  LCLIntf.OpenURL('https://github.com/iBotPeaches/Apktool');
end;

procedure TFormAbout.lblJadxGithubClick(Sender: TObject);
begin
  LCLIntf.OpenURL('https://github.com/skylot/jadx');
end;

procedure TFormAbout.lblsynEditGithubClick(Sender: TObject);
begin
  LCLIntf.OpenURL('https://github.com/SynEdit/SynEdit');
end;

procedure TFormAbout.InitComponents;
begin
  // about
end;

procedure TFormAbout.InitEvents;
begin
  //
end;

procedure TFormAbout.InitLogic;
begin
  //
end;

procedure TFormAbout.InitTheme;
begin
  ThemeUtils.RecolorButton(btnOK);
end;

end.

