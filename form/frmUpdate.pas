unit frmUpdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, frmBase, SmaliIdeAPI, LCLIntf, Buttons;

type

  { TFormUpdate }

  TFormUpdate = class(TFormBase)
    btnUpdate: TBitBtn;
    gbLastVersion: TGroupBox;
    gbHistoryVersion: TGroupBox;
    lblLastVersion: TLabel;
    lblCurrentVersionValue: TLabel;
    lblCurrentVersion: TLabel;
    lblLastVersionValue: TLabel;
    mmDesc: TMemo;
    pnlUpdate: TPanel;
    pnlCurrentVersion: TPanel;
    pnlLastVersion: TPanel;
    sbxHistory: TScrollBox;
    procedure btnUpdateClick(Sender: TObject);
  private
    FUpdateInfo: TUpdateInfo;
    procedure SetUpdateInfo(AValue: TUpdateInfo);

  protected
    procedure InitComponents; override;
    procedure InitEvents; override;
    procedure InitLogic; override;
    procedure InitTheme; override;
  public
    property UpdateInfo: TUpdateInfo read FUpdateInfo write SetUpdateInfo;

  end;

var
  FormUpdate: TFormUpdate;

implementation

uses
  updateHistoryView, ThemeUtils;

{$R *.lfm}

{ TFormUpdate }

procedure TFormUpdate.btnUpdateClick(Sender: TObject);
begin
  if (FUpdateInfo.DownloadURL.Trim <> '') then begin
    LCLIntf.OpenURL(BASE_URL + FUpdateInfo.DownloadURL);
  end;
end;

procedure TFormUpdate.SetUpdateInfo(AValue: TUpdateInfo);
var
  his: TUpdateHistory;
  view: THistoryVersionView;
begin
  FUpdateInfo:=AValue;
  lblCurrentVersionValue.Caption:= IntToStr(FUpdateInfo.RequestVersion);
  lblLastVersionValue.Caption:= IntToStr(FUpdateInfo.LastVersion);
  mmDesc.Lines.Text:= FUpdateInfo.VersionDesc;
  btnUpdate.Enabled:= FUpdateInfo.RequestVersion <> FUpdateInfo.LastVersion;
  for his in FUpdateInfo.History do begin
    view := THistoryVersionView.Create(sbxHistory);
    view.Parent := sbxHistory;
    view.Version:= IntToStr(his.verCode);
    view.VersionDesc:= his.versionDesc;
    view.DownloadUrl:= his.downloadUrl;
  end;
end;

procedure TFormUpdate.InitComponents;
begin
  //
end;

procedure TFormUpdate.InitEvents;
begin
  //
end;

procedure TFormUpdate.InitLogic;
begin
  //
end;

procedure TFormUpdate.InitTheme;
begin
  ThemeUtils.RecolorButton(btnUpdate);
  ThemeUtils.RecolorMemo(mmDesc);
end;

end.

