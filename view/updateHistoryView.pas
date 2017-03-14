unit updateHistoryView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Controls, Graphics, Dialogs, LCLIntf, SmaliIdeAPI, Buttons;

type

  { THistoryVersionView }

  THistoryVersionView = class(TPanel)
  private
    FDownloadUrl: string;
    FVersion: TLabel;
    FChangeLog: TBitBtn;
    FDownload: TBitBtn;
    FVersionDesc: string;
    procedure changelogClick(Sender: TObject);
    procedure downloadClick(Sender: TObject);
    function GetVersion: string;
    procedure SetVersion(AValue: string);
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property Version: string read GetVersion write SetVersion;
    property VersionDesc: string read FVersionDesc write FVersionDesc;
    property DownloadUrl: string read FDownloadUrl write FDownloadUrl;
  end;

implementation

{ THistoryVersionView }

function THistoryVersionView.GetVersion: string;
begin
  Result := FVersion.Caption;
end;

procedure THistoryVersionView.changelogClick(Sender: TObject);
begin
  if (FVersionDesc.Trim <> '') then begin
    MessageDlg('Version', FVersionDesc, mtInformation, [mbOK], 0);
  end;
end;

procedure THistoryVersionView.downloadClick(Sender: TObject);
begin
  LCLIntf.OpenURL(BASE_URL + FDownloadUrl);
end;

procedure THistoryVersionView.SetVersion(AValue: string);
begin
  FVersion.Caption:= AValue;
end;

constructor THistoryVersionView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Align:= alTop;
  BevelInner:= bvRaised;
  BevelOuter:= bvLowered;
  BorderSpacing.Around:= 8;

  FVersion := TLabel.Create(Self);
  FVersion.Parent := Self;
  FVersion.Align:= alLeft;
  FVersion.BorderSpacing.Around:= 8;
  FVersion.Layout:= tlCenter;

  FDownload := TBitBtn.Create(Self);
  FDownload.Parent := Self;
  FDownload.Align:= alRight;
  FDownload.BorderSpacing.Around:= 8;
  FDownload.Width:= 90;
  FDownload.Caption:= 'Download';

  FChangeLog := TBitBtn.Create(Self);
  FChangeLog.Parent := Self;
  FChangeLog.Align:= alRight;
  FChangeLog.BorderSpacing.Around:= 8;
  FChangeLog.Width:= 120;
  FChangeLog.Caption:= 'Change Log';
  FChangeLog.Left:= FDownload.Left - FChangeLog.Width;

  FDownload.OnClick:=@downloadClick;
  FChangeLog.OnClick:=@changelogClick;
end;

end.

