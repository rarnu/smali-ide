unit frmSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, frmBase, Types;

type

  { TFormSettings }

  TFormSettings = class(TFormBase)
    btnChooseJava: TButton;
    btnChooseCurl: TButton;
    edtJavaPath: TEdit;
    edtCurlPath: TEdit;
    gbJava: TGroupBox;
    gbCurl: TGroupBox;
    lblchooseJava: TLabel;
    lblChooseCurl: TLabel;
    lblDefaultCurl: TLabel;
    lblJavaStatus: TLabel;
    lblDefaultJava: TLabel;
    lblCurlStatus: TLabel;
    pnlCurlChoose: TPanel;
    pnlJavaDefault: TPanel;
    pnlJavaChoose: TPanel;
    pgSettings: TPageControl;
    pnlCurlDefault: TPanel;
    tsTemplate: TTabSheet;
    tsFileType: TTabSheet;
    tsHighlight: TTabSheet;
    tsApktool: TTabSheet;
    tsEnvironment: TTabSheet;
    tsShortcut: TTabSheet;
    procedure btnChooseCurlClick(Sender: TObject);
    procedure btnChooseJavaClick(Sender: TObject);
  private
  protected
    procedure InitComponents; override;
    procedure InitEvents; override;
    procedure InitLogic; override;
  public

  end;

var
  FormSettings: TFormSettings;

implementation

uses
  config;

{$R *.lfm}

{ TFormSettings }

procedure TFormSettings.btnChooseJavaClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do begin
    if Execute then begin
      edtJavaPath.Text:= FileName;
    end else begin
      edtJavaPath.Text:= '/usr/bin/java';
    end;
    Free;
  end;
  GlobalConfig.JavaBinaryPath:= edtJavaPath.Text;
end;

procedure TFormSettings.btnChooseCurlClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do begin
    if Execute then begin
      edtCurlPath.Text:= FileName;
    end else begin
      edtCurlPath.Text:= '/usr/bin/curl';
    end;
    Free;
  end;
  GlobalConfig.CurlBinaryPath:= edtCurlPath.Text;
end;

procedure TFormSettings.InitComponents;
begin
  // TODO: settings
end;

procedure TFormSettings.InitEvents;
begin
  //
end;

procedure TFormSettings.InitLogic;
begin
  // java
  if (FileExists('/usr/bin/java')) then begin
    lblJavaStatus.Caption:= '(exists)';
    lblJavaStatus.Font.Color:= clDefault;
  end else begin
    lblJavaStatus.Caption:= '(not exists)';
    lblJavaStatus.Font.Color:= clRed;
  end;
  edtJavaPath.Text:= GlobalConfig.JavaBinaryPath;

  // curl
  if (FileExists('/usr/bin/curl')) then begin
    lblCurlStatus.Caption:= '(exists)';
    lblCurlStatus.Font.Color:= clDefault;
  end else begin
    lblCurlStatus.Caption:= '(not exists)';
    lblCurlStatus.Font.Color:= clRed;
  end;
  edtCurlPath.Text := GlobalConfig.CurlBinaryPath;
end;

end.

