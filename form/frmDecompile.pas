unit frmDecompile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, frmBase;

type

  { TFormDecompile }

  TFormDecompile = class(TFormBase)
    btnApkPath: TButton;
    btnOutputPath: TButton;
    btnOK: TButton;
    btnCanel: TButton;
    chkNoRes: TCheckBox;
    chkNoSrc: TCheckBox;
    edtApkPath: TEdit;
    edtOutputPath: TEdit;
    lblApkFile: TLabel;
    lblOutputPath: TLabel;
    pnlApkfile: TPanel;
    pnlOutputPath: TPanel;
    pnlOptions: TPanel;
    pnlButtons: TPanel;
    procedure btnApkPathClick(Sender: TObject);
    procedure btnOutputPathClick(Sender: TObject);
  private
  protected
    procedure InitComponents; override;
    procedure InitEvents; override;
    procedure InitLogic; override;
  public

  end;

var
  FormDecompile: TFormDecompile;

implementation

{$R *.lfm}

{ TFormDecompile }

procedure TFormDecompile.btnApkPathClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do begin
    Filter:= 'apk files|*.apk';
    if Execute then edtApkPath.Text := FileName;
    Free;
  end;
end;

procedure TFormDecompile.btnOutputPathClick(Sender: TObject);
begin
  with TSelectDirectoryDialog.Create(nil) do begin
    if Execute then edtOutputPath.Text := FileName;
    Free;
  end;
end;

procedure TFormDecompile.InitComponents;
begin
  //
end;

procedure TFormDecompile.InitEvents;
begin
  //
end;

procedure TFormDecompile.InitLogic;
begin
  //
end;

end.

