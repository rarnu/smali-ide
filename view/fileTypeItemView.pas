unit fileTypeItemView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, ExtCtrls, Graphics, Buttons;

type
  TOnFileTypeDelete = procedure (Sender: TObject; AType: string; AEditorPath: string) of object;
  TOnFileTypeSelect = function (Sender: TObject; AType: string; ACurrentEditor: string): string of object;

  { TFileTypeItemView }

  TFileTypeItemView = class(TPanel)
  private
    FFileTypeName: TLabel;
    FBtnEditor: TBitBtn;
    FBtnDelete: TBitBtn;
    FEditorPath: string;
    FOnFileTypeDelete: TOnFileTypeDelete;
    FOnFileTypeSelect: TOnFileTypeSelect;
    function GetEditor: string;
    function GetFileType: string;
    procedure innerDeleteClick(Sender: TObject);
    procedure innerEditorClick(Sender: TObject);
    procedure SetEditor(AValue: string);
    procedure SetFileType(AValue: string);
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property FileType: string read GetFileType write SetFileType;
    property Editor: string read GetEditor write SetEditor;
    property OnFileTypeDelete: TOnFileTypeDelete read FOnFileTypeDelete write FOnFileTypeDelete;
    property OnFileTypeSelect: TOnFileTypeSelect read FOnFileTypeSelect write FOnFileTypeSelect;
  end;

implementation

uses
  config, ThemeUtils;

{ TFileTypeItemView }

function TFileTypeItemView.GetFileType: string;
begin
  Result := FFileTypeName.Caption;
end;

procedure TFileTypeItemView.innerDeleteClick(Sender: TObject);
begin
  if (Assigned(FOnFileTypeDelete)) then begin
    FOnFileTypeDelete(Self, FFileTypeName.Caption, FEditorPath);
  end;
end;

procedure TFileTypeItemView.innerEditorClick(Sender: TObject);
begin
  if (Assigned(FOnFileTypeSelect)) then begin
    FEditorPath:= FOnFileTypeSelect(Self, FFileTypeName.Caption, FEditorPath);
    if (FEditorPath.Trim = '') then begin
      FBtnEditor.Caption:= '(none)';
    end else begin
      FBtnEditor.Caption:= ExtractFileName(FEditorPath);
    end;
  end;
end;

function TFileTypeItemView.GetEditor: string;
begin
  Result := FEditorPath;
end;

procedure TFileTypeItemView.SetEditor(AValue: string);
begin
  FEditorPath:= AValue;
  if (FEditorPath.Trim = '') then begin
    FBtnEditor.Caption:= '(none)';
  end else begin
    FBtnEditor.Caption:= ExtractFileName(FEditorPath);
  end;
end;

procedure TFileTypeItemView.SetFileType(AValue: string);
begin
  FFileTypeName.Caption:= AValue;
end;

constructor TFileTypeItemView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Align:= alTop;
  BevelInner:= bvRaised;
  BevelOuter:= bvLowered;
  BorderSpacing.Around:= 8;

  FFileTypeName := TLabel.Create(Self);
  FFileTypeName.Parent := Self;
  FFileTypeName.Align:= alLeft;
  FFileTypeName.Layout:= tlCenter;
  FFileTypeName.BorderSpacing.Around:= 8;

  FBtnDelete := TBitBtn.Create(Self);
  FBtnDelete.Parent := Self;
  FBtnDelete.Align:= alRight;
  FBtnDelete.BorderSpacing.Around:= 8;
  FBtnDelete.Caption:= 'X';
  FBtnDelete.Width:= 28;

  FBtnEditor := TBitBtn.Create(Self);
  FBtnEditor.Parent := Self;
  FBtnEditor.Align:= alRight;
  FBtnEditor.BorderSpacing.Around:= 8;
  FBtnEditor.Caption:= '(none)';
  FBtnEditor.Width:= 200;
  FBtnEditor.Left:= FBtnDelete.Left - FBtnEditor.Width;

  FBtnEditor.OnClick:=@innerEditorClick;
  FBtnDelete.OnClick:=@innerDeleteClick;

  ThemeUtils.RecolorButton(FBtnEditor);
  ThemeUtils.RecolorButton(FBtnDelete);
end;

end.

