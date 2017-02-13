unit imageView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, ComCtrls, ExtCtrls, Graphics;

type

  { TImageCodeView }

  TImageCodeView = class(TTabSheet)
  private
    FImage: TImage;
    FFileName: string;
    procedure SetFileName(AValue: string);

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FileName: string read FFileName write SetFileName;
  end;

implementation

{ TImageCodeView }

procedure TImageCodeView.SetFileName(AValue: string);
begin
  FFileName:=AValue;
  Caption:= ExtractFileName(FFileName);
  // load image
  if (FileExists(FFileName)) then begin
    FImage.Picture.LoadFromFile(FFileName);
  end;
end;

constructor TImageCodeView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FImage := TImage.Create(Self);
  FImage.Parent := Self;
  FImage.Align:= alClient;
  FImage.Center:= True;
  FImage.Stretch:= False;
end;

destructor TImageCodeView.Destroy;
begin
  inherited Destroy;
end;

end.

