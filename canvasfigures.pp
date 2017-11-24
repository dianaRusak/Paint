unit CanvasFigures;

{$MODE OBJFPC}
{$LONGSTRINGS ON}

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
interface
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

uses
  Classes, SysUtils, Graphics, GraphMath, math,
  Transform;

const
  cFigureIndexInvalid = -1;

type
  TPointArray = array of TPoint;

	{ TCanvasFigure }

  TCanvasFigure = class
  strict protected
    fPoints: array of TFloatPoint;
    fWidth: Integer;
    fPenStyle: TPenStyle;
    fPenColor: TColor;
    fBrushStyle: TBrushStyle;
    fBrushColor: TColor;
    fTransparent: Boolean;
    fRadius: integer;
  public
    Selected : boolean;
    function TopLeft():TFloatPoint;
    procedure DrawSelection (aCanvas: TCanvas);
    function BottomRight():TFloatPoint;
    procedure AddPoint(aValue: TFloatPoint);
    function GetPoint(aIndex: SizeInt): TFloatPoint;
    function GetCanvasPoints(): TPointArray;
    procedure SetPoint(aIndex: SizeInt; aValue: TFloatPoint);
    function PointsCount(): SizeInt;
    function RectInside(RectLeft, RectRight: TFloatPoint): boolean;
    procedure Draw(aCanvas: TCanvas); virtual;
    property Radius: integer read fRadius write fRadius;
    property Width: Integer read fWidth write fWidth;
    property PenStyle: TPenStyle read fPenStyle write fPenStyle;
    property PenColor: TColor read fPenColor write fPenColor;
    property BrushStyle: TBrushStyle read fBrushStyle write fBrushStyle;
    property BrushColor: TColor read fBrushColor write fBrushColor;
    property Transparent: Boolean read fTransparent write fTransparent;
  end;

	{ TFigurePen }

  TFigurePen = class(TCanvasFigure)
  public
    procedure Draw(aCanvas: TCanvas); override;
  end;

	{ TFigureLine }

  TFigureLine = class(TCanvasFigure)
  public
    procedure Draw(aCanvas: TCanvas); override;
  end;

	{ TFigurePolyline }

  TFigurePolyline = class(TCanvasFigure)
  public
    procedure Draw(aCanvas: TCanvas); override;
  end;

	{ TFigureRectangle }

  TFigureRectangle = class(TCanvasFigure)
  public
    procedure Draw(aCanvas: TCanvas); override;
  end;

	{ TFigureEllipse }

  TFigureEllipse = class(TCanvasFigure)
  public
    procedure Draw(aCanvas: TCanvas); override;
  end;

	{ FFigureEmpty }

  FFigureEmpty = class(TCanvasFigure)
  public
    procedure Draw(aCanvas: TCanvas); override;
	end;

	{ TFigureRoundRect }

  TFigureRoundRect = class(TCanvasFigure)
  public
    procedure Draw(aCanvas: TCanvas); override;
	end;

	{ FFigureAllotment }

  FFigureAllotment = class(TCanvasFigure)
  public
    procedure Draw(aCanvas: TCanvas); override;
	end;

  TCanvasFigureClass = class of TCanvasFigure;

////////////////////////////////////////////////////////////////////////////////////////////////////

function AddFigure(aFigureClass: TCanvasFigureClass): SizeInt;
procedure DeleteFigure(aIndex: SizeInt);
procedure ClearFigures();
function GetFigure(aIndex: SizeInt): TCanvasFigure;
function FiguresCount(): SizeInt;
procedure PSelectAll();
procedure UnSelectAll();
procedure DeleteSelected();

var
  beginingRun: Boolean;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
implementation
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

// данные фигур и функции для их добавления/удаления, а также получения конкретной фигуры

uses
  EditorTools;

var
  FiguresData: array of TCanvasFigure;
  aMin, aMax: TFloatPoint;
  count: SizeInt;

function AddFigure(aFigureClass: TCanvasFigureClass): SizeInt;
begin
  Result := Length(FiguresData);
  SetLength(FiguresData, Result+1);
  FiguresData[Result] := aFigureClass.Create();
end;

procedure DeleteFigure(aIndex: SizeInt);
begin
  FreeAndNil(FiguresData[aIndex]);
  SetLength(FiguresData, Length(FiguresData)-1);
end;

procedure ClearFigures();
var
  Figure: TCanvasFigure;
begin
  for Figure in FiguresData do
    Figure.Destroy();
  SetLength(FiguresData, 0);
end;

function GetFigure(aIndex: SizeInt): TCanvasFigure;
begin
  if Length(FiguresData) = 0 then
    Result := nil
  else
    Result := FiguresData[aIndex];
end;

function FiguresCount(): SizeInt;
begin
  Result := Length(FiguresData);
end;

procedure PSelectAll;
begin
  aMax.x := MaxInt;
  aMax.y := MaxInt;
  aMin.x := 5 - MaxInt;
  aMin.y := 5 - MaxInt;
end;

procedure UnSelectAll;
begin
  aMax.x := 5 - MaxInt;
  aMax.y := 5 - MaxInt;
  aMin.x := MaxInt;
  aMin.y := MaxInt;
end;

procedure DeleteSelected;
var
  i, j, k: SizeInt;
begin
  j := 0;
  for i:= Low(FiguresData) to High (FiguresData) do
    if (FiguresData[i] <> nil) and (FiguresData[i].selected) then
    begin
      FreeAndNil(FiguresData[i]);
      Inc(j);
		end;
  for k := 1 to j do
    for i:= Low(FiguresData) to High (FiguresData) do
      if (FiguresData = nil) and (i + 1 < Length(FiguresData)) then
      begin
        FiguresData[i] := FiguresData[i+1];
        FiguresData[i+1] := nil;
	   	end;
  SetLength(FiguresData, Length(FiguresData) - j);
end;

{ FFigureAllotment }

procedure FFigureAllotment.Draw(aCanvas: TCanvas);
var
  cPoints: TPointArray;
begin

  cPoints := GetCanvasPoints();
  with aCanvas.Pen do
  begin
    Color := clPurple;
    Width := 1;
    Style := psDash;
	end;
  aCanvas.Brush.Style := bsClear;
  aCanvas.Rectangle(cPoints[0].x, cPoints[0].y, cPoints[1].x, cPoints[1].y);
  aMin.x := Min(cPoints[0].x, cPoints[1].x);
  aMin.y := Min(cPoints[0].y, cPoints[1].y);
  aMax.x := Max(cPoints[0].x, cPoints[1].x);
  aMax.y := Max(cPoints[0].y, cPoints[1].y);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TCanvasFigure
////////////////////////////////////////////////////////////////////////////////////////////////////


function TCanvasFigure.TopLeft: TFloatPoint;
var
  i: TFloatPoint;
begin
  Result := fPoints[0];
  for i in fPoints do
    begin
      Result.x := min (Result.x, i.x);
      Result.y := min (Result.y, i.y);
		end;
end;

procedure TCanvasFigure.DrawSelection(aCanvas: TCanvas);
var
  FigureLeft, FigureRight: TPoint;
begin
  if (Selected) and (beginingRun) then
  begin
  	FigureLeft := WorldToScreen(TopLeft.x, TopLeft.y);
  	FigureRight := WorldToScreen(BottomRight.x, BottomRight.y);
    with aCanvas.Pen do
	  begin
	    Color := clPurple;
	    Width := 1;
	    Style := psDash;
		end;
    aCanvas.Brush.Style := bsClear;
	  aCanvas.Rectangle(FigureLeft.X - (fWidth div 2)-3, FigureLeft.Y - (fWidth div 2)-3, FigureRight.X +
    (fWidth div 2)+3, FigureRight.Y + (fWidth div 2) +3);
	end;
end;

function TCanvasFigure.BottomRight: TFloatPoint;
var
  i: TFloatPoint;
begin
  Result := fPoints[0];
  for i in fPoints do
    begin
      Result.x := max (Result.x, i.x);
      Result.y := max (Result.y, i.y);
		end;
end;

procedure TCanvasFigure.AddPoint(aValue: TFloatPoint);
var
  Len: SizeInt;
begin
  Len := Length(fPoints);
  SetLength(fPoints, Len+1);
  fPoints[Len] := aValue;
end;

function TCanvasFigure.GetPoint(aIndex: SizeInt): TFloatPoint;
begin
  Result := fPoints[aIndex];
end;

function TCanvasFigure.GetCanvasPoints: TPointArray;
var
  CanvasPoints: TPointArray;
  i: integer;
begin
  SetLength(CanvasPoints, Length(fPoints));
  for i := 0 to Length(fPoints)-1 do
    CanvasPoints[i] := WorldToScreen(fPoints[i].x, fPoints[i].y);
  Result := CanvasPoints;
end;

procedure TCanvasFigure.SetPoint(aIndex: SizeInt; aValue: TFloatPoint);
begin
  fPoints[aIndex] := aValue;
end;

function TCanvasFigure.PointsCount(): SizeInt;
begin
  Result := Length(fPoints);
end;

function TCanvasFigure.RectInside(RectLeft, RectRight: TFloatPoint): boolean;
var
  FigureLeft, FigureRight: TFloatPoint;
begin
	FigureLeft := TopLeft;
	FigureRight := BottomRight;
	Result := (RectLeft.x <= FigureRight.x) and (RectLeft.y <= FigureLeft.y) and
	(RectRight.x >= FigureRight.x) and (RectRight.y >= FigureRight.y);
end;

procedure TCanvasFigure.Draw(aCanvas: TCanvas); 
begin
  aCanvas.Pen.Width := fWidth;
  aCanvas.Pen.Style := fPenStyle;
  aCanvas.Pen.Color := fPenColor;
  aCanvas.Brush.Style := fBrushStyle;
  aCanvas.Brush.Color := fBrushColor;
  Radius := fRadius;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TFigurePen
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TFigurePen.Draw(aCanvas: TCanvas);
begin
  inherited;
  aCanvas.Polyline(GetCanvasPoints());
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//FFigureEmpty
////////////////////////////////////////////////////////////////////////////////////////////////////


procedure FFigureEmpty.Draw(aCanvas: TCanvas);
begin

end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TFigureLine
////////////////////////////////////////////////////////////////////////////////////////////////////


procedure TFigureLine.Draw(aCanvas: TCanvas);
var
  cPoints: TPointArray;
begin
  inherited;
  cPoints := GetCanvasPoints();
  aCanvas.Line(cPoints[0].x, cPoints[0].y, cPoints[1].x, cPoints[1].y);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TFigurePolyline
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TFigurePolyline.Draw(aCanvas: TCanvas);
var
  i:integer;
begin
  inherited;
  aCanvas.Polyline(GetCanvasPoints());
  for i := 0 to FiguresCount() - 1 do
    GetFigure(count).selected :=  GetFigure(count).RectInside(aMin, aMax);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TFigureRectangle
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TFigureRectangle.Draw(aCanvas: TCanvas);
var
  i:integer;
  cPoints: TPointArray;
begin
  inherited;
  cPoints := GetCanvasPoints();
  aCanvas.Rectangle(cPoints[0].x, cPoints[0].y, cPoints[1].x, cPoints[1].y);
  for i := 0 to FiguresCount() - 1 do
    GetFigure(count).selected := GetFigure(count).RectInside(aMin, aMax);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//TFigureRoundRect
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TFigureRoundRect.Draw(aCanvas: TCanvas);
var
  cPoints: TPointArray;
  i:integer;
begin
	inherited;
  cPoints := GetCanvasPoints();
  aCanvas.RoundRect(cPoints[0].x, cPoints[0].y, cPoints[1].x, cPoints[1].y, Radius, Radius);
  for i := 0 to FiguresCount() - 1 do
    GetFigure(count).selected := GetFigure(count).RectInside(aMin, aMax);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TFigureEllipse
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TFigureEllipse.Draw(aCanvas: TCanvas);
var
  i:integer;
  cPoints: TPointArray;
begin
  inherited;
  cPoints := GetCanvasPoints();
  aCanvas.Ellipse(cPoints[0].x, cPoints[0].y, cPoints[1].x, cPoints[1].y);
  for count:= 0 to FiguresCount()-1 do
    GetFigure(count).selected := GetFigure(count).RectInside(aMin, aMax);
end;


initialization
  beginingRun := false;
 	aMax.x := MaxInt;
	aMax.y := MaxInt;
	aMin.x := 5 - MaxInt;
	aMin.y := 5 - MaxInt;

end.
