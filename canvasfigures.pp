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
  ToolsParams, Transform;

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
  public
    function TopLeft():TFloatPoint;
    function BottomRight():TFloatPoint;
    procedure AddPoint(aValue: TFloatPoint);
    function GetPoint(aIndex: SizeInt): TFloatPoint;
    function GetCanvasPoints(): TPointArray;
    procedure SetPoint(aIndex: SizeInt; aValue: TFloatPoint);
    function PointsCount(): SizeInt;
    procedure Draw(aCanvas: TCanvas); virtual;
    property Width: Integer read fWidth write fWidth;
    property PenStyle: TPenStyle read fPenStyle write fPenStyle;
    property PenColor: TColor read fPenColor write fPenColor;
    property BrushStyle: TBrushStyle read fBrushStyle write fBrushStyle;
    property BrushColor: TColor read fBrushColor write fBrushColor;
    property Transparent: Boolean read fTransparent write fTransparent;
  end;

  TFigurePen = class(TCanvasFigure)
  public
    procedure Draw(aCanvas: TCanvas); override;
  end;

  TFigureLine = class(TCanvasFigure)
  public
    procedure Draw(aCanvas: TCanvas); override;
  end;

  TFigurePolyline = class(TCanvasFigure)
  public
    procedure Draw(aCanvas: TCanvas); override;
  end;

  TFigureRectangle = class(TCanvasFigure)
  public
    procedure Draw(aCanvas: TCanvas); override;
  end;

  TFigureEllipse = class(TCanvasFigure)
  public
    procedure Draw(aCanvas: TCanvas); override;
  end;

	{ FFigureEmpty }

  FFigureEmpty = class(TCanvasFigure)
  public
    procedure Draw(aCanvas: TCanvas); override;
	end;

{	{ FFigureRoundRect }

  FFigureRoundRect = class(TCanvasFigure)
  public
    procedure Draw(aCanvas: TCanvas); override;
	end;
}
  TCanvasFigureClass = class of TCanvasFigure;

////////////////////////////////////////////////////////////////////////////////////////////////////

function AddFigure(aFigureClass: TCanvasFigureClass): SizeInt;
procedure DeleteFigure(aIndex: SizeInt);
procedure ClearFigures();
function GetFigure(aIndex: SizeInt): TCanvasFigure;
function FiguresCount(): SizeInt;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
implementation
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

// данные фигур и функции для их добавления/удаления, а также получения конкретной фигуры

var
  FiguresData: array of TCanvasFigure;

function AddFigure(aFigureClass: TCanvasFigureClass): SizeInt;
begin
  Result := Length(FiguresData);
  SetLength(FiguresData, Result+1);
  FiguresData[Result] := aFigureClass.Create();
end;

procedure DeleteFigure(aIndex: SizeInt);
var
  NewLen: SizeInt;
begin
  FiguresData[aIndex].Destroy();
  NewLen := Length(FiguresData)-1;
  if NewLen > 0 then
    FiguresData[aIndex] := FiguresData[NewLen];
  SetLength(FiguresData, NewLen);
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

procedure TCanvasFigure.Draw(aCanvas: TCanvas); 
begin
  aCanvas.Pen.Width := fWidth;
  aCanvas.Pen.Style := fPenStyle;
  aCanvas.Pen.Color := fPenColor;
  aCanvas.Brush.Style := fBrushStyle;
  aCanvas.Brush.Color := fBrushColor;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TFigurePen
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TFigurePen.Draw(aCanvas: TCanvas);
{var
  LastPoint: SizeInt;}
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
begin
  inherited;
  aCanvas.Polyline(GetCanvasPoints());
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TFigureRectangle
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TFigureRectangle.Draw(aCanvas: TCanvas);
var
  cPoints: TPointArray;
begin
  inherited;
  cPoints := GetCanvasPoints();
  aCanvas.Rectangle(cPoints[0].x, cPoints[0].y, cPoints[1].x, cPoints[1].y);
end;
////////////////////////////////////////////////////////////////////////////////////////////////////
//FFigureRoundRect
////////////////////////////////////////////////////////////////////////////////////////////////////

{procedure FFigureRoundRect.Draw(aCanvas: TCanvas);
begin
	inherited;
  cPoints := GetCanvasPoints();
  aCanvas.Rectangle(cPoints[0].x, cPoints[0].y, cPoints[1].x, cPoints[1].y);
end;
}
////////////////////////////////////////////////////////////////////////////////////////////////////
// TFigureEllipse
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TFigureEllipse.Draw(aCanvas: TCanvas);
var
  cPoints: TPointArray;
begin
  inherited;
  cPoints := GetCanvasPoints();
  aCanvas.Ellipse(cPoints[0].x, cPoints[0].y, cPoints[1].x, cPoints[1].y);
end;

end.

