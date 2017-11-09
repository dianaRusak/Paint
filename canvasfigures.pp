unit CanvasFigures;

{$MODE OBJFPC}
{$LONGSTRINGS ON}

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
interface
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

uses
  Classes, SysUtils, Graphics;

const
  cFigureIndexInvalid = -1;

type

  TCanvasFigure = class
  strict protected
    fPoints: array of TPoint;
    fWidth: Integer;
    fPenStyle: TPenStyle;
    fPenColor: TColor;
    fBrushStyle: TBrushStyle;
    fBrushColor: TColor;
    fTransparent: Boolean;
  public
    procedure AddPoint(aValue: TPoint);
    function GetPoint(aIndex: SizeInt): TPoint;
    procedure SetPoint(aIndex: SizeInt; aValue: TPoint);
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
  Result := FiguresData[aIndex];
end;

function FiguresCount(): SizeInt;
begin
  Result := Length(FiguresData);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TCanvasFigure
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TCanvasFigure.AddPoint(aValue: TPoint);
var
  Len: SizeInt;
begin
  Len := Length(fPoints);
  SetLength(fPoints, Len+1);
  fPoints[Len] := aValue;
end;

function TCanvasFigure.GetPoint(aIndex: SizeInt): TPoint;
begin
  Result := fPoints[aIndex];
end;

procedure TCanvasFigure.SetPoint(aIndex: SizeInt; aValue: TPoint);
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

  if fTransparent then begin
    aCanvas.Brush.Style := bsClear;
  end else begin
    aCanvas.Brush.Style := fBrushStyle;
    aCanvas.Brush.Color := fBrushColor;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TFigurePen
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TFigurePen.Draw(aCanvas: TCanvas);
var
  LastPoint: SizeInt;
begin
  inherited;
  aCanvas.Polyline(fPoints);
  // TCanvas.Polyline() рисует линию вплоть до точки, но не включая её,
  // поэтому последнюю точку рисуем отдельно.
  LastPoint := High(fPoints);
  if LastPoint > 0 then
    aCanvas.Pixels[fPoints[LastPoint].x, fPoints[LastPoint].y] := fPenColor;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TFigureLine
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TFigureLine.Draw(aCanvas: TCanvas);
begin
  inherited;
  aCanvas.Line(fPoints[0].x, fPoints[0].y, fPoints[1].x, fPoints[1].y);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TFigurePolyline
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TFigurePolyline.Draw(aCanvas: TCanvas);
begin
  inherited;
  aCanvas.Polyline(fPoints);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TFigureRectangle
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TFigureRectangle.Draw(aCanvas: TCanvas);
begin
  inherited;
  aCanvas.Rectangle(fPoints[0].x, fPoints[0].y, fPoints[1].x, fPoints[1].y);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TFigureEllipse
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TFigureEllipse.Draw(aCanvas: TCanvas);
begin
  inherited;
  aCanvas.Ellipse(fPoints[0].x, fPoints[0].y, fPoints[1].x, fPoints[1].y);
end;

end.

