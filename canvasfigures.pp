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
  intMin = 2 - MaxInt;

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
    fRadius: integer;
  public
    Selected : boolean;
    function TopLeft():TFloatPoint;
    procedure DrawSelection (aCanvas: TCanvas);
    function BottomRight():TFloatPoint;
    procedure AddPoint(aValue: TFloatPoint);
    function LenPoints():Integer;virtual;
    function GetPoint(aIndex: SizeInt): TFloatPoint;
    function GetCanvasPoints(): TPointArray;
    procedure SetPoint(aIndex: SizeInt; aValue: TFloatPoint);
    function PointsCount(): SizeInt;
    procedure MoveFigure(dx, dy: extended);
    function RectInside(RectLeft, RectRight: TFloatPoint): boolean;
    procedure Draw(aCanvas: TCanvas); virtual;
    property Radius: integer read fRadius write fRadius;
    property Width: Integer read fWidth write fWidth;
    property PenStyle: TPenStyle read fPenStyle write fPenStyle;
    property PenColor: TColor read fPenColor write fPenColor;
    property BrushStyle: TBrushStyle read fBrushStyle write fBrushStyle;
    property BrushColor: TColor read fBrushColor write fBrushColor;
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
procedure MoveForefront();
procedure MoveBackground();

var
  beginingRun: Boolean;
  aMin, aMax: TFloatPoint;
  FigureLeft, FigureRight: TPoint;

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
  Result := FiguresData[aIndex];
end;

function FiguresCount(): SizeInt;
begin
  Result := Length(FiguresData);
end;

procedure PSelectAll;
var
  i:SizeInt;
begin
  for i := 0 to FiguresCount() - 1 do
  begin
    GetFigure(i).selected := True;
  end;
end;

procedure UnSelectAll;
var
  i:SizeInt;
begin
  for i := 0 to FiguresCount() - 1 do
  begin
    GetFigure(i).selected := False;
  end;
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
    for i:= Low(FiguresData) to High (FiguresData)-1 do
      if (FiguresData[i] = nil) and (i + 1 < Length(FiguresData)) then
      begin
        FiguresData[i] := FiguresData[i+1];
        FiguresData[i+1] := nil;
	   	end;
  SetLength(FiguresData, Length(FiguresData) - j);
end;

procedure MoveForefront;
var
  i: SizeInt;
  t: TCanvasFigure;
begin
  for i := High(FiguresData) downto Low(FiguresData) do
  begin
    if FiguresData[i].Selected and (i + 1 < Length(FiguresData)) then
    begin
      t := FiguresData[i + 1];
      FiguresData[i + 1] := FiguresData[i];
      FiguresData[i] := t;
    end;
  end;
end;

procedure MoveBackground;
var
  i: SizeInt;
  t: TCanvasFigure;
begin
  for i := Low(FiguresData) to High(FiguresData) do
  begin
    if FiguresData[i].Selected and (i - 1 >= 0) then
    begin
      t := FiguresData[i - 1];
      FiguresData[i - 1] := FiguresData[i];
      FiguresData[i] := t;
    end;
  end;
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
  ot: SizeInt;
begin
  ot := fWidth div 2;
  if (Selected) then
  begin
    FigureLeft := WorldToScreen(TopLeft.x, TopLeft.y);
  	FigureRight := WorldToScreen(BottomRight.x, BottomRight.y);
    with aCanvas do
	  begin
	    Pen.Color := clPurple;
	    Pen.Width := 1;
	    Pen.Style := psDash;
		  Brush.Style := bsClear;
	    Rectangle(FigureLeft.X - ot - 7, FigureLeft.Y - ot - 7, FigureRight.X + ot + 7,
      FigureRight.Y + ot + 7);
      Pen.Color := clBlue;
 	    Pen.Width := 2;
 	    Pen.Style := psSolid;
 		  Brush.Style := bsClear;
      Rectangle(FigureLeft.X - ot - 7, FigureLeft.Y - ot - 7, FigureLeft.X + 1, FigureLeft.Y + 1);
      Rectangle(FigureRight.X - ot + 7, FigureRight.Y - ot + 7, FigureRight.X + 1, FigureRight.Y + 1);
	  end;
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

function TCanvasFigure.LenPoints: Integer;
var
  cPoints: TPointArray;
begin
  cPoints := GetCanvasPoints();
  Result := Length(cPoints);
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

procedure TCanvasFigure.MoveFigure(dx, dy: extended);
var
  i: SizeInt;
begin
  for i := low(fPoints) to High(fPoints) do
  begin
    fPoints[i].x := fPoints[i].x + dx;
    fPoints[i].y := fPoints[i].y + dy;
  end;
end;

function TCanvasFigure.RectInside(RectLeft, RectRight: TFloatPoint): boolean;
const
    eps = 10;
var
  FigureLeft, FigureRight: TFloatPoint;
  diag: double;
begin
  diag := sqrt(sqr(RectRight.x - RectLeft.x) + sqr(RectLeft.y - RectRight.y));

  FigureRight := WorldToScreen(BottomRight.x, BottomRight.y);

  FigureLeft := WorldToScreen(TopLeft.x, TopLeft.y);
  if eps <= diag then
  begin
    Result := (RectLeft.x <= FigureLeft.x) and (RectLeft.y <= FigureLeft.y) and
    (RectRight.x >= FigureRight.x) and (RectRight.Y >= FigureRight.Y);
    beginingRun := False;
  end
  else
  begin
    Result := (RectLeft.x >= FigureLeft.x) and (RectLeft.y >= FigureLeft.y) and
      (RectRight.x <= FigureRight.x) and (RectRight.Y <= FigureRight.Y);
    beginingRun := True;
  end;
end;

procedure TCanvasFigure.Draw(aCanvas: TCanvas); 
begin
  with aCanvas do begin
    Pen.Width := fWidth;
	  Pen.Style := fPenStyle;
	  Pen.Color := fPenColor;
	  Brush.Style := fBrushStyle;
	  if Brush.Style <> bsClear then
	      Brush.Color := fBrushColor;
	  Radius := fRadius;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TFigurePen
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TFigurePen.Draw(aCanvas: TCanvas);
var
  i:integer;
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
end;

begin
  UnSelectAll;
end.
