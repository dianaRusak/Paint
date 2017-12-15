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
  fStylesLine: array[0..5] of TPenStyle = (psSolid, psClear, psDot, psDash,
     psDashDot, psDashDotDot);

  cFigureIndexInvalid = -1;
  intMin = 2 - MaxInt;
  Err = 7;

type
  TPointArray = array of TPoint;

  { TCanvasFigure }

  TCanvasFigure = class
  fPoints: array of TFloatPoint;
  strict protected
    fWidth: Integer;
    fPenStyle: TPenStyle;
    fPenColor: TColor;
    fBrushColor: TColor;
    fBrushStyle: TBrushStyle;
    fRadius: Integer;
  public
    Selected: boolean;
    procedure ResizeFigure(fPointIndex: SizeInt; dX, dY: extended);
    procedure DrawSelection (aCanvas: TCanvas);
    function BottomRight():TFloatPoint;virtual;
    function TopLeft():TFloatPoint;virtual;
    procedure AddPoint(aValue: TFloatPoint);
    function LenPoints():Integer;virtual;
    function GetPoint(aIndex: SizeInt): TFloatPoint;
    function GetCanvasPoints(): TPointArray;
    procedure SetPoint(aIndex: SizeInt; aValue: TFloatPoint);
    function PointsCount(): SizeInt;
    procedure MoveFigure(dx, dy: extended);
    function RectInside(RectLeft, RectRight: TFloatPoint): boolean;
    procedure Draw(aCanvas: TCanvas); virtual;
    published
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
  function BottomRight():TFloatPoint;virtual;
  function TopLeft():TFloatPoint;virtual;
  procedure Draw(aCanvas: TCanvas); override;
end;

	{ TFigureLine }

TFigureLine = class(TCanvasFigure)
public
  function BottomRight():TFloatPoint;virtual;
  function TopLeft():TFloatPoint;virtual;
  procedure Draw(aCanvas: TCanvas); override;
end;

	{ TFigureRectangle }

TFigureRectangle = class(TCanvasFigure)
public
  procedure GetParams(aCanvas: TCanvas);
  procedure Draw(aCanvas: TCanvas); override;
end;

	{ TFigureEllipse }

TFigureEllipse = class(TCanvasFigure)
public
  function BottomRight():TFloatPoint;virtual;
  function TopLeft():TFloatPoint;virtual;
  procedure Draw(aCanvas: TCanvas); override;
end;

	{ TFigureRoundRect }

TFigureRoundRect = class(TCanvasFigure)
public
  function BottomRight():TFloatPoint;virtual;
  function TopLeft():TFloatPoint;virtual;
  procedure Draw(aCanvas: TCanvas); override;
end;
  
	{ FFigureEmpty }

FFigureEmpty = class(TCanvasFigure)
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
function StrToClassFigure(str: String):TCanvasFigureClass;
function StyleToNum(style: TPenStyle):integer;
function NumToStyle(int:integer):TPenStyle;
function BrushStyleToNum(style: TBrushStyle):integer;
function NumToBrushStyle(int:integer):TBrushStyle;

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

function StrToClassFigure(str: String):TCanvasFigureClass;
begin
  case str of
    'TFigurePen': Result := TFigurePen;
    'TFigureLine': Result := TFigureLine;
    'TFigureRectangle': Result := TFigureRectangle;
    'TFigureEllipse': Result := TFigureEllipse;
    'TFigureRoundRect': Result := TFigureRoundRect;
  end;
end;

function StyleToNum(style: TPenStyle): integer;
begin
   case style of
     psSolid: Result := 0;
     psDash: Result := 1;
     psDot: Result := 2;
     psDashDot: Result := 3;
     psDashDotDot: Result := 4;
   end;
end;

function NumToStyle(int: integer): TPenStyle;
begin
  case int of
    0: Result := psSolid;
    1: Result := psDash;
    2: Result := psDot;
    3: Result := psDashDot;
    4: Result := psDashDotDot;
  end;
end;

function BrushStyleToNum(style: TBrushStyle): integer;

begin
   case style of
     bsSolid: Result := 0;
     bsClear: Result := 1;
     bsHorizontal: Result := 2;
     bsVertical: Result := 3;
     bsFDiagonal: Result := 4;
     bsBDiagonal: Result := 5;
     bsDiagCross: Result := 6;
     bsCross: Result := 7;
   end;
end;

function NumToBrushStyle(int: integer): TBrushStyle;
begin
  case int of
    0: Result := bsSolid;
    1: Result := bsClear;
    2: Result := bsHorizontal;
    3: Result := bsVertical;
    4: Result := bsFDiagonal;
    5: Result := bsBDiagonal;
    6: Result := bsDiagCross ;
    7: Result := bsCross;
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


function TCanvasFigure.TopLeft(): TFloatPoint;
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

procedure TCanvasFigure.ResizeFigure(fPointIndex: SizeInt; dX, dY: extended);
begin
  fPoints[fPointIndex].x := fPoints[fPointIndex].x + dx;
  fPoints[fPointIndex].y := fPoints[fPointIndex].y + dy;
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
      Pen.Color := clBlack;
      Pen.Width := 1;
      Pen.Style := psDash;
      Brush.Style := bsClear;

      Rectangle(FigureLeft.X - ot - Err, FigureLeft.Y - ot - Err, FigureRight.X + ot + Err,
      FigureRight.Y + ot + Err);

      Pen.Color := clBlue;
      Pen.Width := 2;
      Pen.Style := psSolid;
      Brush.Style := bsClear;

      Rectangle(FigureLeft.X - Err, FigureLeft.Y - Err, FigureLeft.X + Err,  FigureLeft.Y + Err);
      Rectangle(FigureRight.X + Err, FigureRight.Y + Err, FigureRight.X - Err, FigureRight.Y - Err);

    end;
  end;
end;

function TCanvasFigure.BottomRight(): TFloatPoint;
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

function TCanvasFigure.LenPoints(): Integer;
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

function TCanvasFigure.GetCanvasPoints(): TPointArray;
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

function TFigurePen.BottomRight(): TFloatPoint;
begin
  inherited
end;

function TFigurePen.TopLeft(): TFloatPoint;
begin
  inherited
end;

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

function TFigureLine.BottomRight(): TFloatPoint;
begin
  inherited
end;

function TFigureLine.TopLeft(): TFloatPoint;
begin
  inherited
end;

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

procedure TFigureRectangle.GetParams(aCanvas: TCanvas);
begin
  inherited;
end;

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

function TFigureRoundRect.BottomRight(): TFloatPoint;
begin
  inherited
end;

function TFigureRoundRect.TopLeft(): TFloatPoint;
begin
  inherited
end;

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

function TFigureEllipse.BottomRight(): TFloatPoint;
begin
  inherited
end;

function TFigureEllipse.TopLeft(): TFloatPoint;
begin
  inherited
end;

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
