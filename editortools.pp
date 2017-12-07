unit EditorTools;

{$MODE OBJFPC}
{$LONGSTRINGS ON}

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
interface
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

uses
  Classes, SysUtils, Graphics, Transform, Controls, GraphMath,
  CanvasFigures, ToolsParams;

type

  TFloatPointArray = array of TFloatPoint;

////////////////////////////////////////////////////////////////////////////////////////////////////
//TEditorTool

  TEditorTool = class
  private
	  class function GetParams: TToolParamsList; static; virtual;
  public
		class procedure SetFigureParams (aFigureIndex: SizeInt); virtual; abstract;
		class property Params: TToolParamsList read GetParams;
    // GetName() - Возвращает название инструмента.
    class function GetName(): String; virtual; abstract;
    // GetFigureClass() - Возвращает класс фигуры, рисуемый данным инструментом.
    class function GetFigureClass(): TCanvasFigureClass; virtual; abstract;
    // Start() - Инициализирует уже созданную фигуру по её индексу.
    class procedure Start(aFigureIndex: SizeInt; aXY: TPoint); virtual;
    // Update() - Обновляет положение инструмента. Возвращает, удалось ли это сделать.
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; virtual;
    // Finish() - Завершает рисование фигуры. Возвращает, удалось ли это сделать.
    class function Finish(aFigureIndex: SizeInt): Boolean; virtual;
    // По умолчанию Update() и Finish() возвращают, не равен ли индекс константе cFigureIndexInvalid.
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//TToolPen

  TToolPen = class(TEditorTool)
  private
    class var
      FParams: TToolParamsList;
    class function GetParams: TToolParamsList; static; override;
  public
    class procedure Start(aFigureIndex: SizeInt; aXY: TPoint); override;
    class procedure SetFigureParams (aFigureIndex: SizeInt); override;
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//TToolLine

  TToolLine = class(TEditorTool)
  private
    class var
      FParams: TToolParamsList;
    class function GetParams: TToolParamsList; static; override;
  public
    class procedure Start(aFigureIndex: SizeInt; aXY: TPoint); override;
    class procedure SetFigureParams (aFigureIndex: SizeInt); override;
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//TToolRectangle

  TToolRectangle = class(TEditorTool)
  private
	  class var
	    FParams: TToolParamsList;
    class function GetParams: TToolParamsList; static; override;
  public
    class procedure Start(AFigureIndex: SizeInt; AXY: Tpoint); override;
    class procedure SetFigureParams (aFigureIndex: SizeInt); override;
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//TToolRounRect

  TToolRounRect = class(TEditorTool)
  private
	  class var
	    FParams: TToolParamsList;
  class function GetParams: TToolParamsList; static; override;
  public
    class procedure Start(AFigureIndex: SizeInt; AXY: Tpoint); override;
    class procedure SetFigureParams (aFigureIndex: SizeInt); override;
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//TToolEllipse

  TToolEllipse = class(TEditorTool)
  private
    class var
      FParams: TToolParamsList;
    class function GetParams: TToolParamsList; static; override;
  public
    class procedure SetFigureParams (aFigureIndex: SizeInt); override;
    class function GetName(): String; override;
    class procedure Start(AFigureIndex: SizeInt; AXY: Tpoint); override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//TToolHand

  TToolHand = class(TEditorTool)
  class var
    FCanDraw: Boolean;
  public
    class procedure SetFigureParams (aFigureIndex: SizeInt); override;
    class procedure Start(aFigureIndex: SizeInt; aXY: TPoint); override;
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
    class function Finish(aFigureIndex: SizeInt): Boolean;override;
	end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//TToolZoom

  TToolZoom = class(TEditorTool)
  public
    class procedure SetFigureParams (aFigureIndex: SizeInt); override;
    class procedure Start(aFigureIndex: SizeInt; aXY: TPoint); override;
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
    class function Finish(aFigureIndex: SizeInt): Boolean;override;
	end;
////////////////////////////////////////////////////////////////////////////////////////////////////
//TToolAllocator

  TToolAllocator = class(TEditorTool)
  private
    class var
      FParams: TToolParamsList;
    class function GetParams: TToolParamsList; static; override;
  public
    class procedure Start(aFigureIndex: SizeInt; aXY: TPoint); override;
    class procedure SetFigureParams (aFigureIndex: SizeInt); override;
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
    class function Finish(aFigureIndex: SizeInt): Boolean; override;
	end;

	{ TToolCursor }

 TToolCursor = class(TEditorTool)
   private
     class var
      FCanDraw: Boolean; Previous: TPoint;
   public
    class procedure SetFigureParams (aFigureIndex: SizeInt); override;
    class procedure Start(aFigureIndex: SizeInt; aXY: TPoint); override;
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
    class function Finish(aFigureIndex: SizeInt): Boolean; override;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////
  TEditorToolClass = class of TEditorTool;

////////////////////////////////////////////////////////////////////////////////////////////////////

function GetEditorTool(aIndex: SizeInt): TEditorToolClass;
function EditorToolsCount(): SizeInt;
procedure SetButton(Button: TMouseButton);

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

implementation

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

type
  TEditorToolsClassesArray = array of TEditorToolClass;

var
  EditorToolsClasses: TEditorToolsClassesArray;
  MBtn: TMouseButton;

function GetEditorTool(aIndex: SizeInt): TEditorToolClass;
begin
  Result := EditorToolsClasses[aIndex];
end;

function EditorToolsCount(): SizeInt;
begin
  Result := Length(EditorToolsClasses);
end;

procedure SetButton(Button: TMouseButton);
begin
  MBtn := Button;
end;

{ TToolCursor }

class procedure TToolCursor.SetFigureParams(aFigureIndex: SizeInt);
begin

end;

class procedure TToolCursor.Start(aFigureIndex: SizeInt; aXY: TPoint);
begin
	inherited Start(aFigureIndex, aXY);
  if (FigureLeft.x <=aXY.x) and (FigureLeft.y <= aXY.y) and
  (FigureRight.x >= aXY.x) and (FigureRight.y >= aXY.y) then begin
	  Previous:=aXY;
	  FCanDraw := true;
	end;
end;

class function TToolCursor.GetName: String;
begin
  Result := 'Курсор';
end;

class function TToolCursor.GetFigureClass: TCanvasFigureClass;
begin
  Result := FFigureEmpty;
end;

class function TToolCursor.Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
var
  i, j:SizeInt;
  delta: TFloatPoint;
begin
	Result:=inherited Update(aFigureIndex, aXY);
  if FCanDraw then
  For i:=0 to FiguresCount() - 1  do
    if GetFigure(i).selected then begin
      delta := FloatPoint((aXY.x-Previous.x)/Zoom,(aXY.y-Previous.y)/Zoom);
      GetFigure(i).MoveFigure(delta.x, delta.y);
		end;
  Previous := aXY;
end;

class function TToolCursor.Finish(aFigureIndex: SizeInt): Boolean;
begin
	Result:=inherited Finish(aFigureIndex);
  FCanDraw:=False;
  DeleteFigure(AFigureIndex);
end;

{ TToolAllocator }

class function TToolAllocator.GetParams: TToolParamsList;
begin

end;

class procedure TToolAllocator.Start(aFigureIndex: SizeInt; aXY: TPoint);
begin
	inherited Start(aFigureIndex, aXY);
end;

class procedure TToolAllocator.SetFigureParams(aFigureIndex: SizeInt);
begin

end;

class function TToolAllocator.GetName: String;
begin
  Result := 'Выделитель';
end;

class function TToolAllocator.GetFigureClass: TCanvasFigureClass;
begin
  Result := FFigureAllotment;
end;

class function TToolAllocator.Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
	Result := inherited;
  if not Result then Exit(false);
  GetFigure(aFigureIndex).SetPoint(1, ScreenToWorld(aXY.x, aXY.y));
end;

class function TToolAllocator.Finish(aFigureIndex: SizeInt): Boolean;
var
  i:SizeInt;
  Counter:SizeInt;
begin
  Result := aFigureIndex <> cFigureIndexInvalid;
  DeleteFigure(aFigureIndex);
  If not beginingRun then
    for Counter := 0 to FiguresCount() - 1 do
      GetFigure(Counter).selected := GetFigure(Counter).RectInside(aMin, aMax)
  else begin
    for Counter := 0 to FiguresCount() - 1 do begin
      GetFigure(Counter).selected := GetFigure(Counter).RectInside(aMin, aMax);
      If GetFigure(Counter).selected then
        For i:=0 to Counter-1 do
          GetFigure(i).selected:=false;
    end;
  end;
end;


////////////////////////////////////////////////////////////////////////////////////////////////////
//TToolZoom
////////////////////////////////////////////////////////////////////////////////////////////////////

class procedure TToolZoom.SetFigureParams(aFigureIndex: SizeInt);
begin

end;

class procedure TToolZoom.Start(aFigureIndex: SizeInt; aXY: TPoint);
begin
	inherited Start(aFigureIndex, aXY);
  if MBtn = mbLeft then
    ZoomPoint(ScreenToWorld(aXY.x, aXY.y), Zoom*2)
  else if MBtn = mbRight then
    ZoomPoint(ScreenToWorld(aXY.x, aXY.y), Zoom/2);
end;

class function TToolZoom.GetName: String;
begin
  Result := 'Зуууууууум';
end;

class function TToolZoom.GetFigureClass: TCanvasFigureClass;
begin
  Result := FFigureEmpty;
end;

class function TToolZoom.Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
	Result:= False;
end;

class function TToolZoom.Finish(aFigureIndex: SizeInt): Boolean;
begin
  Result := inherited Finish(AFigureIndex);
  DeleteFigure(AFigureIndex);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//TToolHand
////////////////////////////////////////////////////////////////////////////////////////////////////

class procedure TToolHand.SetFigureParams(aFigureIndex: SizeInt);
begin

end;

class procedure TToolHand.Start(aFigureIndex: SizeInt; aXY: TPoint);
begin
	inherited Start(aFigureIndex, aXY);
  FCanDraw := true;
end;

class function TToolHand.GetName: String;
begin
  Result := 'Лапка';
end;

class function TToolHand.GetFigureClass: TCanvasFigureClass;
begin
  Result := FFigureEmpty;
end;

class function TToolHand.Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  if not FCanDraw then exit(false);
	Result := true;
  ScreenOffset.x := ScreenOffset.x + GetFigure(aFigureIndex).GetPoint(0).x
    - ScreenToWorld(aXY.x, aXY.y).x;
  ScreenOffset.y := ScreenOffset.y + GetFigure(aFigureIndex).GetPoint(0).y
    - ScreenToWorld(aXY.x, aXY.y).y;
end;

class function TToolHand.Finish(aFigureIndex: SizeInt): Boolean;
begin
  Result := inherited Finish(AFigureIndex);
  DeleteFigure(AFigureIndex);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TEditorTool
////////////////////////////////////////////////////////////////////////////////////////////////////

class function TEditorTool.GetParams: TToolParamsList;
begin
  Result := nil;
end;

class procedure TEditorTool.Start(aFigureIndex: SizeInt; aXY: TPoint);
begin
  with GetFigure(aFigureIndex) do begin
    AddPoint(ScreenToWorld(aXY.x, aXY.y));
    AddPoint(ScreenToWorld(aXY.x, aXY.y));
  end;
end;

class function TEditorTool.Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  Result := aFigureIndex <> cFigureIndexInvalid;
end;

class function TEditorTool.Finish(aFigureIndex: SizeInt): Boolean;
begin
  Result := aFigureIndex <> cFigureIndexInvalid;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TToolPen
////////////////////////////////////////////////////////////////////////////////////////////////////

class function TToolPen.GetParams: TToolParamsList;
begin
	Result := FParams;
end;

class procedure TToolPen.Start(aFigureIndex: SizeInt; aXY: TPoint);
begin
	inherited Start(aFigureIndex, aXY);
  UnSelectAll();
end;

class procedure TToolPen.SetFigureParams(aFigureIndex: SizeInt);
var
  fFigure: TCanvasFigure;
begin
  fFigure := GetFigure(aFigureIndex);
  fFigure.PenColor := (FParams[0] as TColorLineParam).Value;
  fFigure.Width := (FParams[1] as TWidthLineParam).Value;
  fFigure.PenStyle := (FParams[2] as TStyleLineParam).Value;
end;

class function TToolPen.GetName(): String;
begin
  Result := 'Карандаш';
end;

class function TToolPen.GetFigureClass(): TCanvasFigureClass;
begin
  Result := TFigurePen;
end;

class function TToolPen.Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  Result := inherited; if not Result then Exit;
  GetFigure(aFigureIndex).AddPoint(ScreenToWorld(aXY.x, aXY.y));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TToolLine
////////////////////////////////////////////////////////////////////////////////////////////////////

class function TToolLine.GetParams: TToolParamsList;
begin
	Result := FParams;
end;

class procedure TToolLine.Start(aFigureIndex: SizeInt; aXY: TPoint);
begin
	inherited Start(aFigureIndex, aXY);
  UnSelectAll();
end;

class procedure TToolLine.SetFigureParams(aFigureIndex: SizeInt);
var
  fFigure: TCanvasFigure;
begin
  fFigure := GetFigure(aFigureIndex);
  fFigure.PenColor := (FParams[0] as TColorLineParam).Value;
  fFigure.Width := (FParams[1] as TWidthLineParam).Value;
  fFigure.PenStyle := (FParams[2] as TStyleLineParam).Value;
end;

class function TToolLine.GetName(): String;
begin
  Result := 'Линия';
end;

class function TToolLine.GetFigureClass(): TCanvasFigureClass;
begin
  Result := TFigureLine;
end;

class function TToolLine.Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  Result := inherited; if not Result then Exit;
  GetFigure(aFigureIndex).SetPoint(1, ScreenToWorld(aXY.x, aXY.y));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TToolRectangle
////////////////////////////////////////////////////////////////////////////////////////////////////

class function TToolRectangle.GetParams: TToolParamsList;
begin
	Result := FParams;
end;

class procedure TToolRectangle.Start(AFigureIndex: SizeInt; AXY: Tpoint);
begin
	inherited Start(AFigureIndex, AXY);
  UnSelectAll();
end;

class procedure TToolRectangle.SetFigureParams(aFigureIndex: SizeInt);
var
  fFigure: TCanvasFigure;
begin
  fFigure := GetFigure(aFigureIndex);
  fFigure.PenColor := (FParams[0] as TColorLineParam).Value;
  fFigure.BrushColor := (FParams[1] as TColorBrushParam).Value;
  fFigure.Width := (FParams[2] as TWidthLineParam).Value;
  fFigure.PenStyle := (FParams[3] as TStyleLineParam).Value;
  fFigure.BrushStyle := (FParams[4] as TFillStyleParam).Value;
end;

class function TToolRectangle.GetName(): String;
begin
  Result := 'Прямоугольник';
end;

class function TToolRectangle.GetFigureClass(): TCanvasFigureClass;
begin
  Result := TFigureRectangle;
end;

class function TToolRectangle.Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  Result := inherited;
  if not Result then Exit;
  GetFigure(aFigureIndex).SetPoint(1, ScreenToWorld(aXY.x, aXY.y));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//TToolRounRect
////////////////////////////////////////////////////////////////////////////////////////////////////

class function TToolRounRect.GetParams: TToolParamsList;
begin
	Result := FParams;
end;

class procedure TToolRounRect.Start(AFigureIndex: SizeInt; AXY: Tpoint);
begin
	inherited Start(AFigureIndex, AXY);
  UnSelectAll();
end;

class procedure TToolRounRect.SetFigureParams(aFigureIndex: SizeInt);
var
  fFigure: TCanvasFigure;
begin
  fFigure := GetFigure(aFigureIndex);
  with fFigure do begin
		PenColor := (FParams[0] as TColorLineParam).Value;

		Width := (FParams[2] as TWidthLineParam).Value;
		BrushStyle := (FParams[4] as TFillStyleParam).Value;
    BrushColor := (FParams[1] as TColorBrushParam).Value;
    PenStyle := (FParams[3] as TStyleLineParam).Value;
		Radius := (FParams[5] as TRadiusParam).Value;
  end;
end;

class function TToolRounRect.GetName: String;
begin
  Result := 'Скруглённый прямоугольник';
end;

class function TToolRounRect.GetFigureClass: TCanvasFigureClass;
begin
  Result := TFigureRoundRect;
end;

class function TToolRounRect.Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
	Result := inherited;
  if not Result then Exit;
  GetFigure(aFigureIndex).SetPoint(1, ScreenToWorld(aXY.x, aXY.y));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TToolEllipse
////////////////////////////////////////////////////////////////////////////////////////////////////

class function TToolEllipse.GetParams: TToolParamsList;
begin
	Result := FParams;
end;

class procedure TToolEllipse.SetFigureParams(aFigureIndex: SizeInt);
var
  fFigure: TCanvasFigure;
begin
  fFigure := GetFigure(aFigureIndex);
  fFigure.PenColor := (FParams[0] as TColorLineParam).Value;
  fFigure.BrushColor := (FParams[1] as TColorBrushParam).Value;;
  fFigure.Width := (FParams[2] as TWidthLineParam).Value;
  fFigure.PenStyle := (FParams[3] as TStyleLineParam).Value;
  fFigure.BrushStyle := (FParams[4] as TFillStyleParam).Value;
end;

class function TToolEllipse.GetName(): String;
begin
  Result := 'Эллипс';
end;

class procedure TToolEllipse.Start(AFigureIndex: SizeInt; AXY: Tpoint);
begin
	inherited Start(AFigureIndex, AXY);
  UnSelectAll();
end;

class function TToolEllipse.GetFigureClass(): TCanvasFigureClass;
begin
  Result := TFigureEllipse;
end;

class function TToolEllipse.Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  Result := inherited; if not Result then Exit;
  GetFigure(aFigureIndex).SetPoint(1, ScreenToWorld(aXY.x, aXY.y));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
initialization
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

  EditorToolsClasses := TEditorToolsClassesArray.Create(
    TToolHand,
    TToolZoom,
    TToolPen,
    TToolLine,
    TToolRectangle,
    TToolRounRect,
    TToolEllipse,
    TToolAllocator,
    TToolCursor
  );
  TToolLine.FParams := TToolParamsList.Create(
    TColorLineParam.Create,
    TWidthLineParam.Create,
    TStyleLineParam.Create
  );
  TToolPen.FParams := TToolParamsList.Create(
    TColorLineParam.Create,
    TWidthLineParam.Create,
    TStyleLineParam.Create
  );
  TToolRectangle.FParams := TToolParamsList.Create(
    TColorLineParam.Create,
    TColorBrushParam.Create,
    TWidthLineParam.Create,
    TStyleLineParam.Create,
    TFillStyleParam.Create
  );
  TToolRounRect.FParams := TToolParamsList.Create(
    TColorLineParam.Create,
    TColorBrushParam.Create,
    TWidthLineParam.Create,
    TStyleLineParam.Create,
    TFillStyleParam.Create,
    TRadiusParam.Create
  );
  TToolEllipse.FParams := TToolParamsList.Create(
    TColorLineParam.Create,
    TColorBrushParam.Create,
    TWidthLineParam.Create,
    TStyleLineParam.Create,
    TFillStyleParam.Create
  );
end.
