unit EditorTools;

{$MODE OBJFPC}
{$LONGSTRINGS ON}

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
interface
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

uses
  Classes, SysUtils, Graphics, Transform, Controls,
  CanvasFigures, ToolsParams;

type

  // GetName() - Возвращает название инструмента.
  // GetFigureClass() - Возвращает класс фигуры, рисуемый данным инструментом.
  // Start() - Инициализирует уже созданную фигуру по её индексу.
  // Update() - Обновляет положение инструмента. Возвращает, удалось ли это сделать.
  // Step() - Указывает, что часть фигуры нарисована. Возвращает, надо ли продолжать рисование.
  // Finish() - Завершает рисование фигуры. Возвращает, удалось ли это сделать.
  // По умолчанию Update() и Finish() возвращают, не равен ли индекс константе cFigureIndexInvalid.

	{ TEditorTool }

  TEditorTool = class
  private
    class function GetParams: TToolParamsList; static; virtual;
  public
		class procedure SetFigureParams (aFigureIndex: SizeInt); virtual; abstract;
		class property Params: TToolParamsList read GetParams;
    class function GetName(): String; virtual; abstract;
    class function GetFigureClass(): TCanvasFigureClass; virtual; abstract;
    class procedure Start(aFigureIndex: SizeInt; aXY: TPoint); virtual;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; virtual;
    class function Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean; virtual; abstract;
    class function Finish(aFigureIndex: SizeInt): Boolean; virtual;
  end;

	{ TToolPen }

  TToolPen = class(TEditorTool)
  private
    class var
      FParams: TToolParamsList;
    class function GetParams: TToolParamsList; static; override;
  public
    class procedure SetFigureParams (aFigureIndex: SizeInt); override;
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
    class function Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
  end;

	{ TToolLine }

  TToolLine = class(TEditorTool)
  private
    class var
      FParams: TToolParamsList;
    class function GetParams: TToolParamsList; static; override;
  public
    class procedure SetFigureParams (aFigureIndex: SizeInt); override;
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
    class function Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;

  end;

	{ TToolPolyline }

  TToolPolyline = class(TEditorTool)
  private
    class var
      FParams: TToolParamsList;
    class function GetParams: TToolParamsList; static; override;
  public
    class procedure SetFigureParams (aFigureIndex: SizeInt); override;
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
    class function Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
  end;

	{ TToolRectangle }

  TToolRectangle = class(TEditorTool)
  private
	  class var
	    FParams: TToolParamsList;
  class function GetParams: TToolParamsList; static; override;
  public
    class procedure SetFigureParams (aFigureIndex: SizeInt); override;
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
    class function Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
  end;

	{ TToolEllipse }

  TToolEllipse = class(TEditorTool)
  private
    class var
      FParams: TToolParamsList;
    class function GetParams: TToolParamsList; static; override;
  public
    class procedure SetFigureParams (aFigureIndex: SizeInt); override;
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
    class function Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
  end;

	{ TToolHand }

  TToolHand = class(TEditorTool)
  private
    class var
      FCanDraw: boolean;
    class function GetParams: TToolParamsList; static; override;
  public
    class procedure SetFigureParams (aFigureIndex: SizeInt); override;
    class procedure Start(aFigureIndex: SizeInt; aXY: TPoint); override;
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
    class function Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
	end;

	{ TToolZoom }

  TToolZoom = class(TEditorTool)
  private
    class var
      FCanDraw: boolean;
    class function GetParams: TToolParamsList; static; override;
  public
    class procedure SetFigureParams (aFigureIndex: SizeInt); override;
    class procedure Start(aFigureIndex: SizeInt; aXY: TPoint); override;
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
    class function Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
	end;


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


{ TToolZoom }

class function TToolZoom.GetParams: TToolParamsList;
begin
	Result:=inherited GetParams;
end;

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

class function TToolZoom.Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  Result := False;
end;

{ TToolHand }

class function TToolHand.GetParams: TToolParamsList;
begin
	Result:=inherited GetParams;
end;

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

class function TToolHand.Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  Result := False;
  FCanDraw := false;
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

class function TToolPen.Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  Result := False;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TToolLine
////////////////////////////////////////////////////////////////////////////////////////////////////

class function TToolLine.GetParams: TToolParamsList;
begin
	Result := FParams;
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

class function TToolLine.Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  Result := False;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TToolPolyline
////////////////////////////////////////////////////////////////////////////////////////////////////

class function TToolPolyline.GetParams: TToolParamsList;
begin
	Result := FParams;
end;

class procedure TToolPolyline.SetFigureParams(aFigureIndex: SizeInt);
var
  fFigure: TCanvasFigure;
begin
  fFigure := GetFigure(aFigureIndex);
  fFigure.PenColor := (FParams[0] as TColorLineParam).Value;
  fFigure.Width := (FParams[1] as TWidthLineParam).Value;
  fFigure.PenStyle := (FParams[2] as TStyleLineParam).Value;
end;

class function TToolPolyline.GetName(): String;
begin
  Result := 'Ломаная';
end;

class function TToolPolyline.GetFigureClass(): TCanvasFigureClass;
begin
  Result := TFigurePolyline;
end;

class function TToolPolyline.Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
var
  Figure: TCanvasFigure;
begin
  Result := inherited; if not Result then Exit;
  Figure := GetFigure(aFigureIndex);
  Figure.SetPoint(Figure.PointsCount()-1, ScreenToWorld(aXY.x, aXY.y));
end;

class function TToolPolyline.Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  Result := aFigureIndex <> cFigureIndexInvalid;
  if not Result then Exit;
  GetFigure(aFigureIndex).AddPoint(ScreenToWorld(aXY.x, aXY.y));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TToolRectangle
////////////////////////////////////////////////////////////////////////////////////////////////////

class function TToolRectangle.GetParams: TToolParamsList;
begin
	Result := FParams;
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
  Result := inherited; if not Result then Exit;
  GetFigure(aFigureIndex).SetPoint(1, ScreenToWorld(aXY.x, aXY.y));
end;

class function TToolRectangle.Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  Result := False;
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

class function TToolEllipse.GetFigureClass(): TCanvasFigureClass;
begin
  Result := TFigureEllipse;
end;

class function TToolEllipse.Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  Result := inherited; if not Result then Exit;
  GetFigure(aFigureIndex).SetPoint(1, ScreenToWorld(aXY.x, aXY.y));

end;

class function TToolEllipse.Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  Result := False;
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
    TToolPolyline,
    TToolRectangle,
    TToolEllipse
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
  TToolPolyline.FParams := TToolParamsList.Create(
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
  TToolEllipse.FParams := TToolParamsList.Create(
    TColorLineParam.Create,
    TColorBrushParam.Create,
    TWidthLineParam.Create,
    TStyleLineParam.Create,
    TFillStyleParam.Create
  );
end.
