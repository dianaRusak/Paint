unit EditorTools;

{$MODE OBJFPC}
{$LONGSTRINGS ON}

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
interface
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

uses
  Classes, SysUtils, Graphics,
  CanvasFigures;

const

  cPenStylesTable: array[0..4] of record 
    Name: String;
    Style: TPenStyle;
  end = (
    (Name: 'Сплошной'; Style: psSolid),
    (Name: 'Прерывистый'; Style: psDash),
    (Name: 'Точечный'; Style: psDot),
    (Name: 'Точка-тире'; Style: psDashDot),
    (Name: 'Точка-точка-тире'; Style: psDashDotDot)
  );

  cBrushStylesTable: array[0..6] of record 
    Name: String;
    Style: TBrushStyle;
  end = (
    (Name: 'Сплошной'; Style: bsSolid),
    (Name: 'Горизонтали'; Style: bsHorizontal),
    (Name: 'Вертикали'; Style: bsVertical),
    (Name: 'Диагонали \'; Style: bsFDiagonal),
    (Name: 'Диагонали /'; Style: bsBDiagonal),
    (Name: 'Прямые клетки'; Style: bsCross),
    (Name: 'Косые клетки'; Style: bsDiagCross)
  );

type

  // GetName() - Возвращает название инструмента.
  // GetFigureClass() - Возвращает класс фигуры, рисуемый данным инструментом.
  // Start() - Инициализирует уже созданную фигуру по её индексу.
  // Update() - Обновляет положение инструмента. Возвращает, удалось ли это сделать.
  // Step() - Указывает, что часть фигуры нарисована. Возвращает, надо ли продолжать рисование.
  // Finish() - Завершает рисование фигуры. Возвращает, удалось ли это сделать.
  // По умолчанию Update() и Finish() возвращают, не равен ли индекс константе cFigureIndexInvalid.

  TEditorTool = class
  public
    class function GetName(): String; virtual; abstract;
    class function GetFigureClass(): TCanvasFigureClass; virtual; abstract;
    class procedure Start(aFigureIndex: SizeInt; aXY: TPoint); virtual;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; virtual;
    class function Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean; virtual; abstract;
    class function Finish(aFigureIndex: SizeInt): Boolean; virtual;
  end;

  TToolPen = class(TEditorTool)
  public
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
    class function Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
  end;

  TToolLine = class(TEditorTool)
  public
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
    class function Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
  end;

  TToolPolyline = class(TEditorTool)
  public
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
    class function Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
  end;

  TToolRectangle = class(TEditorTool)
  public
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
    class function Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
  end;

  TToolEllipse = class(TEditorTool)
  public
    class function GetName(): String; override;
    class function GetFigureClass(): TCanvasFigureClass; override;
    class function Update(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
    class function Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean; override;
  end;

  TEditorToolClass = class of TEditorTool;

////////////////////////////////////////////////////////////////////////////////////////////////////

function GetEditorTool(aIndex: SizeInt): TEditorToolClass;
function EditorToolsCount(): SizeInt;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
implementation
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

type
  TEditorToolsClassesArray = array of TEditorToolClass;

var
  EditorToolsClasses: TEditorToolsClassesArray;

function GetEditorTool(aIndex: SizeInt): TEditorToolClass;
begin
  Result := EditorToolsClasses[aIndex];
end;

function EditorToolsCount(): SizeInt;
begin
  Result := Length(EditorToolsClasses);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TEditorTool
////////////////////////////////////////////////////////////////////////////////////////////////////

class procedure TEditorTool.Start(aFigureIndex: SizeInt; aXY: TPoint);
begin
  with GetFigure(aFigureIndex) do begin
    AddPoint(aXY);
    AddPoint(aXY);
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
  GetFigure(aFigureIndex).AddPoint(aXY);
end;

class function TToolPen.Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  Result := False;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TToolLine
////////////////////////////////////////////////////////////////////////////////////////////////////

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
  GetFigure(aFigureIndex).SetPoint(1, aXY);
end;

class function TToolLine.Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  Result := False;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TToolPolyline
////////////////////////////////////////////////////////////////////////////////////////////////////

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
  Figure.SetPoint(Figure.PointsCount()-1, aXY);
end;

class function TToolPolyline.Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  Result := aFigureIndex <> cFigureIndexInvalid;
  if not Result then Exit;
  GetFigure(aFigureIndex).AddPoint(aXY);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TToolRectangle
////////////////////////////////////////////////////////////////////////////////////////////////////

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
  GetFigure(aFigureIndex).SetPoint(1, aXY);
end;

class function TToolRectangle.Step(aFigureIndex: SizeInt; aXY: TPoint): Boolean;
begin
  Result := False;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TToolEllipse
////////////////////////////////////////////////////////////////////////////////////////////////////

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
  GetFigure(aFigureIndex).SetPoint(1, aXY);
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
    TToolPen,
    TToolLine,
    TToolPolyline,
    TToolRectangle,
    TToolEllipse
  );

end.

