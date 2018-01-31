unit UndoRedo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CanvasFigures;

type
  TCanvasArray = array of TCanvasFigure;
procedure AddUndoRedo();
procedure Undo();
procedure Redo();
function CanUndo(): boolean;
function CanRedo(): boolean;

var
  UndoRedoElements: array of TCanvasArray;
  UndoRedoIndex: integer;

implementation

procedure AddUndoRedo();
var
  f, fClone: TCanvasFigure;
  i: integer;
begin
  UndoRedoIndex := UndoRedoIndex + 1;
  for i := UndoRedoIndex to High(UndoRedoElements) do
    UndoRedoElements[i] := nil;
  SetLength(UndoRedoElements, UndoRedoIndex+1);
  if Length(FiguresData) <> 0 then
  begin
    for f in FiguresData do
    begin
      fClone := f.ClassType.Create as TCanvasFigure;
      with fClone do
      begin
        fPoints := f.fPoints;
        BrushColor := f.BrushColor;
        PenColor := f.PenColor;
        BrushStyle := f.BrushStyle;
        PenStyle := f.PenStyle;
        Radius := f.Radius;
        Width := f.Width;
      end;
      SetLength(UndoRedoElements[UndoRedoIndex], Length(UndoRedoElements[UndoRedoIndex])+1);
      UndoRedoElements[UndoRedoIndex][Length(UndoRedoElements[UndoRedoIndex])-1] := fClone;
    end;
  end;
end;

procedure Undo();
begin
  if UndoRedoIndex - 1 >= 0 then
  begin
    UndoRedoIndex := UndoRedoIndex - 1;
    FiguresData := UndoRedoElements[UndoRedoIndex];
  end;
end;

procedure Redo();
begin
  if UndoRedoIndex + 1 < Length(UndoRedoElements) then
    begin
      UndoRedoIndex := UndoRedoIndex + 1;
      FiguresData := UndoRedoElements[UndoRedoIndex];
    end;
end;

function CanUndo(): boolean;
begin
  Result := UndoRedoIndex > 0
end;

function CanRedo(): boolean;
begin
  Result := (UndoRedoIndex < Length(UndoRedoElements) - 1) and (UndoRedoIndex >= 0);
end;

begin
  UndoRedoIndex := -1;
end.

