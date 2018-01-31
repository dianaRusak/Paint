unit Main;

{$MODE OBJFPC}
{$H+}{$R+}
{$LONGSTRINGS ON}

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
interface
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

uses
  Classes, SysUtils, Graphics, GraphMath, math,
  Forms, Dialogs, Menus, Buttons, Fpjson,
  Controls, ExtCtrls, StdCtrls, Spin, UndoRedo,
  EditorTools, ToolsParams, Transform, CanvasFigures,
  LCLType, Clipbrd;


type

  TPointArray = array of TPoint;

  { TMainForm }

  TMainForm = class(TForm)
    Deselect: TMenuItem;
    DeleteBtn: TMenuItem;
    Background: TMenuItem;
    Forefront: TMenuItem;
    Memo1: TMemo;
    miCutSelected: TMenuItem;
    miCopySelected: TMenuItem;
    miPasteSelected: TMenuItem;
    miRedo: TMenuItem;
    miUndo: TMenuItem;
    SaveClick: TMenuItem;
    Open: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SelectAll: TMenuItem;
    spnZoom: TFloatSpinEdit;
    HorizontalBar: TScrollBar;
    VerticalBar: TScrollBar;
    ToolParamsPanel: TPanel;
    ToolBox: TPanel;
    lblTools: TLabel;
    lstTools: TListBox;
    PaintBox: TPaintBox;
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miExit: TMenuItem;
    miEdit: TMenuItem;
    miClearImage: TMenuItem;
    miHelp: TMenuItem;
    miAbout: TMenuItem;

    procedure DeleteBtnClick(Sender: TObject);
    procedure DeselectClick(Sender: TObject);
    procedure miCopySelectedClick(Sender: TObject);
    procedure miCutSelectedClick(Sender: TObject);
    procedure miPasteSelectedClick(Sender: TObject);
    procedure miRedoClick(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure MoveBack(Sender: TObject);
    procedure MoveFrfront(Sender: TObject);
    procedure OpenClick(Sender: TObject);
    procedure SaveClickClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
    procedure spnZoomChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenHere();
    procedure HorizontalBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure lstToolsSelectionChange(Sender: TObject; User: boolean);
    procedure miClearImageClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure ChangeBorders();
    procedure SetScrollBar();
    procedure spnZoomKeyPress(Sender: TObject; var Key: char);
    procedure ToolParamsPanelClick(Sender: TObject);
    procedure VerticalBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    function IntToColor(Value:Integer):TColor;
  strict private
    fCurrentToolClass: TEditorToolClass;
    fCurrentFigureIndex: SizeInt;
    function SaveToJSON(OnlySelected: Boolean): TJSONObject;
    function ReadFromJSON(J: TJSONObject): TCanvasFigures;
  end;

var
  MainForm: TMainForm;
  WorldTopLeft, WorldBottomRight: TFloatPoint;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

implementation

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

{$R *.lfm}


////////////////////////////////////////////////////////////////////////////////////////////////////
// TMainForm
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: SizeInt;
begin
  Caption := Application.Title;

  for i := 0 to EditorToolsCount()-1 do
    lstTools.Items.Add(GetEditorTool(i).GetName());

  lstTools.ItemIndex := 0;
  fCurrentFigureIndex := cFigureIndexInvalid;
  SetScrollBar();
  miUndo.Enabled := CanUndo;
  miRedo.Enabled := CanRedo;
  AddUndoRedo;
end;

procedure TMainForm.OpenHere();
var
  f: TFileStream;
  FName: String;
  FFigureArray: TCanvasFigures;
  J: TJSONObject;
  i: TCanvasFigure;
begin
  if OpenDialog.Execute then
  begin
    FName:= OpenDialog.FileName;
    f := TFileStream.Create(FName, fmOpenRead or fmShareDenyWrite);
    J := TJSONObject(GetJSON(f));
    FFigureArray := ReadFromJSON(J);
    for i in FFigureArray do begin
      AddFigure(i);
    end;
    FreeAndNil(f);
  end;
  Invalidate();
end;

procedure TMainForm.spnZoomChange(Sender: TObject);
var
  ScreenCenter: TFloatPoint;
begin
  ScreenCenter := ScreenToWorld(PaintBox.Width div 2, PaintBox.Height div 2);
  ZoomPoint(ScreenCenter, spnZoom.Value / 100);
  SetScrollBar();
  PaintBox.Invalidate();
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//work with selection
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TMainForm.DeselectClick(Sender: TObject);
begin
  UnSelectAll();
  PaintBox.Invalidate();
end;

procedure TMainForm.miCopySelectedClick(Sender: TObject);
var
  jObject: TJSONObject;
  cfPaint: TClipboardFormat;
  s: TStringStream;
begin
  jObject := SaveToJSON(true);
  Clipboard.Open;
  if jObject.Arrays['pbox'].Count > 0 then
  begin
    cfPaint := Clipboard.FindFormatID('paint_vector');
    if cfPaint = 0 then
      cfPaint := RegisterClipboardFormat('paint_vector');
    s := TStringStream.Create(jObject.AsJSON);
    Clipboard.AddFormat(cfPaint, s);
    FreeAndNil(jObject);
    FreeAndNil(s);
  end;
  Clipboard.Close;
end;

procedure TMainForm.miCutSelectedClick(Sender: TObject);
begin
  miCopySelectedClick(nil);
  DeleteBtnClick(nil);
end;

procedure TMainForm.miPasteSelectedClick(Sender: TObject);
var
  cfPaint: TClipboardFormat;
  s: TStringStream;
  jObject: TJSONObject;
  FFigures: TCanvasFigures;
  FFigure: TCanvasFigure;
begin
  cfPaint := Clipboard.FindFormatID('paint_vector');
  if cfPaint = 0 then
    cfPaint := RegisterClipboardFormat('paint_vector');
  s := TStringStream.Create('');
  if Clipboard.GetFormat(cfPaint, s) then
  begin
    UnSelectAll();
    jObject := TJSONObject(GetJSON(s.DataString));
    FFigures := ReadFromJSON(jObject);
    for FFigure in FFigures do
      if FFigure <> nil then
      begin
        AddFigure(FFigure);
        FFigure.Selected := true;
      end;
    FreeAndNil(jObject);
    FreeAndNil(s);
    AddUndoRedo();
  end;
  Invalidate();
end;

procedure TMainForm.miRedoClick(Sender: TObject);
begin
  if CanRedo then
    Redo;
  miUndo.Enabled := CanUndo;
  miRedo.Enabled := CanRedo;
  Invalidate;
end;

procedure TMainForm.miUndoClick(Sender: TObject);
begin
  if CanUndo then
    Undo;
  miUndo.Enabled := CanUndo;
  miRedo.Enabled := CanRedo;
  Invalidate;
end;

procedure TMainForm.MoveBack(Sender: TObject);
begin
  MoveBackground;
  AddUndoRedo();
  PaintBox.Invalidate();
end;

procedure TMainForm.MoveFrfront(Sender: TObject);
begin
  MoveForefront;
  AddUndoRedo();
  PaintBox.Invalidate();
end;

procedure TMainForm.OpenClick(Sender: TObject);
var
  buttonSelected: integer;
begin
  UnSelectAll();
  Invalidate;
  buttonSelected := MessageDlg(Open.Caption, 'Открыть в этом проекте?',
    mtCustom, [mbYes, mbNo, mbCancel], 0);
  if buttonSelected = mrCancel then
    exit;
  if buttonSelected = mrNo then begin
    ClearFigures();
    PaintBox.Invalidate();
  end;
  OpenHere();
end;


procedure TMainForm.SaveClickClick(Sender: TObject);
var
  jObject: TJSONObject;
  FName: String;
begin
  if SaveDialog.Execute then begin
    jObject := SaveToJSON(false);
    FName:= SaveDialog.FileName;
    Memo1.Lines.Add(jObject.FormatJSON);
    Memo1.Lines.SaveToFile(FName);
    UnSelectAll();
    Invalidate;
  end;
end;

procedure TMainForm.SelectAllClick(Sender: TObject);
begin
  PSelectAll();
  PaintBox.Invalidate();
end;

procedure TMainForm.DeleteBtnClick(Sender: TObject);
begin
  DeleteSelected();
  AddUndoRedo();
  PaintBox.Invalidate();
  UnSelectAll();
end;

//end
////////////////////////////////////////////////////////////////////////////////////////////////////
//work with Scroll
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TMainForm.ChangeBorders();
var
  i: SizeInt;
begin
  if FiguresCount() = 0 then
  begin
    WorldTopLeft.x := 0;
    WorldTopLeft.y := 0;
    WorldBottomRight.x := 0;
    WorldBottomRight.y := 0;
    exit;
	end;
  WorldTopLeft.x := GetFigure(0).TopLeft().x;
  WorldBottomRight.x := GetFigure(0).BottomRight().x;
  WorldTopLeft.y := GetFigure(0).TopLeft().y;
  WorldBottomRight.y := GetFigure(0).BottomRight().y;
	for i := 1 to FiguresCount()-1 do
  begin
    WorldTopLeft.x := min (WorldTopLeft.x, GetFigure(i).TopLeft().x);
    WorldTopLeft.y := min (WorldTopLeft.y, GetFigure(i).TopLeft().y);
    WorldBottomRight.x := max (WorldBottomRight.x, GetFigure(i).BottomRight().x);
    WorldBottomRight.y := max (WorldBottomRight.y, GetFigure(i).BottomRight().y);
	end;
end;

procedure TMainForm.HorizontalBarScroll(Sender: TObject;ScrollCode: TScrollCode;
var ScrollPos: Integer);
begin
  ScreenOffset.X := HorizontalBar.Position;
  PaintBox.Invalidate();
end;

procedure TMainForm.VerticalBarScroll(Sender: TObject; ScrollCode: TScrollCode;
	var ScrollPos: Integer);
begin
  ScreenOffset.Y := VerticalBar.Position;
  Invalidate();
end;

function TMainForm.IntToColor(Value: Integer): TColor;
begin
  Result:=Value;
end;

function TMainForm.SaveToJSON(OnlySelected: Boolean): TJSONObject;
var
  i, j, len:integer;
  Points:TPointArray;
  jFigure: TJSONObject;
  jArray, jSecArray: TJSONArray;
begin
  Result := TJSONObject.Create;
  Result.Add('pbox', TJSONArray.Create);
  for i:= 0 to FiguresCount()-1 do
    if (not OnlySelected) or GetFigure(i).Selected then begin
      jFigure := TJSONObject.Create;
      jFigure.Add('FigureClass', GetFigure(i).ClassName);
      Points := GetFigure(i).GetCanvasPoints();
      len := Length(Points) - 1;
      jArray := TJSONArray.Create;
      for j := 0 to len do begin
        jSecArray := TJSONArray.Create;
        jSecArray.Add(Points[j].x);
        jSecArray.Add(Points[j].y);
        jArray.Add(jSecArray);
      end;
      jFigure.Add('Points',jArray);
      if (GetFigure(i).ClassName <> 'FFigureEmpty')
      and (GetFigure(i).ClassName <> 'FFigureAllotment') then begin
        jFigure.Add('FigurePenColor', GetFigure(i).PenColor);
        jFigure.Add('FigurePenStyle',  StyleToNum(GetFigure(i).PenStyle));
        jFigure.Add('FigureWidth', GetFigure(i).Width);
        jFigure.Add('FigureBrushColor', GetFigure(i).BrushColor);
        jFigure.Add('FigureBrushStyle',BrushStyleToNum(GetFigure(i).BrushStyle));
        jFigure.Add('FigureRadius', GetFigure(i).Radius);
      end;
      Result.Arrays['pbox'].Add(jFigure);
    end;
end;

function TMainForm.ReadFromJSON(J: TJSONObject): TCanvasFigures;
var
  i, k:integer;
  ClassNames: String;
  jArray, jPoints: TJSONArray;
begin
    jArray := TJSONArray(J.FindPath('pbox'));
    SetLength(Result, jArray.Count);
    for i := 0 to jArray.Count - 1 do begin
      ClassNames := jArray.Objects[i].FindPath('FigureClass').AsString;
      Result[i] := StrToClassFigure(ClassNames).Create();
      jPoints := TJSONArray(jArray.Objects[i].FindPath('Points'));
      for k := 0 to jPoints.Count-1 do begin
        Result[i].AddPoint(FloatPoint(jPoints.Arrays[k].Floats[0],
        jPoints.Arrays[k].Floats[1]));
      end;
        Result[i].PenColor :=
           jArray.Objects[i].FindPath('FigurePenColor').AsInteger;
         Result[i].BrushColor :=
           jArray.Objects[i].FindPath('FigureBrushColor').AsInteger;
         Result[i].Width :=
           jArray.Objects[i].FindPath('FigureWidth').AsInteger;
         Result[i].Radius :=
           jArray.Objects[i].FindPath('FigureRadius').AsInteger;
         Result[i].PenStyle :=
           NumToStyle(jArray.Objects[i].FindPath('FigurePenStyle').AsInteger);
         Result[i].BrushStyle :=
           NumToBrushStyle(jArray.Objects[i].FindPath('FigureBrushStyle').AsInteger);
    end;
end;

procedure TMainForm.SetScrollBar();
var
  ScreenRightEnd: TFloatPoint;
  HorizMin, HorizMax, VertMin, VertMax: integer;
begin
  ScreenRightEnd := ScreenToWorld(PaintBox.Width, PaintBox.Height);
  HorizMin := Round(Min(ScreenOffset.x, 0));
  HorizMax := Round(Max(ScreenRightEnd.x, PaintBox.Width));
  VertMin := Round(Min(ScreenOffset.y, 0));
  VertMax := Round(Max(ScreenRightEnd.y, PaintBox.Height));

  if FiguresCount() > 0 then
  begin
    ChangeBorders();
    HorizMin := Min(HorizMin, Round(WorldTopLeft.x));
    VertMin := Min(VertMin, Round(WorldTopLeft.y));
    HorizMax := Max(HorizMax, Round(WorldBottomRight.x));
    VertMax := Max(VertMax, Round(WorldBottomRight.y));
  end;

  HorizontalBar.SetParams(Round(ScreenOffset.x), HorizMin, HorizMax,
  Round(PaintBox.Width / Zoom));
  VerticalBar.SetParams(Round(ScreenOffset.y), VertMin, VertMax,
  Round(PaintBox.Height / Zoom));
end;

//end
////////////////////////////////////////////////////////////////////////////////////////////////////
//work with mouse

procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin

  SetButton(Button);
  if Button = mbLeft then begin

    if fCurrentToolClass = nil then
      exit;

    if ((FCurrentToolClass) <> (TToolZoom))
    and ((FCurrentToolClass) <> (TToolAllocator))
    and ((FCurrentToolClass) <> (TToolHand))
    and ((FCurrentToolClass) <> (TToolCursor))then
      UnSelectAll();

    fCurrentFigureIndex := AddFigure(fCurrentToolClass.GetFigureClass());
    fCurrentToolClass.SetFigureParams(fCurrentFigureIndex);
    fCurrentToolClass.Start(fCurrentFigureIndex, Point(X, Y));
    Invalidate();
    SetScrollBar();
  end
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if fCurrentToolClass = nil then
    exit;
  if fCurrentToolClass.Update(fCurrentFigureIndex, Point(X, Y)) then
    Invalidate();
  SetScrollBar();
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if fCurrentToolClass = nil then
    exit;
  if Button = mbLeft then
  begin
    if fCurrentToolClass.Finish(fCurrentFigureIndex) then begin
      fCurrentFigureIndex := cFigureIndexInvalid;
      Invalidate();
    end;
  end;
  miUndo.Enabled := CanUndo;
  miRedo.Enabled := CanRedo;
end;

//end
////////////////////////////////////////////////////////////////////////////////////////////////////
//workplace
////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  i: SizeInt;
	count: Integer;
begin
  spnZoom.Value := double(Zoom * 100);
  PaintBox.Canvas.Clear();
  for i := 0 to FiguresCount()-1 do
    if GetFigure(i) <> nil then
      GetFigure(i).Draw(PaintBox.Canvas);
  for count:= 0 to FiguresCount()-1 do begin
    GetFigure(count).DrawSelection(PaintBox.Canvas);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TMainForm.lstToolsSelectionChange(Sender: TObject; User: boolean);
var
  i: TToolParam;
  l: TLabel;
begin
  fCurrentToolClass := GetEditorTool(lstTools.ItemIndex);
  ToolParamsPanel.DestroyComponents;
  for i in fCurrentToolClass.Params do
  begin
    ToolParamsPanel.Visible := False;
    i.ToControl(ToolParamsPanel).Align := alBottom;
    l := TLabel.Create(ToolParamsPanel);
    l.Parent := ToolParamsPanel;
    l.Caption := i.Name;
    l.Align := alBottom;
    ToolParamsPanel.Visible := True;
    PaintBox.Invalidate();
	end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//mi click
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TMainForm.miClearImageClick(Sender: TObject);
begin
  if (MessageDlg('Вы уверены, что хотите очистить изображение?',
      mtConfirmation, mbYesNo, 0) = mrYes) then
  begin
    ClearFigures();
    PaintBox.Invalidate();
  end;
  AddUndoRedo();
end;

procedure TMainForm.miAboutClick(Sender: TObject);
begin
  MessageDlg(miAbout.Caption,
    Application.Title + ' - Векторный графический редактор.' + LineEnding +
    'Русак Диана Игоревна, Б8103а, ДВФУ, 2017 год.',
    mtInformation, [mbOK], 0
  );
end;

procedure TMainForm.spnZoomKeyPress(Sender: TObject; var Key: char);
begin
  Key:=#0;
end;

procedure TMainForm.ToolParamsPanelClick(Sender: TObject);
begin

end;

end.
