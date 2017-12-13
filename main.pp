unit Main;

{$MODE OBJFPC}
{$LONGSTRINGS ON}

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
interface
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

uses
  Classes, SysUtils, Graphics, GraphMath, math,
  Forms, Dialogs, Menus, Buttons,
  Controls, ExtCtrls, StdCtrls, Spin,
  EditorTools, ToolsParams, Transform, CanvasFigures;

type

  TPointArray = array of TPoint;

  { TMainForm }

  TMainForm = class(TForm)
		Deselect: TMenuItem;
		DeleteBtn: TMenuItem;
		Background: TMenuItem;
		Forefront: TMenuItem;
		Memo1: TMemo;
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
		procedure MoveBack(Sender: TObject);
		procedure MoveFrfront(Sender: TObject);
		procedure OpenClick(Sender: TObject);
		procedure SaveClickClick(Sender: TObject);
		procedure SelectAllClick(Sender: TObject);
	  procedure spnZoomChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FileConstructor();
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
		procedure VerticalBarScroll(Sender: TObject; ScrollCode: TScrollCode;
			var ScrollPos: Integer);

  strict private
    fCurrentToolClass: TEditorToolClass;
    fCurrentFigureIndex: SizeInt;
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
end;

procedure TMainForm.FileConstructor;
begin

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

procedure TMainForm.MoveBack(Sender: TObject);
begin
  MoveBackground;
  PaintBox.Invalidate();
end;

procedure TMainForm.MoveFrfront(Sender: TObject);
begin
  MoveForefront;
  PaintBox.Invalidate();
end;

procedure TMainForm.OpenClick(Sender: TObject);
var
  PntBox: TStringList;
begin
  PntBox:= TStringList.Create;
  if OpenDialog.Execute then PntBox.LoadFromFile(OpenDialog.FileName);
  PntBox.Free
end;

procedure TMainForm.SaveClickClick(Sender: TObject);
var
  i, j, len:integer;
  Points:TPointArray;
  str, FName: String;
begin
  SaveDialog.FileName := 'My perfect project.json';
  if SaveDialog.Execute then begin
    Memo1.Lines.Clear;
    for i:= 0 to FiguresCount()-1 do
    begin
      Memo1.Lines.Add('{');
      Points := GetFigure(i).GetCanvasPoints();
      len := Length(Points) - 1;
      for j := 0 to len do begin
        str := str + FloatToStr(Points[j].x) + ', ';
        str := str + FloatToStr(Points[j].y);
        if j <> len then
          str := str + ', '
        else
          str := str + ' ]';
			end;
			Memo1.Lines.Add('"Points":[');
      Memo1.Lines.Add(str);//
      Memo1.Lines.Add('}');
		end;
    FName:= SaveDialog.FileName;
    Memo1.Lines.SaveToFile(FName);
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
  PaintBox.Invalidate();
  UnSelectAll();
end;

//end
////////////////////////////////////////////////////////////////////////////////////////////////////
//work with Scroll
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TMainForm.ChangeBorders;
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

procedure TMainForm.HorizontalBarScroll(Sender: TObject;
	ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  ScreenOffset.X := HorizontalBar.Position;
  PaintBox.Invalidate();
end;

procedure TMainForm.VerticalBarScroll(Sender: TObject; ScrollCode: TScrollCode;
	var ScrollPos: Integer);
begin
  ScreenOffset.Y := VerticalBar.Position;
  PaintBox.Invalidate();
end;

procedure TMainForm.SetScrollBar;
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


end.
