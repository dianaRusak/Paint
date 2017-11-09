unit Main;

{$MODE OBJFPC}
{$LONGSTRINGS ON}

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
interface
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

uses
  Classes, SysUtils, Graphics,
  Forms, Dialogs, Menus, Buttons, ColorBox,
  Controls, ExtCtrls, StdCtrls, ComCtrls,
  EditorTools;

type

  { TMainForm }

  TMainForm = class(TForm)
    ToolBox: TPanel;
    lblTools: TLabel;
    lstTools: TListBox;
    lblPenColor: TLabel;
    clrPenColor: TColorBox;
    lblPenStyle: TLabel;
    cmbPenStyle: TComboBox;
    lblWidth: TLabel;
    trkWidth: TTrackBar;
    lblBrushColor: TLabel;
    clrBrushColor: TColorBox;
    lblBrushStyle: TLabel;
    cmbBrushStyle: TComboBox;
    chkTransparent: TCheckBox;
    PaintBox: TPaintBox;
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miExit: TMenuItem;
    miEdit: TMenuItem;
    miClearImage: TMenuItem;
    miHelp: TMenuItem;
    miAbout: TMenuItem;

    procedure FormCreate(Sender: TObject);

    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);

    procedure lstToolsSelectionChange(Sender: TObject; User: boolean);
    procedure miClearImageClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);

  strict private
    fCurrentToolClass: TEditorToolClass;
    fCurrentFigureIndex: SizeInt;
  end;

var
  MainForm: TMainForm;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
implementation
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

{$R *.lfm}

uses
  CanvasFigures;

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
  fCurrentToolClass := GetEditorTool(lstTools.ItemIndex);

  fCurrentFigureIndex := cFigureIndexInvalid;

  for i := Low(cPenStylesTable) to High(cPenStylesTable) do
    cmbPenStyle.Items.Add(cPenStylesTable[i].Name);
  for i := Low(cBrushStylesTable) to High(cBrushStylesTable) do
    cmbBrushStyle.Items.Add(cBrushStylesTable[i].Name);

  cmbPenStyle.ItemIndex := 0;
  cmbBrushStyle.ItemIndex := 0;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  fCurrentFigureIndex := AddFigure(fCurrentToolClass.GetFigureClass());

  with GetFigure(fCurrentFigureIndex) do begin
    Width := trkWidth.Position;
    PenStyle := cPenStylesTable[cmbPenStyle.ItemIndex].Style;
    PenColor := clrPenColor.Selected;
    BrushStyle := cBrushStylesTable[cmbBrushStyle.ItemIndex].Style;
    BrushColor := clrBrushColor.Selected;
    Transparent := chkTransparent.Checked;
  end;

  ToolBox.Enabled := False;
  miEdit.Enabled := False;

  fCurrentToolClass.Start(fCurrentFigureIndex, Point(X, Y));
  PaintBox.Invalidate();
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  //if (X < 0) or (Y < 0) or (X > PaintBox.Width) or (Y > PaintBox.Height) then
  //  Exit;

  if fCurrentToolClass.Update(fCurrentFigureIndex, Point(X, Y)) then
    PaintBox.Invalidate();
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (not fCurrentToolClass.Step(fCurrentFigureIndex, Point(X, Y))) or
     (Button = mbRight) then
  begin
    if fCurrentToolClass.Finish(fCurrentFigureIndex) then begin
      fCurrentFigureIndex := cFigureIndexInvalid;
      ToolBox.Enabled := True;
      miEdit.Enabled := True;
      PaintBox.Invalidate();
    end;
  end;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  i: SizeInt;
begin
  PaintBox.Canvas.Clear();
  for i := 0 to FiguresCount()-1 do
    GetFigure(i).Draw(PaintBox.Canvas);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TMainForm.lstToolsSelectionChange(Sender: TObject; User: boolean);
begin
  fCurrentToolClass := GetEditorTool(lstTools.ItemIndex);
end;

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

end.
