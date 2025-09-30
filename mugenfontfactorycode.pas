unit mugenfontfactorycode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, LazFileUtils;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    OpenTxtButton: TButton;
    OpenPcxButton: TButton;
    CompileButton: TButton;
    OpenFntButton: TButton;
    DecompileButton: TButton;
    TxtField: TLabeledEdit;
    PcxField: TLabeledEdit;
    FntField: TLabeledEdit;
    OpenDialog: TOpenDialog;
    WorkSpace: TPageControl;
    SaveDialog: TSaveDialog;
    CompilationSheet: TTabSheet;
    DecompilationSheet: TTabSheet;
    procedure OpenTxtButtonClick(Sender: TObject);
    procedure OpenPcxButtonClick(Sender: TObject);
    procedure CompileButtonClick(Sender: TObject);
    procedure OpenFntButtonClick(Sender: TObject);
    procedure DecompileButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TxtFieldChange(Sender: TObject);
    procedure PcxFieldChange(Sender: TObject);
    procedure FntFieldChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var MainWindow: TMainWindow;

implementation

function convert_file_name(const source:string): string;
var target:string;
begin
 target:=source;
 if Pos(' ',source)>0 then
 begin
  target:='"'+source+'"';
 end;
 convert_file_name:=target;
end;

function execute_program(const executable:string;const argument:string):Integer;
var code:Integer;
begin
 try
  code:=ExecuteProcess(executable,argument,[]);
 except
  code:=-1;
 end;
 execute_program:=code;
end;

procedure run_backend_tool(const arguments:string);
var error:SmallInt;
var message: array[0..5] of string=('The operation was successfully completed','Cant open the input file','Cant create the output file','Cant jump to the target offset','Cant allocate memory','The invalid format');
var job,status:string;
begin
 status:='Cant execute an external program';
 job:=ExtractFilePath(Application.ExeName)+'fntreconstructor.exe';
 error:=execute_program(job,arguments);
 if error>=0 then
 begin
  status:=message[error];
 end;
 ShowMessage(status);
end;

procedure decompile_font(const font:string);
begin
 run_backend_tool(convert_file_name(font));
end;

procedure compile_font(const configuration:string;const graphic:string;const font:string);
var target,job:string;
begin
 target:=ExtractFileNameWithoutExt(font)+'.fnt';
 job:=convert_file_name(graphic)+' '+convert_file_name(configuration)+' '+convert_file_name(target);
 run_backend_tool(job);
end;

procedure window_setup();
begin
 Application.Title:='MUGEN FONT FACTORY';
 MainWindow.Caption:='MUGEN FONT FACTORY 2.2.5';
 MainWindow.BorderStyle:=bsDialog;
 MainWindow.Font.Name:=Screen.MenuFont.Name;
 MainWindow.Font.Size:=14;
end;

procedure interface_setup();
begin
 MainWindow.TxtField.LabelPosition:=lpLeft;
 MainWindow.PcxField.LabelPosition:=MainWindow.TxtField.LabelPosition;
 MainWindow.FntField.LabelPosition:=MainWindow.TxtField.LabelPosition;
 MainWindow.TxtField.Enabled:=False;
 MainWindow.PcxField.Enabled:=MainWindow.TxtField.Enabled;
 MainWindow.FntField.Enabled:=MainWindow.TxtField.Enabled;
 MainWindow.CompileButton.Enabled:=False;
 MainWindow.DecompileButton.Enabled:=MainWindow.CompileButton.Enabled;
 MainWindow.TxtField.Text:='';
 MainWindow.PcxField.Text:=MainWindow.TxtField.Text;
 MainWindow.FntField.Text:=MainWindow.TxtField.Text;
 MainWindow.OpenTxtButton.ShowHint:=False;
 MainWindow.OpenPcxButton.ShowHint:=MainWindow.OpenTxtButton.ShowHint;
 MainWindow.CompileButton.ShowHint:=MainWindow.OpenTxtButton.ShowHint;
 MainWindow.OpenFntButton.ShowHint:=MainWindow.OpenTxtButton.ShowHint;
 MainWindow.DecompileButton.ShowHint:=MainWindow.OpenTxtButton.ShowHint;
end;

procedure language_setup();
begin
 MainWindow.TxtField.EditLabel.Caption:='A text file';
 MainWindow.PcxField.EditLabel.Caption:='A graphic file';
 MainWindow.FntField.EditLabel.Caption:='The font file';
 MainWindow.OpenTxtButton.Caption:='Open';
 MainWindow.OpenPcxButton.Caption:='Open';
 MainWindow.CompileButton.Caption:='Compile';
 MainWindow.OpenFntButton.Caption:='Open';
 MainWindow.DecompileButton.Caption:='Decompile';
 MainWindow.WorkSpace.Pages[0].Caption:='Compilation';
 MainWindow.WorkSpace.Pages[1].Caption:='Decompilation';
end;

procedure setup();
begin
 window_setup();
 interface_setup();
 language_setup();
end;

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
begin
 setup();
end;

procedure TMainWindow.TxtFieldChange(Sender: TObject);
begin
 MainWindow.CompileButton.Enabled:=(MainWindow.TxtField.Text<>'') and (MainWindow.PcxField.Text<>'');
end;

procedure TMainWindow.PcxFieldChange(Sender: TObject);
begin
 MainWindow.CompileButton.Enabled:=(MainWindow.TxtField.Text<>'') and (MainWindow.PcxField.Text<>'');
end;

procedure TMainWindow.FntFieldChange(Sender: TObject);
begin
 MainWindow.DecompileButton.Enabled:=MainWindow.FntField.Text<>'';
end;

procedure TMainWindow.OpenTxtButtonClick(Sender: TObject);
begin
 MainWindow.OpenDialog.Title:='Open a source text file';
 MainWindow.OpenDialog.DefaultExt:='*.txt';
 MainWindow.OpenDialog.FileName:='*.txt';
 MainWindow.OpenDialog.Filter:='A text files|*.txt';
 if MainWindow.OpenDialog.Execute()=True then
 begin
  MainWindow.TxtField.Text:=MainWindow.OpenDialog.FileName;
 end;

end;

procedure TMainWindow.OpenPcxButtonClick(Sender: TObject);
begin
 MainWindow.OpenDialog.Title:='Open a source graphic file';
 MainWindow.OpenDialog.DefaultExt:='*.pcx';
 MainWindow.OpenDialog.FileName:='*.pcx';
 MainWindow.OpenDialog.Filter:='PC PaintBrush image|*.pcx';
 if MainWindow.OpenDialog.Execute()=True then
 begin
  MainWindow.PcxField.Text:=MainWindow.OpenDialog.FileName;
 end;

end;

procedure TMainWindow.CompileButtonClick(Sender: TObject);
begin
 MainWindow.SaveDialog.Title:='Save a target mugen font';
 MainWindow.SaveDialog.DefaultExt:='*.fnt';
 MainWindow.SaveDialog.FileName:='*.fnt';
 MainWindow.SaveDialog.Filter:='Mugen font|*.fnt';
 if MainWindow.SaveDialog.Execute()=True then
 begin
  compile_font(MainWindow.TxtField.Text,MainWindow.PcxField.Text,MainWindow.SaveDialog.FileName);
 end;

end;

procedure TMainWindow.OpenFntButtonClick(Sender: TObject);
begin
 MainWindow.OpenDialog.Title:='Open a source mugen font';
 MainWindow.OpenDialog.DefaultExt:='*.fnt';
 MainWindow.OpenDialog.FileName:='*.fnt';
 MainWindow.OpenDialog.Filter:='Mugen font|*.fnt';
 if MainWindow.OpenDialog.Execute()=True then
 begin
  MainWindow.FntField.Text:=MainWindow.OpenDialog.FileName;
 end;

end;

procedure TMainWindow.DecompileButtonClick(Sender: TObject);
begin
 decompile_font(MainWindow.FntField.Text);
end;

end.
