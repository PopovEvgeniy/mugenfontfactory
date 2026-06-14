unit mugenfontfactorycode;

{
 This software was made by Popov Evgeniy Alekseyevich.
 It is distributed under the GNU GENERAL PUBLIC LICENSE (Version 2 or higher).
}

{$mode objfpc}
{$H+}

interface

uses Classes, SysUtils, Forms, Controls, Dialogs, ComCtrls, ExtCtrls, StdCtrls, LazFileUtils;

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
    procedure window_setup();
    procedure interface_setup();
    procedure language_setup();
    procedure setup();
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
var message: array[0..7] of string=('The operation was successfully completed','Can not open the input file','Can not create the output file','Can not read data','Can not write data','Can not jump to the target offset','Can not allocate memory','The invalid format');
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

procedure TMainWindow.window_setup();
begin
 Application.Title:='MUGEN FONT FACTORY';
 MainWindow.Caption:='MUGEN FONT FACTORY 2.2.7';
 Self.BorderStyle:=bsDialog;
 Self.Font.Name:=Screen.MenuFont.Name;
 Self.Font.Size:=14;
end;

procedure TMainWindow.interface_setup();
begin
 Self.TxtField.LabelPosition:=lpLeft;
 Self.PcxField.LabelPosition:=lpLeft;
 Self.FntField.LabelPosition:=lpLeft;
 Self.TxtField.Enabled:=False;
 Self.PcxField.Enabled:=False;
 Self.FntField.Enabled:=False;
 Self.CompileButton.Enabled:=False;
 Self.DecompileButton.Enabled:=False;
 Self.TxtField.Text:='';
 Self.PcxField.Text:='';
 Self.FntField.Text:='';
 Self.OpenTxtButton.ShowHint:=False;
 Self.OpenPcxButton.ShowHint:=False;
 Self.CompileButton.ShowHint:=False;
 Self.OpenFntButton.ShowHint:=False;
 Self.DecompileButton.ShowHint:=False;
end;

procedure TMainWindow.language_setup();
begin
 Self.TxtField.EditLabel.Caption:='A text file';
 Self.PcxField.EditLabel.Caption:='A graphic file';
 Self.FntField.EditLabel.Caption:='The font file';
 Self.OpenTxtButton.Caption:='Open';
 Self.OpenPcxButton.Caption:='Open';
 Self.CompileButton.Caption:='Compile';
 Self.OpenFntButton.Caption:='Open';
 Self.DecompileButton.Caption:='Decompile';
 Self.WorkSpace.Pages[0].Caption:='Compilation';
 Self.WorkSpace.Pages[1].Caption:='Decompilation';
end;

procedure TMainWindow.setup();
begin
 Self.window_setup();
 Self.interface_setup();
 Self.language_setup();
end;

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
begin
 Self.setup();
end;

procedure TMainWindow.TxtFieldChange(Sender: TObject);
begin
 Self.CompileButton.Enabled:=(Self.TxtField.Text<>'') and (Self.PcxField.Text<>'');
end;

procedure TMainWindow.PcxFieldChange(Sender: TObject);
begin
 Self.CompileButton.Enabled:=(Self.TxtField.Text<>'') and (Self.PcxField.Text<>'');
end;

procedure TMainWindow.FntFieldChange(Sender: TObject);
begin
 Self.DecompileButton.Enabled:=Self.FntField.Text<>'';
end;

procedure TMainWindow.OpenTxtButtonClick(Sender: TObject);
begin
 Self.OpenDialog.Title:='Open a source text file';
 Self.OpenDialog.DefaultExt:='*.txt';
 Self.OpenDialog.FileName:='*.txt';
 Self.OpenDialog.Filter:='A text files|*.txt';
 if Self.OpenDialog.Execute()=True then
 begin
  Self.TxtField.Text:=Self.OpenDialog.FileName;
 end;

end;

procedure TMainWindow.OpenPcxButtonClick(Sender: TObject);
begin
 Self.OpenDialog.Title:='Open a source graphic file';
 Self.OpenDialog.DefaultExt:='*.pcx';
 Self.OpenDialog.FileName:='*.pcx';
 Self.OpenDialog.Filter:='PC PaintBrush image|*.pcx';
 if Self.OpenDialog.Execute()=True then
 begin
  Self.PcxField.Text:=Self.OpenDialog.FileName;
 end;

end;

procedure TMainWindow.CompileButtonClick(Sender: TObject);
begin
 Self.SaveDialog.Title:='Save a target mugen font';
 Self.SaveDialog.DefaultExt:='*.fnt';
 Self.SaveDialog.FileName:='*.fnt';
 Self.SaveDialog.Filter:='Mugen font|*.fnt';
 if Self.SaveDialog.Execute()=True then
 begin
  compile_font(Self.TxtField.Text,Self.PcxField.Text,Self.SaveDialog.FileName);
 end;

end;

procedure TMainWindow.OpenFntButtonClick(Sender: TObject);
begin
 Self.OpenDialog.Title:='Open a source mugen font';
 Self.OpenDialog.DefaultExt:='*.fnt';
 Self.OpenDialog.FileName:='*.fnt';
 Self.OpenDialog.Filter:='Mugen font|*.fnt';
 if Self.OpenDialog.Execute()=True then
 begin
  Self.FntField.Text:=Self.OpenDialog.FileName;
 end;

end;

procedure TMainWindow.DecompileButtonClick(Sender: TObject);
begin
 decompile_font(Self.FntField.Text);
end;

end.
