program wcradioemmiter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms, main
  { you can add units after this };

{$R *.res}

{$ifdef DEBUG}
const
  cHeapTrace = 'heaptrace.trc';
{$endif}

begin
  {$ifdef DEBUG}
  if FileExists(cHeapTrace) then
     DeleteFile(cHeapTrace);
  SetHeapTraceOutput(cHeapTrace);
  {$endif}
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

