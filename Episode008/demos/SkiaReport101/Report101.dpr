program Report101;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  Sample.Form.PDF.Viewer in 'Sample.Form.PDF.Viewer.pas' {frmPDFViewer};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait];
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmPDFViewer, frmPDFViewer);
  Application.Run;
end.
