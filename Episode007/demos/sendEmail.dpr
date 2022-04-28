program sendEmail;

{$APPTYPE CONSOLE}

uses
  AWS.SESV2,
  System.SysUtils;

var
  Client: ISESV2Client;
  Destination: ISESV2Destination;
  EmailMessage: ISESV2Message;
  Content: ISESV2Content;
  Request: ISESV2SendEmailRequest;
  Response: ISESV2SendEmailResponse;

begin
  try
    Destination := TSESV2Destination.Create;
    Destination.AddToAddress('fernando.rizzato@embarcadero.com');

    EmailMessage := TSESV2Message.Create('Embarcadero DevLounge');
    var sHTML: string := '<h1 style="color: #5e9ca0;">Voc&ecirc; est&aacute; convidado para o DevLounge!</h1><p><strong>&nbsp;</strong></p>';
    Content := TSESV2Content.Create(sHTML, 'UTF-8');
    EmailMessage.Body.Html := Content;

    Request := TSESV2SendEmailRequest.Create;
    Request.FromEmailAddress := 'fernando.rizzato@gmail.com';
    Request.Destination := Destination;
    Request.Content.Simple := EmailMessage;

    Client := TSESV2Client.Create;
    Response := Client.sendEmail(Request);
    writeln('Response.StatusText: ' + Response.StatusText);
    readln;
  except
    on E: Exception do
    begin
      writeln(E.ClassName, ': ', E.Message);
      readln;
    end;
  end;

end.
