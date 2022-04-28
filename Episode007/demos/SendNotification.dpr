program SendNotification;

{$APPTYPE CONSOLE}

uses
  AWS.SNS,
  System.SysUtils;

var
  SNSClient: ISNSClient;
  Request: ISNSPublishRequest;

begin
  try
    SNSClient := TSNSClient.Create;
    Request := TSNSPublishToPhoneNumberRequest.Create('+5511982100319',
      'Obrigado por partipar do DevLounge!');

    var Response := SNSClient.Publish(Request);
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
