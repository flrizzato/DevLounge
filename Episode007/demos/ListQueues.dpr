program ListQueues;

{$APPTYPE CONSOLE}

uses
  AWS.SQS,
  System.SysUtils;

var
  SQS: ISQSClient;

begin
  try
    SQS := TSQSClient.Create;
    var Response := SQS.ListQueues;
    writeln('Response.StatusText: ' + Response.StatusText);

    if Response.QueueUrls.Count > 0 then
      for var QueueUrl in Response.QueueUrls do
        writeln('QueueUrl: ' + QueueUrl)
    else
      writeln('There is no queues to display...');

    var Status := SQS.SendMessage
      ('https://sqs.us-east-2.amazonaws.com/292598732169/Queue101',
      'Test Message...');
    writeln('StatusText: ' + Status.StatusText);

    readln;
  except
    on E: Exception do
     begin
      writeln(E.ClassName, ': ', E.Message);
      readln;
     end;
  end;

end.
