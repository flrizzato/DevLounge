program ListBuckets;

{$APPTYPE CONSOLE}

uses
  AWS.S3,
  System.SysUtils;

var
  S3Client: IS3Client;

begin
  try
    S3Client := TS3Client.Create;
    var Response := S3Client.ListBuckets;
    for var BucketName in Response.Buckets do
      Writeln('BucketName.Name:' + BucketName.Name);
    readln;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      readln;
    end;
  end;

end.
