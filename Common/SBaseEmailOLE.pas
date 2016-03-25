unit
  SBaseEmailOLE;

interface

type
  TEMailOLEConfig = packed record
    Server            : String;
    ServerPort        : Integer;
    EMail             : String;
    UserName          : String;
    Password          : String;
    ConnectionTimeout : Integer;
    ProxyServer       : String;
    ProxyBypass       : String;
  end;

  procedure SendMail(Config: TEMailOLEConfig; const EMailTo, Subject, TextBody: String; const HTMLBody: String = ''; const FileNames: String = '');

implementation

uses
  Classes, System.Win.ComObj, SysUtils;


const
  cdoSendUsingPickup  = 1;
  cdoSendUsingPort    = 2;

  cdoAnonymous        = 0;
  cdoBasic            = 1;

//  SReportParamMail_Server = 'Server';
//  SReportParamMail_EMail  = 'EMail';
//  SReportParamMail_UserName = 'UserName';
//  SReportParamMail_Password = 'Password';
//  SReportParamMail_ConnectionTimeout = 'ConnectionTimeout';
//  SReportParamMail_ServerPort = 'ServerPort';
//  SReportParamMail_ProxyServer = 'ProxyServer';
//  SReportParamMail_ProxyBypass = 'ProxyBypass';

  // Mail Constants
  SReportMail_OLEClassNameOutlook = 'Outlook.Application';
  SReportMail_OLEClassNameMessage = 'CDO.Message';
  SReportMail_OLEClassNameConfig = 'CDO.Configuration';
  SReportMail_cdoSendUsingMethod = 'http://schemas.microsoft.com/cdo/configuration/sendusing';
  SReportMail_cdoSMTPServer = 'http://schemas.microsoft.com/cdo/configuration/smtpserver';
  SReportMail_cdoSMTPConnectionTimeout = 'http://schemas.microsoft.com/cdo/configuration/smtpconnectiontimeout';
  SReportMail_cdoSMTPServerPort = 'http://schemas.microsoft.com/cdo/configuration/smtpserverport';
  SReportMail_cdoSMTPAuthenticate = 'http://schemas.microsoft.com/cdo/configuration/smtpauthenticate';
  SReportMail_cdoSendUserName = 'http://schemas.microsoft.com/cdo/configuration/sendusername';
  SReportMail_cdoSendPassword = 'http://schemas.microsoft.com/cdo/configuration/sendpassword';
  SReportMail_cdoURLProxyServer = 'http://schemas.microsoft.com/cdo/configuration/urlproxyserver';
  SReportMail_cdoURLProxyBypass = 'http://schemas.microsoft.com/cdo/configuration/urlproxybypass';
  SReportMail_cdoURLGetLatestVersion = 'http://schemas.microsoft.com/cdo/configuration/urlgetlatestversion';

// Mail
function GetMailConfig(const Server, EMailFrom: String; const UserName: String = ''; const Password: String = '';
  ConnectionTimeout: Integer = 60; ServerPort: Integer = 25; const ProxyServer: String = ''; const ProxyBypass: String = ''): TEMailOLEConfig;
begin
  Result.Server := Server;
  Result.UserName := UserName;
  Result.Password := Password;
  Result.ConnectionTimeout := ConnectionTimeout;
  Result.ServerPort := ServerPort;
  Result.ProxyServer := ProxyServer;
  Result.ProxyBypass := ProxyBypass;
end;

procedure SendMail(Config: TEMailOLEConfig; const EMailTo, Subject, TextBody, HTMLBody, FileNames: String);
var
  i: Integer;
  AFileNames: TStrings;
  iMsg, iConf, Flds: OLEVariant;
begin
  iMsg := CreateOLEObject(SReportMail_OLEClassNameMessage);
  iConf := CreateOLEObject(SReportMail_OLEClassNameConfig);
  Flds := iConf.Fields;
  // Server
  Flds.Item(SReportMail_cdoSendUsingMethod) := cdoSendUsingPort;
  Flds.Item(SReportMail_cdoSMTPServer) := Config.Server;
  Flds.Item(SReportMail_cdoSMTPConnectionTimeout) := Config.ConnectionTimeout;
  Flds.Item(SReportMail_cdoSMTPServerPort) := Config.ServerPort;
  // User
  if Trim(Config.UserName) = '' then
    Flds.Item(SReportMail_cdoSMTPAuthenticate) := cdoAnonymous
  else
  begin
    Flds.Item(SReportMail_cdoSendUserName) := Config.UserName;
    Flds.Item(SReportMail_cdoSendPassword) := Config.Password;
  end;
  // Proxy
  if Trim(Config.ProxyServer) <> '' then
  begin
    Flds.Item(SReportMail_cdoURLProxyServer) := Config.ProxyServer;
    Flds.Item(SReportMail_cdoURLProxyBypass) := Config.ProxyBypass;
    Flds.Item(SReportMail_cdoURLGetLatestVersion) := True;
  end;
  Flds.Update;
  // Message
  iMsg.Configuration := iConf;
  iMsg.To := EMailTo;
  iMsg.From := Config.EMail;
  iMsg.Subject := Subject;
  if Trim(HTMLBody) <> '' then
    iMsg.HtmlBody := HTMLBody
  else
    iMsg.TextBody := TextBody;
  if Trim(FileNames) <> '' then
  begin
    AFileNames := TStringList.Create;
    try
      AFileNames.Delimiter := ';';
      AFileNames.DelimitedText := FileNames;
      for i := 0 to Pred(AFileNames.Count) do
        iMsg.AddAttachment(AFileNames[i]);
    finally
      AFileNames.Free;
    end;
  end;
  iMsg.Send;
end;

procedure OpenMail(const EMailTo, Subject, TextBody, HTMLBody, FileNames: String);
var
  i: Integer;
  AFileNames: TStrings;
  OutlookApp, MailItem: OLEVariant;
begin
  OutlookApp := CreateOleObject(SReportMail_OLEClassNameOutlook);
  MailItem := OutlookApp.CreateItem(0);
  MailItem.To := EMailTo;
  MailItem.Subject := Subject;
  MailItem.Body := TextBody;
  MailItem.HTMLBody := HTMLBody;
  if Trim(FileNames) <> '' then
  begin
    AFileNames := TStringList.Create;
    try
      AFileNames.Delimiter := ';';
      AFileNames.DelimitedText := FileNames;
      for i := 0 to Pred(AFileNames.Count) do
        MailItem.Attachments.Add(AFileNames[i], 1, 1);
    finally
      AFileNames.Free;
    end;
  end;
  try
    MailItem.Display;
  except
    // TODO: Error ???!!!
  end;
end;

end.
