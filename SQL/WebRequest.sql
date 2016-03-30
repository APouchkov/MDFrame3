DECLARE
	@ResponseStatus [smallint],
	@ResponseHeader [nvarchar](max),
	@ResponseBody [nvarchar](max)

EXEC [System].[WebRequest*Procedure]
  @Url          = 'https://api.open-broker.ru/v1/megafonscoring',
  @Method       = 'POST',
  @ContentType  = 'application/json;charset=utf-8',
  @Cookies      = NULL,
  @Header       = NULL,
  @Params       = N'{
  "id": "107821992071",
  "data": {
    "msisdn": "79261311299",
    "variables": [
      {
        "name": "NAME_CORRECTNESS"
      }
    ],
    "attributes": [
      {
        "name": "FULLNAME",
        "value": {
          "name": "ִלטענטי",
          "surname": "",
          "patronymic": ""
        }
      }
    ]
  }
}',
  @ResponseStatus = @ResponseStatus OUT,
  @ResponseHeader = @ResponseHeader OUT,
  @ResponseBody   = @ResponseBody   OUT


SELECT @ResponseStatus, @ResponseHeader, @ResponseBody



--{
--  "response":
--  {
--    "data":
--    {
--      "msisdn":"79261311299",
--      "variables":
--      [
--        {
--          "name":"NAME_CORRECTNESS","status":"SUCCESS","type":"DOUBLE","value":"0.0"
--        }
--      ]
--    },
--    "id":"107821992071"
--  }
--}
