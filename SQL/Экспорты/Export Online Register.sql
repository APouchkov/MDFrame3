
RETURN
EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.Persons',
  @Table                = NULL,
  @IdentityField        = 'Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 0
  --, @BackUpFields = '<FIELD NAME="Test" FUNCTION="[dbo].[Test](&lt;% TABLE %&gt;.[Type_Id])"/>'

EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.Persons',
  @Table                = '[Base].[Persons:Identifiers&Specificators]',
  @IdentityField        = 'Person_Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 1

EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.Persons',
  @Table                = '[Base].[Persons:Addresses]',
  @IdentityField        = 'Person_Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 1

EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.Persons',
  @Table                = '[Base].[Persons:Documents]',
  @IdentityField        = 'Person_Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 1



EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.[Persons:Representatives]',
  @Table                = NULL,
  @IdentityField        = 'Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 0

EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.[Persons:Contracts]',
  @Table                = NULL,
  @IdentityField        = 'Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 0

EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.[Persons:Accounts]',
  @Table                = NULL,
  @IdentityField        = 'Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 0

EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.[Persons:Accounts]',
  @Table                = 'Base.[Persons:Accounts:Trusts]',
  @IdentityField        = 'Account_Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 1

EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.[Persons:Accounts:Parts]',
  @Table                = NULL,
  @IdentityField        = 'Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 0

---

EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.[Instruments]',
  @Table                = NULL,
  @IdentityField        = 'Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 0
  , @BackUpFields = '<FIELD NAME="Type_Id"/><FIELD NAME="Type_Group" FUNCTION="[Base].[Dic::Instruments::Types::Get Group Code]([Type_Id])"/>'

EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.[Instruments]',
  @Table                = 'Base.[Instruments:Currencies]',
  @IdentityField        = 'Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 0

EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.[Instruments]',
  @Table                = 'Base.[Instruments:Derivatives]',
  @IdentityField        = 'Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 0

EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.[Instruments]',
  @Table                = 'Base.[Instruments:Derivatives:Contracts]',
  @IdentityField        = 'Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 0

EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.[Instruments]',
  @Table                = 'Base.[Instruments:Derivatives:Contracts:Sources]',
  @IdentityField        = 'Instrument_Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 1

EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.[Instruments]',
  @Table                = 'Base.[Instruments:Money Actives]',
  @IdentityField        = 'Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 0

EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.[Instruments]',
  @Table                = 'Base.[Instruments:Securities]',
  @IdentityField        = 'Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 0

EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.[Instruments]',
  @Table                = 'Base.[Instruments:Securities:Issues]',
  @IdentityField        = 'Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 0

EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.[Instruments]',
  @Table                = 'Base.[Instruments:Codes]',
  @IdentityField        = 'Instrument_Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 1

---

EXEC [Export].[OnLine::Create Triggers]
  @Object               = 'Base.[Instruments:Securities:Actions]',
  --@Table                = 'Base.[Instruments:Securities:Issues]',
  @IdentityField        = 'Id',
  @IdentityAllowChange  = 0,
  @Distinct             = 0
  , @BackUpFields = '<FIELD NAME="Instrument_Id"/><FIELD NAME="Type_Id"/>'
