@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View For ZEHO_I_RESP_MAPPING'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZEHO_C_RESP_MAPPING as projection on ZEHO_I_RESP_MAPPING
{
    @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_BANKCODES',
      element: 'Bankcode'
    }  }]
    key Bankcode,
    key RespTag,
    OutputValue,
    SingletonID,
    LocalLastChangedBy,
    LocalLastChangedAt,
    LastChangedAt,
    /* Associations */
   _Banks : redirected to parent ZEHO_C_BANK,
   _Singleton : redirected to ZEHO_C_SINGLETON
}
