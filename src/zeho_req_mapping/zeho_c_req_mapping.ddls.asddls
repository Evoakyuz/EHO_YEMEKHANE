@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View For ZEHO_I_REQ_MAPPING'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZEHO_C_REQ_MAPPING
  as projection on ZEHO_I_REQ_MAPPING
{

      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_BANKCODES',
      element: 'Bankcode'
      }  }]
  key Bankcode,
  key InputTag,
       @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_REQ_FIELDS',
      element: 'fieldname'
      }  }]
      InputValue,
      SingletonID,
      LocalLastChangedBy,
      LocalLastChangedAt,
      LastChangedAt,
      /* Associations */
      _Banks     : redirected to parent ZEHO_C_BANK,
      _Singleton : redirected to ZEHO_C_SINGLETON
}
