@AccessControl.authorizationCheck: #NOT_ALLOWED
@EndUserText.label: 'Consumption View For ZEHO_I_COMPS'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZEHO_C_COMPS
  //provider contract transactional_query
  as projection on ZEHO_I_COMPS

{
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_BANKCODES',
      element: 'Bankcode'
      }  }]
  key Bankcode,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_CompanyCodeVH',
      element: 'CompanyCode'
      }  }]
  key Bukrs,
      comp_desc,
      LocalLastChangedBy,
      LocalLastChangedAt,
      LastChangedAt,
      SingletonID,
      _Banks     : redirected to parent ZEHO_C_BANK,
      _Singleton : redirected to ZEHO_C_SINGLETON,
      _Acc       : redirected to composition child ZEHO_C_ACC

}
