@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View For ZEHO_I_ACC'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZEHO_C_ACC
  as projection on ZEHO_I_ACC
{
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_BANKCODES',
      element: 'Bankcode'
      }  }]
  key Bankcode,
  key Bukrs,
  key Iban,
  key Branch,
      AcccountNo,
      AccNoServ,
      AccSfxServ,
          @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_Currency',
      element: 'Currency'
      }  }]
      Waers,
      CustNo,
      Username,
      Password,
      ClientId,
      ClientSecret,
      AppKEy,
      UniqueNumber,
     @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_GLAccountInCompanyCodeStdVH',
      element: 'GLAccount'
      } 
//       ,
//      additionalBinding: [{ usage: #FILTER, element: 'CompanyCode', parameter: #(bukrs) }] 
      }]
      Hkont,
      BegdateFormat,
      EnddateFormat,
      SingletonID,
      LocalLastChangedBy,
      LocalLastChangedAt,
      LastChangedAt,
      /* Associations */
      _Comp : redirected to parent ZEHO_C_COMPS,
      _Singleton : redirected to ZEHO_C_SINGLETON
}
