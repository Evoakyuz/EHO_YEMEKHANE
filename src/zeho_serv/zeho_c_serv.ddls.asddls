@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View For ZEHO_I_SERV'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZEHO_C_SERV  
as projection on ZEHO_I_SERV
{
    key Bankcode,
//       @Consumption.valueHelpDefinition: [{ entity: { element: 'Bukrs', name: 'ZEHO_VH_BUKRS' } },
//                               { additionalBinding: [{ usage: #FILTER, element: 'Bankcode', parameter: 'bank'}] }]
     @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_BUKRS',
      element: 'Bukrs'
      }  }]
    key Bukrs,
    Host,
    ContentType,
    SoapAction,
    ServUrl,
    SingletonID,
    LocalLastChangedBy,
    LocalLastChangedAt,
    LastChangedAt,
    /* Associations */
    _Banks : redirected to parent ZEHO_C_BANK ,
    _Singleton : redirected to ZEHO_C_SINGLETON
    
}
