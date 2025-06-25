@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_ALLOWED
@EndUserText.label: 'Company Codes'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
serviceQuality: #X,
sizeCategory: #S,
dataClass: #MIXED
}
@Metadata.allowExtensions: true
@ObjectModel.dataCategory: #TEXT
define view entity ZEHO_I_SERV 
as select from zeho_a_serv
  association to parent ZEHO_I_BANK as _Banks 
  on $projection.Bankcode = _Banks.Bankcode
  association [1..1] to ZEHO_I_SINGLETON as _Singleton on $projection.SingletonID = _Singleton.SingletonID
{
      key cast(bankcode as bankk preserving type ) as Bankcode,
    
//    @Consumption.valueHelpDefinition: [{ entity: { element: 'Bukrs', name: 'ZEHO_VH_BUKRS' } },
//                                  { additionalBinding: [{ usage: #FILTER, element: 'bankcode', parameter: 'bank'}] }]
   
    @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_BUKRS',
      element: 'Bukrs'
      }  }]
    key cast(bukrs as bukrs preserving type ) as Bukrs,
    host as Host,
    content_type as ContentType,
    soap_action as SoapAction,
    @Semantics.text: true
    serv_url as ServUrl,
    1 as SingletonID,
      @Semantics.user.localInstanceLastChangedBy: true
      local_last_changed_by                    as LocalLastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at                    as LocalLastChangedAt,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at                          as LastChangedAt,
    _Banks,
    _Singleton
    
}
