@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View - Request Mapping'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZEHO_I_REQ_MAPPING 
as select from zeho_a_req_m
 association to parent ZEHO_I_BANK as _Banks 
  on $projection.Bankcode = _Banks.Bankcode
 
  association [1..1] to ZEHO_I_SINGLETON as _Singleton 
                     on $projection.SingletonID = _Singleton.SingletonID
{   
   @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_BANKCODES',
      element: 'Bankcode'} 
      
       }]
    key cast(bankcode as bankk preserving type ) as Bankcode,
    
    key input_tag as InputTag,
     @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_REQ_FIELDS',
      element: 'fieldname'
      }  ,
      additionalBinding: [{ usage: #FILTER, element: 'tabname',  parameter: 'ZEHO_A_ACC'}] 
      }]
    input_value as InputValue,
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
