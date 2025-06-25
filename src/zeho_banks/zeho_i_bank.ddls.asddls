@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_ALLOWED
@EndUserText.label: 'Bankcode View'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.dataCategory: #TEXT
define  view entity ZEHO_I_BANK
 as select from zeho_a_bank 
 association to parent ZEHO_I_SINGLETON as _Singleton
                    on  $projection.SingletonID = _Singleton.SingletonID
 composition [0..*] of ZEHO_I_COMPS as _Comps
 composition [0..*] of ZEHO_I_SERV as _Serv
 composition [0..*] of ZEHO_I_REQ_MAPPING as _ReqM
 composition [0..*] of ZEHO_I_RESP_MAPPING as _ResM
 //composition [0..*] of ZEHO_I_COMPS as _Comps

{   

    key bankcode as Bankcode,
     @Semantics.text: true
       bank_desc,
       @Semantics.user.localInstanceLastChangedBy: true
      local_last_changed_by as LocalLastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at       as LastChangedAt,
       1 as SingletonID,
      _Comps,
      _Singleton,
      _Serv,
      _ReqM, // request mapping
      _ResM  // resposne mapping


}
