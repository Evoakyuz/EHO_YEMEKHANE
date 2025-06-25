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
define view entity ZEHO_I_COMPS
  as select from zeho_a_comp
  association to parent ZEHO_I_BANK as _Banks 
  on $projection.Bankcode = _Banks.Bankcode
  
   association [1..1] to ZEHO_I_SINGLETON as _Singleton on $projection.SingletonID = _Singleton.SingletonID
   composition [0..*] of ZEHO_I_ACC as _Acc
  //association to parent ZEHO_I_BANK as _Banks

{
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_BANKCODES',
      element: 'Bankcode'
      }  }]
      key cast(bankcode as bankk preserving type ) as Bankcode,
      key bukrs                                    as Bukrs,
      @Semantics.text: true
      comp_desc,
       1 as SingletonID,
      @Semantics.user.localInstanceLastChangedBy: true
      local_last_changed_by                    as LocalLastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at                    as LocalLastChangedAt,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at                          as LastChangedAt,
      _Banks,
      _Singleton,
      _Acc

}
