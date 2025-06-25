@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Response Mapping'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZEHO_I_RESP_MAPPING
  as select from zeho_a_resp_m
  association        to parent ZEHO_I_BANK as _Banks     on $projection.Bankcode = _Banks.Bankcode

  association [1..1] to ZEHO_I_SINGLETON   as _Singleton on $projection.SingletonID = _Singleton.SingletonID
{
  key bankcode              as Bankcode,
  key resp_tag              as RespTag,
      output_value          as OutputValue,
      1                     as SingletonID,
      @Semantics.user.localInstanceLastChangedBy: true
      local_last_changed_by as LocalLastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at       as LastChangedAt,
      _Banks,
      _Singleton
}
