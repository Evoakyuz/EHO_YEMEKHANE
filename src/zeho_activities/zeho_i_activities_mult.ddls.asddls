@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View - Activities Multiple P'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZEHO_I_ACTIVITIES_MULT
  as select from zeho_a_aa_mult 
  association        to parent ZEHO_I_ACTIVITIES as Activities on  $projection.bankcode    = Activities.bankcode
                                                                and $projection.bukrs       = Activities.bukrs
                                                                and $projection.iban        = Activities.iban
                                                                and $projection.branch      = Activities.branch
                                                                and $projection.acccount_no = Activities.acccount_no
                                                                and $projection.act_date    = Activities.act_date
                                                                and $projection.act_time    = Activities.act_time
                                                                and $projection.act_no      = Activities.act_no
//  association [1..1] to ZEHO_I_ACTIVITIES        as _Activities  on  $projection.bankcode    = _Singleton.bankcode
//                                                                and $projection.bukrs       = _Singleton.bukrs
//                                                                and $projection.iban        = _Singleton.iban
//                                                                and $projection.branch      = _Singleton.branch
//                                                                and $projection.acccount_no = _Singleton.acccount_no
//                                                                and $projection.act_date    = _Singleton.act_date
//                                                                and $projection.act_time    = _Singleton.act_time
//                                                                and $projection.act_no      = _Singleton.act_no
  //composition of ZEHO_I_ACTIVITIES as _association_name
{
  key bankcode,
  key bukrs,
  key iban,
  key branch,
  key acccount_no,
  key act_date,
  key act_time,
  key act_no,
  key item_no,
      //      description       ,
     
      shkzg,
      @Semantics.amount.currencyCode: 'waers'
      amount,
      //       @Semantics.amount.currencyCode: 'waers'
      //      instant_amount    ,
      waers,
      //      activity_type     ,
      //      belnr             ,
      //      gjahr             ,
      blart,
      lifnr,
      kunnr,
      name1,
      secondgl_acc,
      kostl,
      prctr,
      gsber,
      umskz,
      mwskz,
      //      cancel_process    ,
      //      customization_type,
      //      affacted_priority ,
      Activities // Make association public
//     _Singleton 
}
