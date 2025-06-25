@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View - Activities'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZEHO_I_ACTIVITIES
  as select from    zeho_a_aa      as aa
    left outer join zeho_a_log     as log     on  log.bankcode    = aa.bankcode
                                              and log.bukrs       = aa.bukrs
                                              and log.iban        = aa.iban
                                              and log.branch      = aa.branch
                                              and log.acccount_no = aa.acccount_no
                                              and log.act_date    = aa.act_date
                                              and log.act_time    = aa.act_time
                                              and log.act_no      = aa.act_no
    left outer join zeho_a_actcust as actcust on  actcust.bankcode    = aa.bankcode
                                              and actcust.bukrs       = aa.bukrs
                                              and actcust.iban        = aa.iban
                                              and actcust.branch      = aa.branch
                                              and actcust.acccount_no = aa.acccount_no
                                              and actcust.act_date    = aa.act_date
                                              and actcust.act_time    = aa.act_time
                                              and actcust.act_no      = aa.act_no
    left outer join zeho_a_acc     as acc     on  acc.bankcode = aa.bankcode
                                              and acc.bukrs    = aa.bukrs
                                              and acc.iban     = aa.iban
                                              and acc.branch   = aa.branch
  composition [0..*] of ZEHO_I_ACTIVITIES_MULT as _Mult
  //composition of target_data_source_name as _association_name
{

      //   key client          ,
  key aa.bankcode,
  key aa.bukrs,
  key aa.iban,
  key aa.branch,
  key aa.acccount_no,
  key aa.act_date,
  key aa.act_time,
  key aa.act_no,
      //      aa.act_date, // sıra değişti
      //      aa.act_time,
      aa.description,
      aa.shkzg,
      @Semantics.amount.currencyCode : 'WAERS'
      aa.amount,
      @Semantics.amount.currencyCode : 'WAERS'
      aa.instant_amount,
      aa.waers,
      aa.activity_type,
      aa.activity_explanation,
      aa.sender_iban,
      aa.sender_vkn,
      aa.debited_vkn,
      aa.sender_name,
      aa.sender_bank,
      aa.customer_ref,
      acc.hkont,
      log.belnr,
      log.gjahr,
      //      actcust.blart ,

      case
      when log.blart is not initial then  log.blart
      else actcust.blart
      end                as blart,


      case
      when log.lifnr is not initial then  log.lifnr
      else actcust.lifnr
      end                as lifnr,

      case
       when log.kunnr <> ' ' then  log.kunnr
       else actcust.kunnr
       end               as kunnr,

      case
       when log.name1 is not initial then  log.name1
       else actcust.name1
       end               as name1,

      case
       when log.secondgl_acc is not initial then  log.secondgl_acc
       else actcust.secondgl_acc
       end               as secondgl_acc,

      case
       when log.kostl is not initial then  log.kostl
       else actcust.kostl
       end               as kostl,

      case
       when log.prctr is not initial then  log.prctr
       else actcust.prctr
       end               as prctr,


      case
       when log.gsber is not initial then  log.gsber
       else actcust.gsber
       end               as gsber,


      case
       when log.umskz is not initial then  log.umskz
       else actcust.umskz
       end               as umskz,

      case
      when log.mwskz is not initial then  log.mwskz
      else actcust.mwskz
      end                as mwskz,

      case
      when log.cancel_process is not initial then  log.cancel_process
      else actcust.cancel_process
      end                as cancel_process,

      case
      when log.customization_type is not initial then  log.customization_type
      else actcust.customization_type
      end                as customization_type,

      case
      when log.affacted_priority is not initial then  log.affacted_priority
      else actcust.affacted_priority
      end                as affacted_priority,

      @Semantics.amount.currencyCode: 'waers'
      case
      when  aa.waers  <> 'TRY' then currency_conversion( amount => aa.amount,
                                           source_currency => aa.waers ,
                                           target_currency => cast( 'TRY' as abap.cuky( 5 )),
                                           exchange_rate_date => aa.act_date )
      else aa.amount end as local_amount,

      @UI.lineItem: [{ hidden: true  }]
      @UI.identification: [{ hidden: true  }]
      cast ( case 
      when  log.belnr is not initial then '3'
      when  log.cancel_process is not initial then '1'
      when  actcust.cancel_process is not initial then '1'
      else '2' end as abap.char(1) ) as Status ,
//      @ObjectModel.text.element: [ 'StatusText' ]
       cast ( case 
      when  log.belnr is not initial then 'Saved'
      when  log.cancel_process is not initial then 'Canceled'
      when  actcust.cancel_process is not initial then 'Canceled'
      else 'Not Processed' end as abap.char(15) ) as StatusText ,  
      
      _Mult








}
