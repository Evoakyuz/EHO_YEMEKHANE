@AccessControl.authorizationCheck: #NOT_ALLOWED
@EndUserText.label: 'Interface View - Explanation'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZEHO_I_EXP as select from zeho_a_exp
//composition of target_data_source_name as _association_name
{
    key cast(bankcode as bankk preserving type )  as Bankcode,
    key cast(bukrs as bukrs preserving type ) as Bukrs,
    key activity_type as ActivityType,
    key explanation as Explanation,
    key dc as Dc,
    blart as Blart,
    lifnr as Lifnr,
    kunnr as Kunnr,
    secondgl_acc as SecondglAcc,
    kostl as Kostl,
    prctr as Prctr,
    gsber as Gsber,
    umskz as Umskz,
    mwskz as Mwskz,
    cancel_process as CancelProcess
//    _association_name // Make association public
}
