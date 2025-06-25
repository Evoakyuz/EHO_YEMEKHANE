@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View - Accounts'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
serviceQuality: #X,
sizeCategory: #S,
dataClass: #MIXED
}
@Metadata.allowExtensions: true
@ObjectModel.dataCategory: #TEXT
define view entity ZEHO_I_ACC
  as select from zeho_a_acc
  association        to parent ZEHO_I_COMPS as _Comp      on  $projection.Bankcode = _Comp.Bankcode
                                                          and $projection.Bukrs    = _Comp.Bukrs
  association [1..1] to ZEHO_I_SINGLETON    as _Singleton on  $projection.SingletonID = _Singleton.SingletonID
{
      @Consumption.valueHelpDefinition: [{ entity: {
         name: 'ZEHO_VH_BANKCODES',
         element: 'Bankcode'
         }  }]
  key cast(bankcode as bankk preserving type ) as Bankcode,
  key cast(bukrs as bukrs preserving type )    as Bukrs,
  key iban                                     as Iban,
  key branch                                   as Branch,
      acccount_no                              as AcccountNo,
      acc_no_serv                              as AccNoServ,
      acc_sfx_serv                             as AccSfxServ,
      waers                                    as Waers,
      cust_no                                  as CustNo,
      username                                 as Username,
      password                                 as Password,
      client_id                                as ClientId,
      client_secret                            as ClientSecret,
      api_key                                  as AppKEy,
      unique_number                            as UniqueNumber,
//        @Consumption.valueHelpDefinition: [{ entity: {
//         name: 'I_GLAccountInCompanyCode',
//         element: 'Bankcode'
//         }  }]
        @Consumption.valueHelpDefinition: [{ entity: {
         name: 'I_GLAccountInCompanyCodeStdVH',
         element: 'GLAccount'
      }  
//      ,
//      additionalBinding: [{ usage: #FILTER, element: 'CompanyCode', parameter: #(bukrs) }] 
      }]
      hkont                                    as Hkont,
      begdate_format                           as BegdateFormat,
      enddate_format                           as EnddateFormat,
      1                                        as SingletonID,
      @Semantics.user.localInstanceLastChangedBy: true
      local_last_changed_by                    as LocalLastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at                    as LocalLastChangedAt,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at                          as LastChangedAt,
      _Comp,
      _Singleton
}
