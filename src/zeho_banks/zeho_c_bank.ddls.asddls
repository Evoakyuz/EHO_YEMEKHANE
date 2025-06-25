@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View For ZEHO_I_BANKCODE'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define  view entity ZEHO_C_BANK
as projection on ZEHO_I_BANK
//  provider contract transactional_query
//  as projection on ZEHO_I_BANK

{   


  key Bankcode,
      bank_desc,
      LocalLastChangedBy,
      LocalLastChangedAt,
      LastChangedAt,
      SingletonID,
      _Comps : redirected to composition child ZEHO_C_COMPS,
      _Singleton  : redirected to parent ZEHO_C_SINGLETON,
      _Serv  : redirected to composition child ZEHO_C_SERV,
      _ReqM  : redirected to composition child ZEHO_C_REQ_MAPPING,
      _ResM  : redirected to composition child ZEHO_C_RESP_MAPPING
}
