@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Debit / Credit Value Help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.resultSet.sizeCategory: #XS
define view entity ZEHO_VH_DEBIT_CREDIT
  as select from DDCDS_CUSTOMER_DOMAIN_VALUE_T( p_domain_name:'ZEHO_DM_DC' )
{
      @UI.hidden: true
  key domain_name,
      @UI.hidden: true
  key value_position,
      @UI.hidden: true
  key language,
//      @UI.lineItem: [{ position: 10  , label: 'ABC'}]
//      @UI.identification: [{ position: 10 , label: 'ABC'  }]
      @ObjectModel.text.element: ['DCText']
   key value_low as dc ,
      //    @UI.hidden: false
      @UI.lineItem: [{ position: 20  , label: 'ABC'}]
      @UI.identification: [{ position: 20 , label: 'ABC' }]
//      @ObjectModel.sort.transformedBy: 'ABAP:ZCL_EHO_SORT_PRIORITY'
      text      as DCText



}
where
  language = $session.system_language
