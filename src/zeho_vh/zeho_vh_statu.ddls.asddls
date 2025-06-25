
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Debit / Credit Value Help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@VDM.viewType: #BASIC
@ObjectModel.dataCategory: #VALUE_HELP
@ObjectModel.representativeKey: 'Status'
@ObjectModel.supportedCapabilities: [#SQL_DATA_SOURCE,
                                     #CDS_MODELING_DATA_SOURCE,
                                     #CDS_MODELING_ASSOCIATION_TARGET,
                                     #VALUE_HELP_PROVIDER,
                                     #SEARCHABLE_ENTITY]
@Consumption.ranked: true
//@ObjectModel.modelingPattern: #NONE
//@ObjectModel.usageType.serviceQuality: #B
//@ObjectModel.usageType.sizeCategory: #XL
//@ObjectModel.usageType.dataClass: #MASTER

@ObjectModel.resultSet.sizeCategory: #XS
define view entity ZEHO_VH_STATU
 as select from DDCDS_CUSTOMER_DOMAIN_VALUE_T( p_domain_name:'ZEHO_DM_STATUTEXT' )
{
          @UI.hidden: true
  key domain_name,
      @UI.hidden: true
  key value_position,
      @UI.hidden: true
  key language,
//      @UI.lineItem: [{ position: 10  , label: 'ABC'}]
//      @UI.identification: [{ position: 10 , label: 'ABC'  }]
      @ObjectModel.text.element: ['StatuText']
   key value_low as Status ,
      //    @UI.hidden: false
//      @UI.lineItem: [{ position: 20  , label: 'ABC'}]
//      @UI.identification: [{ position: 20 , label: 'ABC' }]
//      @ObjectModel.sort.transformedBy: 'ABAP:ZCL_EHO_SORT_PRIORITY'
      text      as StatuText
}where
  language = $session.system_language
//  and value_low <> 'A'
