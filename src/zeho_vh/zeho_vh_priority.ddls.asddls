@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Priority Value Help'
@Metadata.ignorePropagatedAnnotations: true
//@ObjectModel.usageType:{
//    serviceQuality: #X,
//    sizeCategory: #,
//    dataClass: #MIXED
//}
@ObjectModel.resultSet.sizeCategory: #XS // Search Helpin List Box olarak gözükmesini sağlar
define view entity ZEHO_VH_PRIORITY  as select from DDCDS_CUSTOMER_DOMAIN_VALUE_T( p_domain_name:'ZEHO_DM_PRIORITY' )
{
       
   @UI.hidden: true
    key domain_name,
    @UI.hidden: true
    key value_position,
    @UI.hidden: true
    key language,
//    @ObjectModel.text.element: [ 'PriorityText'] 
//    @ObjectModel.sort.enabled: true

//    @UI.lineItem: [{ position: 1 }]
//    @UI.hidden: false
//    @UI.lineItem: [{ position: 10  , label: 'ABC'}]
//    @UI.identification: [{ position: 10 , label: 'ABC'  }]
     @ObjectModel.text.element: ['PriorityText']
 
    key value_low as Priority,
//    @UI.hidden: false
//     @UI.lineItem: [{ position: 20  , label: 'ABC'}]
//        @UI.identification: [{ position: 20 , label: 'ABC' }]
           @ObjectModel.sort.transformedBy: 'ABAP:ZCL_EHO_SORT_PRIORITY'
    text as PriorityText
} where language = $session.system_language
 
 
