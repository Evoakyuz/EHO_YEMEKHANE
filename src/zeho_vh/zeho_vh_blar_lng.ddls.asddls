@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Language Value Help for blart'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define view entity ZEHO_VH_BLAR_LNG as select from ZEHO_VH_BLART
{  
    @ObjectModel.text.element: ['Description']
   key  AccountingDocumentType,
    
    Description ,
   @UI.hidden: true
//   @UI.hidden: true
   Language 
} where Language = $session.system_language
