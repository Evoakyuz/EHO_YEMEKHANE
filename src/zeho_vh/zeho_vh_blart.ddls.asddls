@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Document Type Value Help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZEHO_VH_BLART as select from I_AccountingDocumentType
{  
    @ObjectModel.text.element: ['Description']
   key  AccountingDocumentType,
    
   _Text.AccountingDocumentTypeName as Description ,
   @UI.hidden: true
//   @UI.hidden: true
   _Text.Language as Language 
} 
