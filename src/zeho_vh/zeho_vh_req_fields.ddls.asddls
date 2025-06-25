@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BankCode Value Help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.resultSet.sizeCategory: #XS 
@Metadata.allowExtensions: true

@Search.searchable: true
define view  entity  ZEHO_VH_REQ_FIELDS 
 as select  distinct from  zeho_a_tab_fld
          
{   
    
//    key bankcode,
//    @ObjectModel.virtualElementCalculatedBy:    'ABAP:ZCL_EHO_AMDP'
    @UI.hidden: true
    key tabname,
    @Search.defaultSearchElement: true
    key fieldname
    
    
//    @ObjectModel.virtualElementCalculatedBy:    'ABAP:ZCL_EHO_AMDP'
//    cast('' as abap.char(30) ) as fieldname
    
  
}
