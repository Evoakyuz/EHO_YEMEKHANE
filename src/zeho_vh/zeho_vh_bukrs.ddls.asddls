@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Company Code Value Help'

@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #A,
    sizeCategory: #S,
    dataClass: #MASTER
}
//@ObjectModel.resultSet.sizeCategory: #XS 
@ObjectModel.resultSet: {
    sizeCategory: #XXS
}
//@ClientHandling.algorithm: #SESSION_VARIABLE
@ObjectModel.dataCategory: #VALUE_HELP
@ObjectModel.representativeKey: 'Bukrs'
@ObjectModel.supportedCapabilities: [#CDS_MODELING_ASSOCIATION_TARGET, 
                                     #CDS_MODELING_DATA_SOURCE,
                                     #SQL_DATA_SOURCE]
@Consumption.ranked: true                                     
define view entity ZEHO_VH_BUKRS  
as select from zeho_a_comp

{   
 
    @UI.hidden: true
    
     key cast(bankcode as bankk preserving type ) as Bankcode,

     key bukrs as Bukrs ,
     comp_desc
}



