@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Supplier CDS Entity'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZEHO_I_SUPPLIER as select from I_Supplier as sup
                                  inner join I_SupplierCompany as comp
                                          on comp.Supplier = sup.Supplier
{
    sup.Supplier as lifnr,
    comp.CompanyCode    as bukrs,
    sup.TaxNumber1      as stcd1,
    sup.TaxNumber2      as stcd2,
    sup.TaxNumber3      as stcd3 ,
    sup.BusinessPartnerName1 as name1
}
