@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cds View entity for customer'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define view entity ZEHO_I_CUSTOMER2 as select from I_Customer as cust
    inner join I_CustomerCompany  as comp
            on comp.Customer = cust.Customer 
{
    
    key cust.Customer as Customer,
    comp.CompanyCode  as CompanyCode,
    cust.TaxNumber1   as TaxNumber1, 
    cust.TaxNumber2   as TaxNumber2,
    cust.TaxNumber3   as TaxNumber3,
    cust.CustomerName as name1
}
