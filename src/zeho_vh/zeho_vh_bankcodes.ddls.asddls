@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BankCode Value Help'
@Metadata.ignorePropagatedAnnotations: true

@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@Consumption.cacheSettings: {
    useNonTransactional: true,
    usage: #NOT_ALLOWED,
    refreshForbidden: false
}
@ObjectModel.dataCategory: #VALUE_HELP
@ObjectModel.resultSet.sizeCategory: #XXS
//@ObjectModel.query.combinedCountAndDataRetrievalEnabled: false
//@ObjectModel.
define view entity ZEHO_VH_BANKCODES as select distinct from zeho_a_bank
{
   key zeho_a_bank.bankcode as Bankcode
}
