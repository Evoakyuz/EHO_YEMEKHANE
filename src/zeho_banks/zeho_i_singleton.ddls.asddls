@AccessControl.authorizationCheck: #NOT_ALLOWED
@EndUserText.label: 'Interface View - Singleton'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZEHO_I_SINGLETON 
as select from zeho_a_bank
composition [0..*] of ZEHO_I_BANK as _Bank
{
    
    key 1 as SingletonID,
    _Bank,
    max( zeho_a_bank.last_changed_at ) as LastChangedAtMax
}
