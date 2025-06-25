@EndUserText.label: 'Maintain Bank Codes'
@AccessControl.authorizationCheck: #NOT_ALLOWED
@UI: {
  headerInfo: {
    typeName: 'SingletonID',
      typeNamePlural: 'SingletonID', 
    title: {
      type: #STANDARD, 
      label: 'Main Screen', 
      value: 'SingletonID'
    }
  }
}
@ObjectModel.semanticKey: [ 'SingletonID' ]
define root view entity ZEHO_C_SINGLETON 
  provider contract transactional_query
  as projection on ZEHO_I_SINGLETON
{   

    @UI.facet: [ {
    id: 'ZEHO_I_BANK', 
    purpose: #STANDARD, 
    type: #LINEITEM_REFERENCE, 
    label: 'Bank Codes ', 
    position: 1 , 
    targetElement: '_Bank'
  } ]
    @UI.lineItem: [ {
    position: 1 ,
    label: 'Bank Maintenance',
    cssDefault.width: '20rem'
  } ]
    key SingletonID,
    @UI.hidden: true
    LastChangedAtMax,
    /* Associations */
    _Bank : redirected to composition child ZEHO_C_BANK
}
