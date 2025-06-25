@AccessControl.authorizationCheck: #NOT_ALLOWED
@EndUserText.label: 'Interface View - Activity Type'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZEHO_I_ACCT
  as select from zeho_a_actt

{

  key cast(bankcode as bankk preserving type )  as Bankcode,


    key cast(bukrs as bukrs preserving type ) as Bukrs,
  key activity_type                             as ActivityType,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_PRIORITY',
      element: 'Priority'
      }  }]
  key priority                                  as Priority,
      dc                                        as Dc,
      blart                                     as Blart,
      vendor_control                            as VendorControl,
      customer_control                          as CustomerControl,
      virement_control                          as VirementControl,
      secondgl_acc                              as SecondglAcc,
      kostl                                     as Kostl,
      prctr                                     as Prctr,
      gsber                                     as Gsber,
      cancel_process                            as CancelProcess
}
