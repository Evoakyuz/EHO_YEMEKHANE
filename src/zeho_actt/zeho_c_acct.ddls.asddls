@AccessControl.authorizationCheck: #NOT_ALLOWED
@EndUserText.label: 'Consumption View For ZEHO_I_ACCT'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

define root view entity ZEHO_C_ACCT
  provider contract transactional_query
  as projection on ZEHO_I_ACCT
{

      @Consumption.valueHelpDefinition: [{ entity: {
       name: 'ZEHO_VH_BANKCODES',
       element: 'Bankcode' 
       
       } ,useForValidation: true  }]
      @Consumption.filter.multipleSelections: false
      @Consumption.filter.selectionType: #SINGLE
      @Consumption.filter:{ mandatory:true }

  key Bankcode,
      @Consumption.valueHelpDefinition: [ { entity:
      {
      name: 'ZEHO_VH_BUKRS',
      element: 'Bukrs'
      },
      distinctValues: true,
      useForValidation: true,
      additionalBinding: [{ usage: #FILTER  ,element: 'Bankcode' , localElement: 'Bankcode'   }] 
      }]
  
  key Bukrs,
  key ActivityType,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_PRIORITY',
      element: 'Priority'
    
      }   , useForValidation: true }]
      @Consumption.filter.hidden: true
  key Priority,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_DEBIT_CREDIT',
      element: 'dc'
      }   }]
      //     @Consumption.filter.defaultValue: 'All'
      //     @Consumption.defaultValue: 'A'
      //    @UI.defaultValue: 'All'
      @Consumption.filter.hidden: true
      Dc,
      @Consumption.valueHelpDefinition: [{ entity: {
        name: 'ZEHO_VH_BLAR_LNG',
//        name: 'ZEHO_VH_BLART',
        element: 'AccountingDocumentType'
        }   }]
      @Consumption.filter.hidden: true
      Blart,
      @Consumption.filter.hidden: true
      VendorControl,
      @Consumption.filter.hidden: true
      CustomerControl,
      @Consumption.filter.hidden: true
      VirementControl,

      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_OffsettingAccount',
      element: 'OffsettingAccount'
      }   }]
      @EndUserText: { label:  'Offsetting Account' }
      @Consumption.filter.hidden: true
      SecondglAcc,
      @Consumption.valueHelpDefinition: [{ entity: {
       name: 'I_CostCenterStdVH',
       element: 'CostCenter'
       }   }]
      @Consumption.filter.hidden: true
      Kostl,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_ProfitCenterStdVH',
      element: 'ProfitCenter'
      }   }]
      @Consumption.filter.hidden: true

      Prctr,

      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_BusinessAreaStdVH',
      element: 'BusinessArea'
      }   }]
      @Consumption.filter.hidden: true
      Gsber,
      @Consumption.filter.hidden: true
      CancelProcess
}
