@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View For ZEHO_I_EXP'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZEHO_C_EXP
  provider contract transactional_query
  as projection on ZEHO_I_EXP
{
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_BANKCODES',
      element: 'Bankcode'
      }  }]
      @Consumption.filter.multipleSelections: false
      @Consumption.filter.selectionType: #SINGLE
      @Consumption.filter:{ mandatory:true }
  key Bankcode,
      @Consumption.valueHelpDefinition: [{ entity:
      {
      name: 'ZEHO_VH_BUKRS',
      element: 'Bukrs'
      },
      distinctValues: true,
      additionalBinding: [{ usage: #FILTER  ,element: 'Bankcode' , localElement: 'Bankcode'   }]
      }]
  key Bukrs,
  key ActivityType,
      @Consumption.filter.hidden: true
  key Explanation,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_DEBIT_CREDIT',
      element: 'dc'
      }   }]
      @Consumption.filter.hidden: true
  key Dc,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_BLAR_LNG',
//      name: 'ZEHO_VH_BLART',
      element: 'AccountingDocumentType'
      }   }]
      @Consumption.filter.hidden: true
      Blart,

      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_Supplier_VH',
      element: 'Supplier'
      }   }]
      @Consumption.filter.hidden: true
      Lifnr,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_Customer_VH',
      element: 'Customer'
      }   }]
      @Consumption.filter.hidden: true
      Kunnr,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_OffsettingAccount',
      element: 'OffsettingAccount'
      }   }]
      @Consumption.filter.hidden: true
      @EndUserText: { label:  'Offsetting Account' }
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

      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_SpecialGLCode',
      element: 'SpecialGLCode'
      }   }]
      @Consumption.filter.hidden: true
      Umskz,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_TaxCodeStdVH',
      element: 'TaxCode'
      }   }]
      @Consumption.filter.hidden: true
      Mwskz,
      @Consumption.filter.hidden: true
      CancelProcess
}
