@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View For ZEHO_I_ACTIVITIES'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZEHO_C_ACTIVITIES_MULT
   
  as projection on ZEHO_I_ACTIVITIES_MULT
{
      @UI.hidden: true
  key bankcode,
      @UI.hidden: true
  key bukrs,
      @UI.hidden: true
  key iban,
      @UI.hidden: true
  key branch,
      @UI.hidden: true
  key acccount_no,
      @UI.hidden: true
  key act_date,
      @UI.hidden: true
  key act_time,
      @UI.hidden: true
  key act_no,
      @UI.hidden: true
  key item_no,
      //      description,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_DEBCRED',
      element: 'dc'
      }   }]
      
      shkzg,
      @Semantics.amount.currencyCode: 'waers'
      amount,
      //      @Semantics.amount.currencyCode: 'waers'
      //      instant_amount,
      waers,
      //      activity_type,
      //      belnr,
      //      gjahr,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_BLAR_LNG',
//      name: 'ZEHO_VH_BLART',
      element: 'AccountingDocumentType'
      }   }]
      blart,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_Supplier_VH',
      element: 'Supplier'
      }   }]
      lifnr,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_Customer_VH',
      element: 'Customer'
      }   }]
      kunnr,
      name1,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_OffsettingAccount',
      element: 'OffsettingAccount'
      }   }]
      secondgl_acc,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_CostCenterStdVH',
      element: 'CostCenter'
      }   }]
      kostl,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_ProfitCenterStdVH',
      element: 'ProfitCenter'
      }   }]
      prctr,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_BusinessAreaStdVH',
      element: 'BusinessArea'
      }   }]
      gsber,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_SpecialGLCode',
      element: 'SpecialGLCode'
      }   }]
      umskz,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_TaxCodeStdVH',
      element: 'TaxCode'
      }   }]
      mwskz,
      //      cancel_process,
      //      customization_type,
      //      affacted_priority,
      /* Associations */
      Activities : redirected to parent ZEHO_C_ACTIVITIES
}
