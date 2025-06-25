@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View For ZEHO_I_ACTIVITIES'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZEHO_C_ACTIVITIES
  provider contract transactional_query
  as projection on ZEHO_I_ACTIVITIES
{
  key     bankcode,
  key     bukrs,
  key     iban,
  key     branch,
  key     acccount_no,
  key     act_date,
  key     act_time,
  key     act_no,
          //      act_date,
          //      act_time,
          description,
          shkzg,
          @Semantics.amount.currencyCode : 'waers'
          amount,
          @Semantics.amount.currencyCode : 'waers'
          instant_amount,
          waers,
          activity_type,
          activity_explanation,
          sender_iban,
          sender_vkn,
          debited_vkn,
          sender_name,
          sender_bank,
          customer_ref,
          hkont,
          belnr,
          gjahr,
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
          cancel_process,
          customization_type,
          affacted_priority,
          @Semantics.amount.currencyCode : 'waers'
          local_amount,
          //      @Consumption.filter.hidden: true
               @Consumption.valueHelpDefinition: [{ entity: {
          name: 'ZEHO_VH_STATU',
          element: 'Status'
          }   }]
          @UI.selectionField: [ { position: 9  }  ]
          @Consumption.filter.multipleSelections: false
          @UI.identification: [{ hidden: true  }]
          @UI.lineItem: [{ hidden: true   }]
          //       @UI.hidden: true
          Status,
          StatusText,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZEHO_CL_DOCDISP'
          @UI.lineItem: [{ hidden }]
          @UI.identification: [{ hidden: true }]
  virtual DocumentDisplayUrl : abap.string(0),

          //      StatusText ,
          _Mult : redirected to composition child ZEHO_C_ACTIVITIES_MULT
}
