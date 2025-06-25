@EndUserText.label: 'Custom Entity For Activities'
@ObjectModel.query.implementedBy: 'ABAP:ZEHO_CL_RAP_ACTIVITIES_QUERY'
@Metadata.allowExtensions: true

define root custom entity ZEHO_I_ACTIVITIES_CUSTOM
  // with parameters parameter_name : parameter_type
{

//  key client               : abap.clnt;

  key bankcode             : bankk;

  key bukrs                : bukrs;

  key iban                 : iban;

  key branch               : zeho_de_branch;

  key acccount_no          : zeho_de_acc_no;


  key act_date             : zeho_de_activity_date;

  key act_time             : zeho_de_act_time;

  key act_no               : zeho_de_act_key;

      description          : zeho_de_description;

      shkzg                : shkzg;

      @Semantics.amount.currencyCode : 'WAERS'
      amount               : zeho_de_amount;

      @Semantics.amount.currencyCode : 'WAERS'
      
      instant_amount       : zeho_de_instant_amount;
//      @UI.hidden: true
      @Semantics.currencyCode: true
      waers              : waers; 

      activity_type        : zeho_de_activity_types;

      activity_explanation : zeho_de_activity_explanation;

      sender_iban          : iban;

      sender_vkn           : stcd2;

      debited_vkn          : stcd2;

      sender_name          : zeho_de_sender_name;

      sender_bank          : zeho_de_sender_bank;

      customer_ref         : zeho_de_customer_ref;
      
       hkont                 : hkont;

      belnr                : belnr_d;

      gjahr                : gjahr;

      blart                : blart;

      lifnr                : lifnr;

      kunnr                : kunnr;
      name1                : name1_gp;

      secondgl_acc         : saknr;

      kostl                : kostl;

      prctr                : prctr;

      gsber                : gsber;

      umskz                : zeho_de_spec_gl_indic;

      mwskz                : mwskz;

      cancel_process       : zeho_de_cancell_process;
      customization_type   : zeho_de_customization_type;
      affacted_priority    : zeho_de_priority;

}
