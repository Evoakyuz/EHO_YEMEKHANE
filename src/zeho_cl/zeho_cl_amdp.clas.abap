CLASS zeho_cl_amdp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS lv_s TYPE string VALUE ''.
    INTERFACES if_amdp_marker_hdb.
    CLASS-METHODS get_acitivities AMDP OPTIONS READ-ONLY CDS SESSION CLIENT DEPENDENT
      IMPORTING VALUE(I_where_clause) TYPE string
                VALUE(I_MANDT)        TYPE ZEHO_DE_CLIENT
      EXPORTING VALUE(et_aa)          TYPE zeho_tt_activities.

    CLASS-METHODS get_custom_for_activity AMDP OPTIONS READ-ONLY CDS SESSION CLIENT DEPENDENT
      IMPORTING VALUE(I_where_clause) TYPE string
          VALUE(I_MANDT)       TYPE ZEHO_DE_CLIENT
      EXPORTING VALUE(et_exp)  TYPE zeho_tt_exp
                VALUE(et_actt) TYPE zeho_tt_actt.

     CLASS-METHODS : get_partners AMDP OPTIONS READ-ONLY CDS SESSION CLIENT DEPENDENT
**     CLIENT current
      IMPORTING   VALUE(I_WHERE)       TYPE string
      EXPORTING  VALUE(et_customer)   TYPE zeho_tt_customer
                 VALUE(et_supplier)   TYPE zeho_tt_supplier.




  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZEHO_CL_AMDP IMPLEMENTATION.


  METHOD get_acitivities  BY DATABASE PROCEDURE FOR HDB LANGUAGE
                        SQLSCRIPT OPTIONS READ-ONLY
                        USING zeho_a_aa
                              zeho_a_log
                              zeho_a_acc.


    et_aa = select
                   acc.bankcode                  ,
                   acc.bukrs                     ,
                   acc.iban                      ,
                   acc.branch                    ,
                   acc.acccount_no               ,
                   acc.act_date                  ,
                   acc.act_time                  ,
                   acc.act_no                    ,
                   acc.description               ,
                   acc.shkzg                     ,
                   acc.amount                    ,
                   acc.instant_amount            ,
                   acc.waers                     ,
                   acc.activity_type             ,
                   acc.activity_explanation      ,
                   acc.sender_iban               ,
                   acc.sender_vkn                ,
                   acc.debited_vkn               ,
                   acc.sender_name               ,
                   acc.sender_bank               ,
                   acc.customer_ref              ,
                   account.hkont                 ,
                   log.belnr                     ,
                   log.gjahr                     ,
                   log.blart                     ,
                   log.lifnr                     ,
                   log.kunnr                     ,
                   log.name1                     ,
                   log.secondgl_acc              ,
                   log.kostl                     ,
                   log.prctr                     ,
                   log.gsber                     ,
                   log.umskz                     ,
                   log.mwskz                     ,
                   log.cancel_process            ,
                   log.customization_type      ,
                   log.affacted_priority
                   from zeho_a_aa  as acc
                   left OUTER join zeho_a_log as log
                                on  log.bankcode    = acc.bankcode
                                and log.bukrs       = acc.bukrs
                                and log.iban        = acc.iban
                                and log.branch      = acc.branch
                                and log.acccount_no = acc.acccount_no
                                and log.act_date    = acc.act_date
                                and log.act_time    = acc.act_time
                                and log.act_no      = acc.act_no
                  left outer join zeho_a_acc as account
                               on account.bankcode = acc.bankcode
                              and account.bukrs     = acc.bukrs
                              and account.iban      = acc.iban
                              and account.branch    = acc.branch
                  where acc.client = :i_mandt;

   if i_where_clause is not null
   THEN
   et_aa = APPLY_FILTER( :et_aa , :i_where_clause );
   end if;



  ENDMETHOD.


  METHOD get_custom_for_activity BY DATABASE PROCEDURE FOR HDB LANGUAGE
                                 SQLSCRIPT OPTIONS READ-ONLY
                                 USING zeho_a_actt
                                       zeho_a_exp.

 et_exp = select   exp.client        ,
                   exp.bankcode      ,
                   exp.bukrs         ,
                   exp.activity_type ,
                   exp.explanation   ,
                   exp.dc            ,
                   exp.blart         ,
                   exp.lifnr         ,
                   exp.kunnr         ,
                   exp.secondgl_acc  ,
                   exp.kostl         ,
                   exp.prctr         ,
                   exp.gsber         ,
                   exp.umskz         ,
                   exp.mwskz         ,
                   exp.cancel_process
                   FROM zeho_a_exp as exp
                   WHERE client = :i_mandt;

   et_actt = select   acct.bankcode        ,
                    acct.bukrs           ,
                    acct.activity_type   ,
                    acct.priority        ,
                    acct.dc              ,
                    acct.blart           ,
                    acct.vendor_control  ,
                    acct.customer_control,
                    acct.virement_control,
                    acct.secondgl_acc    ,
                    acct.kostl           ,
                    acct.prctr           ,
                    acct.gsber           ,
                    acct.cancel_process
                    from zeho_a_actt as acct
                    WHERE client = :i_mandt;



    if i_where_clause is not null
   THEN
   et_exp = APPLY_FILTER( :et_exp , :i_where_clause );
   end if;

    if i_where_clause is not null
   THEN
   et_actt = APPLY_FILTER( :et_actt , :i_where_clause );
   end if;




  ENDMETHOD.


  METHOD get_partners  BY DATABASE PROCEDURE FOR HDB LANGUAGE
                        SQLSCRIPT OPTIONS READ-ONLY
                        using ZEHO_I_CUSTOMER2
                              ZEHO_I_SUPPLIER.

  et_customer = select  Customer as kunnr,
                        CompanyCode as bukrs,
                        TaxNumber1  as stcd1,
                        TaxNumber2  as stcd2,
                        TaxNumber3  as stcd3 ,
                        name1
                from ZEHO_I_CUSTOMER2;


    et_supplier = select  lifnr,
                          bukrs,
                          stcd1,
                          stcd2,
                          stcd3 ,
                          name1
                from ZEHO_I_SUPPLIER;

  ENDMETHOD.
ENDCLASS.
