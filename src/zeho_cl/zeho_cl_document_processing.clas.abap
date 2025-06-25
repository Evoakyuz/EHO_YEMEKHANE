CLASS zeho_cl_document_processing DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS : c_stcd TYPE char4 VALUE 'STCD'.
    CONSTANTS : c_actt TYPE char4 VALUE 'ACTT'.
    CONSTANTS : c_exp  TYPE char4 VALUE 'EXPL'.
    CONSTANTS : c_all  TYPE zeho_s_exp-dc VALUE 'A'.
    CONSTANTS : c_BusinessTransactionType  TYPE glvor VALUE 'RFBU'.
    CONSTANTS : c_try  TYPE waers VALUE 'TRY'.


    DATA instance TYPE REF TO zeho_badi_activity.

    DATA badi_imp_exists TYPE REF TO zeho_badi_activity.
    DATA imp_class_Tab  TYPE STANDARD TABLE OF REF TO zif_ex_eho_activity_imp .
    DATA imp_class      TYPE REF TO zif_ex_eho_activity_imp.



    TYPES:
      mty_je_post_hierarchy TYPE STRUCTURE FOR ACTION IMPORT i_journalentrytp~post .
    TYPES: lt_entyry     TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
           ls_entry      TYPE LINE OF lt_entyry,
           lty_glitem    TYPE  ls_entry-%param-_glitems,
           lty_currencya TYPE LINE OF  ls_entry-%param-_glitems,
           lty_currency  TYPE  lty_currencya-_currencyamount,
           lty_ARItems   TYPE  ls_entry-%param-_aritems,
           lty_APItems   TYPE  ls_entry-%param-_apitems.

    CLASS-DATA: cl_doc       TYPE REF TO zeho_cl_document_processing,
                e_subrc      TYPE sy-subrc,
                stcd_counter TYPE sy-tabix,
                field_name   TYPE zeho_de_fieldname,
                tt_accounts  TYPE  zeho_tt_acc.
    CLASS-METHODS : fill_list IMPORTING tt_exp     TYPE zeho_tt_exp
                                        tt_acct    TYPE zeho_tt_actt
                                        tt_cust    TYPE zeho_tt_customer
                                        tt_suppl   TYPE zeho_tt_supplier
                                        tt_account TYPE zeho_tt_acc

                              CHANGING  rd_aa      TYPE REF TO zeho_s_activity.

    METHODS : fill_header IMPORTING rd_aa TYPE zeho_i_activities
                          CHANGING  e_je  TYPE  mty_je_post_hierarchy.
    METHODS : fill_gl_account IMPORTING rd_aa      TYPE zeho_i_activities
                                        e_hkont    TYPE hkont
                                        e_dc       TYPE zeho_de_dc
*                                             e_amount TYPE zeho_de_amount
*                                             e_shkzg TYPe zeho_de_dc
                              CHANGING  tt_glitems TYPE   lty_glitem.
    METHODS : fill_secondgl_account IMPORTING rd_aa      TYPE zeho_i_activities
                                              e_hkont    TYPE hkont
                                              e_dc       TYPE zeho_de_dc
*                                             e_amount TYPE zeho_de_amount
*                                             e_shkzg TYPe zeho_de_dc
                                    CHANGING  tt_glitems TYPE   lty_glitem.



    METHODS : fill_customer  IMPORTING rd_aa      TYPE zeho_i_activities
                                       e_dc       TYPE zeho_de_dc
                             CHANGING  tt_aritems TYPE lty_ARItems.

    METHODS : fill_supplier  IMPORTING rd_aa      TYPE zeho_i_activities
                                       e_dc       TYPE zeho_de_dc
                             CHANGING  tt_apitems TYPE lty_APItems.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS : check_explanation IMPORTING tt_exp  TYPE zeho_tt_exp
                                CHANGING  rd_aa   TYPE REF TO zeho_s_activity
                                          e_subrc TYPE sy-subrc.
    METHODS : check_activity_type IMPORTING tt_acct  TYPE zeho_tt_actt
                                            tt_cust  TYPE zeho_tt_customer
                                            tt_suppl TYPE zeho_tt_supplier
                                  CHANGING  rd_aa    TYPE REF TO zeho_s_activity
                                            e_subrc  TYPE sy-subrc.
    METHODS : fill_currency_amount IMPORTING rd_aa        TYPE zeho_i_activities
                                             i_dc         TYPE  zeho_de_dc
                                   CHANGING
*                                    tt_glitems TYPE   lty_glitem
                                             tt_currencya TYPE lty_currency.
    CLASS-DATA : m_buzei TYPE buzei.
*    CLASS-DATA : cl_Ref TYPE REF TO zeho_cl_document_processing.

ENDCLASS.



CLASS ZEHO_CL_DOCUMENT_PROCESSING IMPLEMENTATION.


  METHOD check_activity_type.
    DATA lv_count TYPE sy-tabix.
    DATA lv_priority  TYPE zeho_a_actt-priority.
    DATA lrd_acttype TYPE REF TO zeho_s_acct.
    DATA : lrd_cust TYPE REF TO zeho_s_customer.
    DATA : lrd_suppl TYPE REF TO zeho_s_supplier.
    DATA : lrd_accounts TYPE REF TO zeho_s_acc.
    DATA : matching_ok TYPE char1.
    CLEAR : lv_count , matching_ok.

    LOOP AT tt_acct TRANSPORTING NO FIELDS WHERE  bukrs         = rd_aa->bukrs
                                             AND  bankcode      = rd_aa->bankcode
                                             AND  activity_type = rd_aa->activity_type.
      lv_count = lv_count + 1.
    ENDLOOP.
    lv_priority = 1.
    DO lv_count TIMES.
      READ TABLE tt_acct REFERENCE INTO lrd_acttype WITH KEY bukrs         = rd_aa->bukrs
                                                             bankcode      = rd_aa->bankcode
                                                             activity_type = rd_aa->activity_type
                                                             priority      = lv_priority.

      IF lrd_acttype->dc IS NOT INITIAL
        AND lrd_acttype->dc <> rd_aa->shkzg.
        lv_priority = lv_priority + 1.
        CONTINUE.
      ENDIF.

      IF lrd_acttype->vendor_control = abap_true.
        CLEAR stcd_counter.
        DO 3 TIMES.
          field_name = c_stcd && stcd_counter.

          READ TABLE tt_suppl REFERENCE INTO lrd_suppl WITH KEY (field_name) = rd_aa->sender_vkn
                                                                bukrs        = rd_aa->bukrs.
          IF sy-subrc = 0 AND rd_aa->sender_vkn IS NOT INITIAL.
            rd_aa->lifnr = lrd_suppl->lifnr.
            rd_aa->name1 = lrd_suppl->name1.
            rd_aa->customization_type = me->c_actt.
            rd_aa->affacted_priority = lv_priority.
            matching_ok = abap_true.
            e_subrc     = 0.
            EXIT.
          ENDIF.

          READ TABLE tt_suppl REFERENCE INTO lrd_suppl WITH KEY (field_name) = rd_aa->debited_vkn
                                                                bukrs        = rd_aa->bukrs.
          IF sy-subrc = 0 AND rd_aa->debited_vkn IS NOT INITIAL.
            rd_aa->lifnr = lrd_suppl->lifnr.
            rd_aa->name1 = lrd_suppl->name1.
            rd_aa->customization_type = me->c_actt.
            rd_aa->affacted_priority = lv_priority.
            matching_ok = abap_true.
            e_subrc     = 0.
            EXIT.
          ENDIF.
        ENDDO.

      ELSEIF lrd_acttype->customer_control = abap_true.
        READ TABLE tt_cust REFERENCE INTO lrd_cust WITH KEY (field_name) = rd_aa->sender_vkn
                                                            bukrs        = rd_aa->bukrs.
        IF sy-subrc = 0 AND rd_aa->sender_vkn IS NOT INITIAL.
          rd_aa->kunnr = lrd_cust->kunnr.
          rd_aa->name1 = lrd_cust->name1.
          rd_aa->customization_type = me->c_actt.
          rd_aa->affacted_priority = lv_priority.
          matching_ok = abap_true.
          e_subrc     = 0.
          EXIT.
        ENDIF.

        READ TABLE tt_cust REFERENCE INTO lrd_cust WITH KEY (field_name) = rd_aa->debited_vkn
                                                            bukrs        = rd_aa->bukrs.
        IF sy-subrc = 0 AND rd_aa->debited_vkn IS NOT INITIAL.
          rd_aa->kunnr = lrd_cust->kunnr.
          rd_aa->name1 = lrd_cust->name1.
          rd_aa->customization_type = me->c_actt.
          rd_aa->affacted_priority = lv_priority.
          matching_ok = abap_true.
          e_subrc     = 0.
          EXIT.
        ENDIF.
      ELSEIF lrd_acttype->virement_control = abap_true.
        READ TABLE me->tt_accounts REFERENCE INTO lrd_accounts WITH KEY iban = rd_aa->sender_iban
                                                                        bukrs  = rd_aa->bukrs.
        IF sy-subrc IS INITIAL AND
           rd_aa->sender_iban IS NOT INITIAL
           AND rd_aa->iban NE rd_aa->sender_iban.           "20211021.
          rd_aa->secondgl_acc = lrd_accounts->hkont.
          IF rd_aa->secondgl_acc IS NOT INITIAL.
            rd_aa->customization_type = me->c_actt.
            rd_aa->affacted_priority = lv_priority.
            matching_ok = abap_true.
            e_subrc     = 0.
            EXIT.
          ENDIF.
        ENDIF.

      ELSEIF lrd_acttype->secondgl_acc IS NOT INITIAL .
        matching_ok = abap_true.
        e_subrc     = 0.
        rd_aa->customization_type = me->c_actt.
        rd_aa->affacted_priority = lv_priority.
        EXIT.

      ELSEIF lrd_acttype->cancel_process IS NOT INITIAL.
        matching_ok = abap_true.
        rd_aa->cancel_process = abap_true.
        rd_aa->customization_type = me->c_actt.
        rd_aa->affacted_priority = lv_priority.
        EXIT.
      ENDIF.
      lv_priority = lv_priority + 1.


    ENDDO.




  ENDMETHOD.


  METHOD check_explanation.
    DATA lrd_exp TYPE REF TO zeho_s_exp.
    DATA lv_check TYPE char1.

    LOOP AT tt_exp REFERENCE INTO lrd_exp.
      IF rd_aa->description CP lrd_exp->explanation AND lrd_exp->dc = me->c_all.
        lv_check = abap_true.
      ELSEIF rd_aa->description CP lrd_exp->explanation AND rd_aa->shkzg EQ lrd_exp->dc .
        lv_check = abap_true.
      ENDIF.
      IF  lv_check = abap_true.
        rd_aa->blart          =    lrd_exp->blart          .
        rd_aa->lifnr          =    lrd_exp->lifnr          .
        rd_aa->kunnr          =    lrd_exp->kunnr          .
        rd_aa->secondgl_acc   =    lrd_exp->secondgl_acc   .
        rd_aa->kostl          =    lrd_exp->kostl          .
        rd_aa->prctr          =    lrd_exp->prctr          .
        rd_aa->gsber          =    lrd_exp->gsber          .
        rd_aa->umskz          =    lrd_exp->umskz          .
        rd_aa->mwskz          =    lrd_exp->mwskz          .
        rd_aa->cancel_process =    lrd_exp->cancel_process .
        rd_aa->customization_type = me->c_exp.
        e_subrc = 0.
        EXIT.
      ENDIF.

    ENDLOOP.
    IF lv_check = abap_false.
      e_subrc = 1.
    ENDIF.


  ENDMETHOD.


  METHOD fill_currency_amount.
    DATA lv_currtype TYPE char2.
    DATA lv_amount TYPE zeho_de_amount.
    DATA lv_local_curr_amount TYPE zeho_de_amount.
    IF i_dc EQ 'S'."
      lv_amount =  rd_aa-amount.
    ELSE.
      lv_amount = -1 * rd_aa-amount.
    ENDIF.

    IF i_dc EQ 'S'."
      lv_local_curr_amount =  rd_aa-local_amount .
    ELSE.
      lv_local_curr_amount = rd_aa-local_amount * -1.
    ENDIF.



    tt_currencya = VALUE #(
       (
          journalentryitemamount = lv_amount
          Currency               = rd_aa-waers
          currencyrole     = '00'

       )
     ).

    IF rd_aa-waers <> c_try.
      tt_currencya = VALUE #( BASE  tt_currencya
     (
        journalentryitemamount = lv_local_curr_amount
        Currency         = c_try
        currencyrole     = '10'
        ExchangeRate     = rd_aa-local_amount / rd_aa-amount

     )
   ).

    ENDIF.


  ENDMETHOD.


  METHOD fill_customer.
    DATA ls_aritem TYPE LINE OF   lty_aritems.
    DATA lt_currencyamount TYPE lty_currency.
    DATA lt_aritem TYPE lty_aritems.

    m_buzei = m_buzei + 1.

    ls_aritem-GLAccountLineItem = m_buzei.
    ls_aritem-DocumentItemText  = rd_aa-description.
    ls_aritem-Customer          = rd_aa-kunnr .
    ls_aritem-BusinessPlace     = rd_aa-gsber .
    ls_aritem-SpecialGLCode     = rd_aa-umskz .

    fill_currency_amount(
   EXPORTING
     rd_aa        = rd_aa
      i_Dc         =  e_dc "COND #( WHEN  rd_aa-shkzg = 'H'
                   "   THEN 'S' ELSE 'H'  )
   CHANGING
*        tt_glitems   = lt_glitems
     tt_currencya = lt_currencyamount
 ).

    CLEAR lt_aritem.
    lt_aritem = VALUE #(
   (
    GLAccountLineItem = ls_aritem-GLAccountLineItem
    DocumentItemText  = ls_aritem-DocumentItemText
    Customer          = ls_aritem-Customer
    BusinessPlace     = ls_aritem-BusinessPlace
    SpecialGLCode     = ls_aritem-SpecialGLCode
    _currencyamount   = CORRESPONDING #( lt_currencyamount )

   )
   ).
    IF imp_class IS BOUND.
      imp_class->customer_processing(
        EXPORTING
          rd_aa      = rd_aa
          e_dc       = e_dc
        CHANGING
          tt_aritems = lt_aritem
      ).
    ENDIF.

    APPEND LINES OF lt_aritem TO tt_aritems.

*    tt_aritems = VALUE #( BASE tt_aritems
*     (
*      GLAccountLineItem = ls_aritem-GLAccountLineItem
*      DocumentItemText  = ls_aritem-DocumentItemText
*      Customer          = ls_aritem-Customer
*      BusinessPlace     = ls_aritem-BusinessPlace
*      SpecialGLCode     = ls_aritem-SpecialGLCode
*      _currencyamount   = CORRESPONDING #( lt_currencyamount )
*
*     )
*     ).

  ENDMETHOD.


  METHOD fill_gl_account.
    DATA ls_glitems TYPE LINE OF   lty_glitem.
    DATA lt_currencyamount TYPE lty_currency.
    DATA lt_glitems TYPE lty_glitem.


    m_buzei = m_buzei + 1.
    CLEAR ls_glitems.
    CLEAR lt_currencyamount.
    ls_glitems-glaccountlineitem       = m_buzei            .
    ls_glitems-glaccount               = e_hkont            .
    ls_glitems-DocumentItemText        = rd_aa-description  .
    ls_glitems-BusinessPlace           = rd_aa-gsber        .
    ls_glitems-ValueDate               = rd_aa-act_date     .


    fill_currency_amount(
       EXPORTING
         rd_aa        = rd_aa
         i_dc         = rd_aa-shkzg



       CHANGING
*        tt_glitems   = lt_glitems
         tt_currencya = lt_currencyamount
     ).

    CLEAR lt_glitems.
    lt_glitems  = VALUE #(
   (
        glaccountlineitem = ls_glitems-glaccountlineitem
        glaccount         = ls_glitems-glaccount
        DocumentItemText  = ls_glitems-DocumentItemText
        BusinessPlace     = ls_glitems-BusinessPlace
        ValueDate         = ls_glitems-ValueDate
       _currencyamount = CORRESPONDING #( lt_currencyamount )
    )
   ).


    IF imp_class IS BOUND.
      imp_class->firstgl_processing(
        EXPORTING
          rd_aa      = rd_aa
          e_hkont    = e_hkont
          e_dc       = e_dc
        CHANGING
          tt_glitems = lt_glitems
      ).
    ENDIF.

    APPEND LINES OF lt_glitems TO tt_glitems.


*    tt_glitems = VALUE #(  BASE tt_glitems
*    (
*         glaccountlineitem = ls_glitems-glaccountlineitem
*         glaccount         = ls_glitems-glaccount
*         DocumentItemText  = ls_glitems-DocumentItemText
*         BusinessPlace     = ls_glitems-BusinessPlace
*         ValueDate         = ls_glitems-ValueDate
*        _currencyamount = CORRESPONDING #( lt_currencyamount )
*     )
*    ).



  ENDMETHOD.


  METHOD fill_header.

    me->instance =  zeho_badi_activity=>get_instance( ).
    imp_class_Tab = zeho_badi_activity=>get_instance(  )->imps.
    imp_class = VALUE #( imp_class_Tab[ 1 ]  OPTIONAL ).

    CLEAR m_buzei.
    e_je-%param = VALUE #(
     companycode = rd_aa-bukrs
*    documentreferenceid = 'BKPFF'
     createdbyuser = sy-uname
     businesstransactiontype = c_BusinessTransactionType
     accountingdocumenttype = rd_aa-blart
     documentdate = rd_aa-act_date
     postingdate = rd_aa-act_date
     accountingdocumentheadertext = rd_aa-description
     ).
  ENDMETHOD.


  METHOD fill_list.

*   me->check_explanation
    CREATE OBJECT cl_doc.
    CLEAR e_subrc.
    cl_doc->check_explanation(
      EXPORTING
        tt_exp = tt_exp
      CHANGING
        rd_aa  = rd_aa
        e_subrc = e_subrc
    ).

    CHECK e_subrc <> 0.

    cl_doc->check_activity_type(
      EXPORTING
        tt_acct  = tt_acct
        tt_cust  = tt_cust
        tt_suppl = tt_suppl
      CHANGING
        rd_aa    = rd_aa
        e_subrc  = e_subrc
    ).


  ENDMETHOD.


  METHOD fill_secondgl_account.
    DATA ls_glitems TYPE LINE OF   lty_glitem.
    DATA lt_currencyamount TYPE lty_currency.
    DATA lt_glitems TYPE lty_glitem.


    m_buzei = m_buzei + 1.
    CLEAR ls_glitems.
    CLEAR lt_currencyamount.
    ls_glitems-glaccountlineitem       = m_buzei            .
    ls_glitems-glaccount               = e_hkont            .
    ls_glitems-DocumentItemText        = rd_aa-description  .
    ls_glitems-BusinessPlace           = rd_aa-gsber        .
    ls_glitems-ValueDate               = rd_aa-act_date     .


    fill_currency_amount(
       EXPORTING
         rd_aa        = rd_aa
         i_dc         = e_dc "COND #( WHEN  rd_aa-shkzg = 'H'
                     "  THEN 'S' ELSE 'H'  )

       CHANGING
*        tt_glitems   = lt_glitems
         tt_currencya = lt_currencyamount
     ).


    CLEAR lt_glitems.

    lt_glitems = VALUE #(
    (
         glaccountlineitem = ls_glitems-glaccountlineitem
         glaccount         = ls_glitems-glaccount
         DocumentItemText  = ls_glitems-DocumentItemText
         BusinessPlace     = ls_glitems-BusinessPlace
         ValueDate         = ls_glitems-ValueDate
        _currencyamount = CORRESPONDING #( lt_currencyamount )
     )
    ).
    IF imp_class IS BOUND.
      imp_class->secondgl_processing(
        EXPORTING
          rd_aa      =  rd_aa
          e_hkont    =  e_hkont
          e_dc       =  e_dc
        CHANGING
          tt_glitems =  lt_glitems
      ).
    ENDIF.

    APPEND LINES OF lt_glitems TO tt_glitems.


*    tt_glitems = VALUE #(  BASE tt_glitems
*    (
*         glaccountlineitem = ls_glitems-glaccountlineitem
*         glaccount         = ls_glitems-glaccount
*         DocumentItemText  = ls_glitems-DocumentItemText
*         BusinessPlace     = ls_glitems-BusinessPlace
*         ValueDate         = ls_glitems-ValueDate
*        _currencyamount = CORRESPONDING #( lt_currencyamount )
*     )
*    ).



  ENDMETHOD.


  METHOD fill_supplier.
    DATA ls_apitem TYPE LINE OF   lty_apitems.
    DATA lt_currencyamount TYPE lty_currency.
    DATA lt_apitem TYPE lty_apitems.

    m_buzei = m_buzei + 1.
    ls_apitem-GLAccountLineItem = m_buzei.
    ls_apitem-DocumentItemText  = rd_aa-description.
    ls_apitem-Supplier          = rd_aa-lifnr .
    ls_apitem-BusinessPlace     = rd_aa-gsber .
    ls_apitem-SpecialGLCode     = rd_aa-umskz .
    ls_apitem-ProfitCenter      = rd_aa-prctr .

    fill_currency_amount(
        EXPORTING
         rd_aa        = rd_aa
          i_Dc         =  e_dc"COND #( WHEN  rd_aa-shkzg = 'H'
                          "THEN 'S' ELSE 'H'  )
        CHANGING
         tt_currencya = lt_currencyamount
        ).

    CLEAR lt_apitem.
    lt_apitem = VALUE #(
        (
        GLAccountLineItem =  ls_apitem-GLAccountLineItem
        DocumentItemText  =  ls_apitem-DocumentItemText
        Supplier          =  ls_apitem-Supplier
        BusinessPlace     =  ls_apitem-BusinessPlace
        SpecialGLCode     =  ls_apitem-SpecialGLCode
        ProfitCenter      =  ls_apitem-ProfitCenter
        _currencyamount   =  CORRESPONDING #( lt_currencyamount )
        )

        ).

    IF imp_class IS BOUND.
      imp_class->supplier_processing(
        EXPORTING
          rd_aa      = rd_aa
          e_dc       = e_dc
        CHANGING
          tt_apitems = lt_apitem
      ).
    ENDIF.

    APPEND LINES OF lt_apitem TO tt_apitems.



*    tt_apitems = VALUE #( BASE tt_apitems
* (
*  GLAccountLineItem =  ls_apitem-GLAccountLineItem
*  DocumentItemText  =  ls_apitem-DocumentItemText
*  Supplier          =  ls_apitem-Supplier
*  BusinessPlace     =  ls_apitem-BusinessPlace
*  SpecialGLCode     =  ls_apitem-SpecialGLCode
*  ProfitCenter      =  ls_apitem-ProfitCenter
*  _currencyamount   =  CORRESPONDING #( lt_currencyamount )
*    )
*
*
* ).


  ENDMETHOD.
ENDCLASS.
