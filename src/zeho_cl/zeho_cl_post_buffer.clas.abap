CLASS zeho_cl_post_buffer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES : BEGIN OF ts_message,
              bankcode TYPE zeho_i_activities-bankcode,
              bukrs    TYPE zeho_i_activities-bukrs,
              act_date TYPE zeho_i_activities-act_date,
              act_time TYPE zeho_i_activities-act_time,
              act_no   TYPE zeho_i_activities-act_no,
              iban     TYPE zeho_i_activities-iban,
              branch   TYPE zeho_i_activities-branch,
              symsg    TYPE symsg,
              fields   TYPE string_table,
            END OF ts_message,

            ty_activities      TYPE STANDARD TABLE OF zeho_i_activities,
            tt_activity_in     TYPE TABLE FOR READ IMPORT zeho_i_activities,
            tt_activity_out    TYPE TABLE FOR READ RESULT zeho_i_activities,
            tt_activity_failed TYPE TABLE FOR FAILED zeho_i_activities,
            tt_message         TYPE STANDARD TABLE OF ts_message.
    DATA : tt_exp        TYPE zeho_tt_exp,
           tt_acct       TYPE zeho_tt_actt,
           tt_activities TYPE zeho_tt_activities,
           tt_customer   TYPE zeho_tt_customer,
           tt_supplier   TYPE zeho_tt_supplier,
           lv_where      TYPE string.

    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO zeho_cl_post_buffer.

    METHODS get_data
      IMPORTING it_activity        TYPE tt_activity_in OPTIONAL
      EXPORTING et_acitivity       TYPE tt_activity_out
                et_activity_failed TYPE tt_activity_failed.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA : mo_instance TYPE REF TO zeho_cl_post_buffer.
ENDCLASS.



CLASS ZEHO_CL_POST_BUFFER IMPLEMENTATION.


  METHOD get_data.

    IF it_activity IS SUPPLIED.
      SELECT activity~*
      FROM zeho_i_activities AS activity
      INNER JOIN @it_activity AS lt
                 ON  activity~bankcode    = lt~bankcode
                 AND activity~bukrs       = lt~bukrs
                 AND activity~iban        = lt~iban
                 AND activity~branch      = lt~branch
                 AND activity~acccount_no = lt~acccount_no
                 AND activity~act_date    = lt~act_date
                 AND activity~act_time    = lt~act_time
                 AND activity~act_no      = lt~act_no
           INTO corresponding fields of TABLE @tt_activities        .


      IF tt_activities IS NOT INITIAL.
        et_acitivity = CORRESPONDING #( tt_activities ).
      ENDIF.





    ENDIF.



****    DATA : lt_activities TYPE zeho_tt_activities.
****    DATA tt_activitytype_Range TYPE zeho_tt_activitytype_range.
****    DATA : tt_bankcode_range  TYPE zeho_tt_bankcode_range.
****    DATA : tt_bukrs_range     TYPE zeho_tt_bukrs_range.
****
****    DATA rd_activity TYPE REF TO ZEHO_S_ACTIVITY.
****    DATA tt_acc TYPE  zeho_tt_acc.
****
****    IF it_activity IS SUPPLIED.
****      SELECT "acc~client                    ,
****                acc~bankcode                  ,
****                acc~bukrs                     ,
****                acc~iban                      ,
****                acc~branch                    ,
****                acc~acccount_no               ,
****                acc~act_date                  ,
****                acc~act_time                  ,
****                acc~act_no                    ,
****                acc~description               ,
****                acc~shkzg                     ,
****                acc~amount                    ,
****                acc~instant_amount            ,
****                acc~waers                     ,
****                acc~activity_type             ,
****                acc~activity_explanation      ,
****                acc~sender_iban               ,
****                acc~sender_vkn                ,
****                acc~debited_vkn               ,
****                acc~sender_name               ,
****                acc~sender_bank               ,
****                acc~customer_ref              ,
****                account~hkont               ,
****                log~belnr                     ,
****                log~gjahr                     ,
****                log~blart                     ,
****                log~lifnr                     ,
****                log~kunnr                     ,
****                log~name1                     ,
****                log~secondgl_acc              ,
****                log~kostl                     ,
****                log~prctr                     ,
****                log~gsber                     ,
****                log~umskz                     ,
****                log~mwskz                     ,
****                log~cancel_process            ,
****                log~customization_type      ,
****                log~affacted_priority
****    FROM @it_activity AS lt
****    INNER JOIN zeho_a_aa AS acc
****           ON  acc~bankcode    = lt~bankcode
****           AND acc~bukrs       = lt~bukrs
****           AND acc~iban        = lt~iban
****           AND acc~branch      = lt~branch
****           AND acc~acccount_no = lt~acccount_no
****           AND acc~act_date    = lt~act_date
****           AND acc~act_time    = lt~act_time
****           AND acc~act_no      = lt~act_no
****    LEFT OUTER JOIN zeho_a_log AS log
****           ON  log~bankcode    = acc~bankcode
****           AND log~bukrs       = acc~bukrs
****           AND log~iban        = acc~iban
****           AND log~branch      = acc~branch
****           AND log~acccount_no = acc~acccount_no
****           AND log~act_date    = acc~act_date
****           AND log~act_time    = acc~act_time
****           AND log~act_no      = acc~act_no
****    LEFT OUTER JOIN zeho_a_acc AS account
****          ON account~bankcode = acc~bankcode
****         AND account~bukrs     = acc~bukrs
****         AND account~iban      = acc~iban
****         AND account~branch    = acc~branch
*****                  where (lv_conditions)
****        INTO TABLE @tt_activities."et_acitivity.
****
****
****      IF tt_activities IS NOT INITIAL.
****
****        CLEAR lv_where.
****        lt_activities = tt_activities.
****        SORT lt_activities BY  activity_type.
****        DELETE ADJACENT DUPLICATES FROM lt_activities COMPARING activity_type.
****
****        tt_activitytype_range = VALUE #( FOR act IN  lt_activities
****                       ( sign = 'I' option = 'EQ' low =  act-activity_type )
****                ).
****
****
****        lt_activities = tt_activities.
****        SORT lt_activities BY  bankcode.
****        DELETE ADJACENT DUPLICATES FROM lt_activities COMPARING bankcode.
****
****        tt_bankcode_range = VALUE #( FOR act IN  lt_activities
****                       ( sign = 'I' option = 'EQ' low =  act-bankcode )
****            ).
****
****
****        lt_activities = tt_activities.
****        SORT lt_activities BY  bukrs.
****        DELETE ADJACENT DUPLICATES FROM lt_activities COMPARING bukrs.
****
****        tt_bukrs_range = VALUE #( FOR act IN  lt_activities
****                       ( sign = 'I' option = 'EQ' low =  act-bukrs )
****            ).
****
****
****
****        lv_where = zeho_cl_seltab=>combine_seltabs(
****                          it_named_seltabs = VALUE #(
****                        (    name = 'BANKCODE' dref = REF #(  tt_bankcode_range ) )
****                        (    name = 'BUKRS'    dref = REF #(  tt_bukrs_range    ) )
****                        (    name = 'ACTIVITY_TYPE' dref = REF #(  tt_activitytype_range  ) )
****                         )
****                        ).
****
****        IF tt_exp IS INITIAL AND tt_acct IS INITIAL.
****
****
****          zeho_cl_amdp=>get_custom_for_activity(
****            EXPORTING
****              i_where_clause = lv_where
****              i_mandt        = sy-mandt
****            IMPORTING
****              et_exp         = tt_exp
****              et_actt        = tt_acct
****          ).
****
****        ENDIF.
****
****        IF tt_customer IS INITIAL AND tt_supplier IS INITIAL.
****
****          zeho_cl_amdp=>get_partners(
****            EXPORTING
****              i_where     = lv_where
****            IMPORTING
****              et_customer = tt_customer
****              et_supplier = tt_supplier
****          ).
****        ENDIF..
****
****        SELECT *
****        FROM zeho_a_acc
****         INTO  CORRESPONDING FIELDS OF TABLE @tt_acc.
****        LOOP AT tt_activities REFERENCE INTO rd_activity WHERE belnr = ' '
****                                                           AND cancel_process = ' '.
****
****          zeho_cl_document_processing=>fill_list(
****            EXPORTING
****              tt_exp   = tt_exp
****              tt_acct  = tt_acct
****              tt_cust  = tt_customer
****              tt_suppl = tt_supplier
****              tt_account   = tt_acc
****            CHANGING
****              rd_aa    = rd_activity
****          ).
****
****        ENDLOOP.
****
****      ENDIF.
****
****
****
****
****
****  IF tt_activities IS NOT INITIAL.
****    et_acitivity = CORRESPONDING #( tt_activities ).
****  ENDIF.




*ENDIF.

  ENDMETHOD.


  METHOD get_instance.

    mo_instance = ro_instance = COND #( WHEN mo_instance IS BOUND
                                        THEN mo_instance
                                        ELSE NEW #(  ) ).

  ENDMETHOD.
ENDCLASS.
