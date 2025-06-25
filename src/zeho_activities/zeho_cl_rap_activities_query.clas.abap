CLASS zeho_cl_rap_activities_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_response_tab TYPE STANDARD TABLE OF zeho_i_activities_custom WITH EMPTY KEY.
    DATA : lv_where TYPE string.
    DATA : tt_exp        TYPE zeho_tt_exp,
           tt_acct       TYPE zeho_tt_actt,
           tt_activities TYPE zeho_tt_activities,
           tt_customer   TYPE zeho_tt_customer,
           tt_supplier   TYPE zeho_tt_supplier.
ENDCLASS.



CLASS ZEHO_CL_RAP_ACTIVITIES_QUERY IMPLEMENTATION.


  METHOD if_rap_query_provider~select.

    DATA : lt_activities TYPE zeho_tt_activities.
    DATA response_Tab TYPE ty_response_tab.
    DATA tt_activitytype_Range TYPE zeho_tt_activitytype_range.
    DATA rd_activity TYPE REF TO zeho_s_activity.
    DATA tt_acc TYPE  zeho_tt_acc.
    CLEAR :  tt_activities .", tt_acct  , tt_exp , tt_customer , tt_supplier , tt_acc.
    IF  io_request->is_data_requested( ).

      TRY.
          DATA(filters) = io_request->get_filter( )->get_as_ranges( ).
          DATA(top)     = io_request->get_paging( )->get_page_size( ).
          DATA(skip)    = io_request->get_paging( )->get_offset( ).
*        DATA(requested_fields)  = io_request->get_requested_elements( ).
          DATA(sort_order)    = io_request->get_sort_elements( ).
        CATCH cx_rap_query_filter_no_range INTO DATA(ex_ranges).
*        RAISE EXCEPTION NEW zcx_rap_query_prov_not_im( previous = ex_ranges ).
      ENDTRY.
      DATA(selopt_bankcode) = VALUE #( filters[ name = 'BANKCODE' ]-range OPTIONAL ).
      DATA(selopt_bukrs) = VALUE #( filters[ name = 'BUKRS' ]-range OPTIONAL ).
      DATA(selopt_actdate) = VALUE #( filters[ name = 'ACT_DATE' ]-range OPTIONAL ).
***      DATA(selopt_iban) = VALUE #( filters[ name = 'IBAN' ]-range OPTIONAL ).
***      DATA(selopt_branch) = VALUE #( filters[ name = 'BRANCH' ]-range OPTIONAL ).
***      DATA(selopt_acccount_no) = VALUE #( filters[ name = 'ACCOUNT_NO' ]-range OPTIONAL ).
***      DATA(selopt_actno) = VALUE #( filters[ name = 'ACT_NO' ]-range OPTIONAL ).

*      DATA(selopt_act_date) = VALUE #( filters[ name = 'ACT_DATE' ]-range OPTIONAL ).



      TRY.
***          DATA(lv_where) = zeho_cl_seltab=>combine_seltabs(
***                             it_named_seltabs = VALUE #(
***                           (    name = 'BANKCODE' dref = REF #(  selopt_bankcode ) )
***                           (    name = 'BUKRS'    dref = REF #(  selopt_bukrs    ) )
***                           (    name = 'ACT_DATE' dref = REF #(  selopt_actdate  ) )
***                           (    name = 'IBAN' dref = REF #(  selopt_iban  ) )
***                           (    name = 'BRANCH' dref = REF #(  selopt_branch  ) )
***                           (    name = 'ACT_NO' dref = REF #(  selopt_actno  ) )
***                           (    name = 'ACT_DATE' dref = REF #(  selopt_acccount_no  ) )
***                            )
***                           ).

         DATA(lv_conditions) =  io_request->get_filter(  )->get_as_sql_string( ).

*          zeho_cl_amdp=>get_acitivities(
*            EXPORTING
*              i_where_clause = lv_conditions
*              i_mandt        = sy-mandt
*            IMPORTING
*              et_aa          = tt_activities
*          ).

            select "acc~client                    ,
                   acc~bankcode                  ,
                   acc~bukrs                     ,
                   acc~iban                      ,
                   acc~branch                    ,
                   acc~acccount_no               ,
                   acc~act_date                  ,
                   acc~act_time                  ,
                   acc~act_no                    ,
                   acc~description               ,
                   acc~shkzg                     ,
                   acc~amount                    ,
                   acc~instant_amount            ,
                   acc~waers                     ,
                   acc~activity_type             ,
                   acc~activity_explanation      ,
                   acc~sender_iban               ,
                   acc~sender_vkn                ,
                   acc~debited_vkn               ,
                   acc~sender_name               ,
                   acc~sender_bank               ,
                   acc~customer_ref
*                   account~hkont               ,
*                   log~belnr                     ,
*                   log~gjahr                     ,
*                   log~blart                     ,
*                   log~lifnr                     ,
*                   log~kunnr                     ,
*                   log~name1                     ,
*                   log~secondgl_acc              ,
*                   log~kostl                     ,
*                   log~prctr                     ,
*                   log~gsber                     ,
*                   log~umskz                     ,
*                   log~mwskz                     ,
*                   log~cancel_process            ,
*                   log~customization_type      ,
*                   log~affacted_priority
                   from zeho_a_aa  as acc
*                   left OUTER join zeho_a_log as log
*                                on  log~bankcode    = acc~bankcode
*                                and log~bukrs       = acc~bukrs
*                                and log~iban        = acc~iban
*                                and log~branch      = acc~branch
*                                and log~acccount_no = acc~acccount_no
*                                and log~act_date    = acc~act_date
*                                and log~act_time    = acc~act_time
*                                and log~act_no      = acc~act_no
*                  left outer join zeho_a_acc as account
*                               on account~bankcode = acc~bankcode
*                              and account~bukrs     = acc~bukrs
*                              and account~iban      = acc~iban
*                              and account~branch    = acc~branch
                  where (lv_conditions)
                  INTO TABLE @DATA(lt_activities_temp).


                     select "acc~client                    ,
                   acc~bankcode                  ,
                   acc~bukrs                     ,
                   acc~iban                      ,
                   acc~branch                    ,
                   acc~acccount_no               ,
                   acc~act_date                  ,
                   acc~act_time                  ,
                   acc~act_no                    ,
                   acc~description               ,
                   acc~shkzg                     ,
                   acc~amount                    ,
                   acc~instant_amount            ,
                   acc~waers                     ,
                   acc~activity_type             ,
                   acc~activity_explanation      ,
                   acc~sender_iban               ,
                   acc~sender_vkn                ,
                   acc~debited_vkn               ,
                   acc~sender_name               ,
                   acc~sender_bank               ,
                   acc~customer_ref              ,
                   account~hkont               ,
                   log~belnr                     ,
                   log~gjahr                     ,
                   log~blart                     ,
                   log~lifnr                     ,
                   log~kunnr                     ,
                   log~name1                     ,
                   log~secondgl_acc              ,
                   log~kostl                     ,
                   log~prctr                     ,
                   log~gsber                     ,
                   log~umskz                     ,
                   log~mwskz                     ,
                   log~cancel_process            ,
                   log~customization_type      ,
                   log~affacted_priority
                   from @lt_activities_temp  as acc
                   left OUTER join zeho_a_log as log
                                on  log~bankcode    = acc~bankcode
                                and log~bukrs       = acc~bukrs
                                and log~iban        = acc~iban
                                and log~branch      = acc~branch
                                and log~acccount_no = acc~acccount_no
                                and log~act_date    = acc~act_date
                                and log~act_time    = acc~act_time
                                and log~act_no      = acc~act_no
                  left outer join zeho_a_acc as account
                               on account~bankcode = acc~bankcode
                              and account~bukrs     = acc~bukrs
                              and account~iban      = acc~iban
                              and account~branch    = acc~branch
*                  where (lv_conditions)
                  INTO TABLE @tt_activities.




          IF tt_activities IS NOT INITIAL.

            CLEAR lv_where.
            lt_activities = tt_activities.
            SORT lt_activities BY  activity_type.
            DELETE ADJACENT DUPLICATES FROM lt_activities COMPARING activity_type.

            tt_activitytype_range = VALUE #( FOR act IN  lt_activities
                           ( sign = 'I' option = 'EQ' low =  act-activity_type )
                    ).


            lv_where = zeho_cl_seltab=>combine_seltabs(
                              it_named_seltabs = VALUE #(
                            (    name = 'BANKCODE' dref = REF #(  selopt_bankcode ) )
                            (    name = 'BUKRS'    dref = REF #(  selopt_bukrs    ) )
                            (    name = 'ACTIVITY_TYPE' dref = REF #(  tt_activitytype_range  ) )
                             )
                            ).

            if tt_exp is initial and tt_acct is initial.


            zeho_cl_amdp=>get_custom_for_activity(
              EXPORTING
                i_where_clause = lv_where
                i_mandt        = sy-mandt
              IMPORTING
                et_exp         = tt_exp
                et_actt        = tt_acct
            ).

            endif.

            if tt_customer is initial and tt_supplier is initial.

            zeho_cl_amdp=>get_partners(
              EXPORTING
                i_where     = lv_where
              IMPORTING
                et_customer = tt_customer
                et_supplier = tt_supplier
            ).
            ENDIF..

            SELECT *
            FROM zeho_a_acc
*        WHERE client = @sy-mandt
             INTO  CORRESPONDING FIELDS OF TABLE @tt_acc.
            LOOP AT tt_activities REFERENCE INTO rd_activity WHERE belnr = ' '
                                                               AND cancel_process = ' '.

              zeho_cl_document_processing=>fill_list(
                EXPORTING
                  tt_exp   = tt_exp
                  tt_acct  = tt_acct
                  tt_cust  = tt_customer
                  tt_suppl = tt_supplier
                  tt_account   = tt_acc
                CHANGING
                  rd_aa    = rd_activity
              ).

            ENDLOOP.

          ENDIF.

        CATCH zeho_cl_messages.
          "handle exception
      ENDTRY.



      IF tt_activities IS NOT INITIAL.


        response_tab = CORRESPONDING #( tt_activities[] ).
*        io_response->set_total_number_of_records( iv_total_number_of_records = lines( tt_activities )  ).
        io_response->set_data( it_data = response_tab ).
      ENDIF.


    ENDIF.


        IF io_request->is_total_numb_of_rec_requested( ) and tt_activities is initial.
**          lv_where = zeho_cl_seltab=>combine_seltabs(
**                             it_named_seltabs = VALUE #(
**                           (    name = 'BANKCODE' dref = REF #(  selopt_bankcode ) )
**                           (    name = 'BUKRS'    dref = REF #(  selopt_bukrs    ) )
**                           (    name = 'ACT_DATE' dref = REF #(  selopt_actdate  ) )
**                            )
**                           ).
**
**          zeho_cl_amdp=>get_acitivities(
**            EXPORTING
**              i_where_clause = lv_where
**              i_mandt        = sy-mandt
**            IMPORTING
**              et_aa          = tt_activities
**          ).
**           io_response->set_total_number_of_records( iv_total_number_of_records = lines( tt_activities )  ).
         lv_conditions =  io_request->get_filter( )->get_as_sql_string( ).
         DATA lv_count TYPe int8.
            select count( * )
                   from zeho_a_aa
                  where (lv_conditions)
               into @lv_count.
         io_response->set_total_number_of_records( iv_total_number_of_records = lv_count ).
*                  INTO TABLE @tt_activities.


    else.
        io_response->set_total_number_of_records( iv_total_number_of_records = lines( tt_activities )  ).
    ENDIF.



  ENDMETHOD.
ENDCLASS.
