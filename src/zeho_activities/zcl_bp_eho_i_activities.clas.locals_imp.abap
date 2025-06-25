

CLASS lcl_buffer DEFINITION CREATE PRIVATE .

  PUBLIC SECTION.
    INTERFACES zeho_activity_if.

    DATA : tt_act_key TYPE zeho_activity_if~tty_act_with_key.




    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO lcl_buffer.

    METHODS get_data
      IMPORTING it_activity        TYPE zeho_activity_if~tt_activity_in OPTIONAL
      EXPORTING et_acitivity       TYPE zeho_activity_if~tt_activity_out
                et_activity_failed TYPE zeho_activity_if~tt_activity_failed.
    METHODS set_post   IMPORTING tt_act TYPE zeho_tt_activities.
    METHODS get_post   EXPORTING tt_act TYPE zeho_tt_activities.
    METHODS set_keys   IMPORTING tt_keys TYPE zeho_activity_if~tty_act_with_key.
    METHODS get_keys   EXPORTING tt_keys TYPE zeho_activity_if~tty_act_with_key.
    METHODS set_reverse  IMPORTING tt_act TYPE zeho_activity_if~tty_act_with_key.
    METHODS get_reverse  EXPORTING tt_act TYPE zeho_activity_if~tty_act_with_key.
    METHODS post_doc IMPORTING tt_act      TYPE zeho_tt_activities
                               tt_bapidata TYPE zeho_activity_if~tty_bapi_data
                     EXPORTING tt_keys     TYPE zeho_activity_if~tty_act_with_key
                               tt_failed   TYPE zeho_activity_if~ty_failed
                               tt_reported TYPE zeho_activity_if~ty_reported.
    METHODS set_reread IMPORTING tt_act TYPE zeho_tt_activities.
    METHODS set_bapi_data IMPORTING tt_bapi_data TYPE zeho_activity_if~tty_bapi_data.
    METHODS get_bapi_data EXPORTING tt_bapi_data TYPE zeho_activity_if~tty_bapi_data.
    METHODS set_mult_reported IMPORTING tt_reported TYPE zeho_activity_if~tty_mult_reported.
    METHODS get_mult_reported EXPORTING tt_reported TYPE zeho_activity_if~tty_mult_reported.
    METHODS clear_mult_reported.
    METHODS set_global_features IMPORTING tt_features TYPE zeho_activity_if~tty_feature.
    METHODS get_global_features EXPORTING tt_features TYPE zeho_activity_if~tty_feature.
    METHODS check_mult_fields IMPORTING tt_mult   TYPE zeho_activity_if~tt_activity_multi
                                        i_action  TYPE zeho_de_action
                              EXPORTING tt_report TYPE zeho_activity_if~tty_mult_reported.
    METHODS change_updated_fields
      IMPORTING tt_upd_entities  TYPE zeho_activity_if~tty_update_entity
      EXPORTING tt_mult          TYPE zeho_activity_if~tt_activity_multi
                tt_mult_reported TYPE zeho_activity_if~tty_mult_reported.

    METHODS change_updated_fields_header
      IMPORTING tt_upd_entities  TYPE zeho_activity_if~tty_update_entity_header
      EXPORTING tt_mult          TYPE zeho_activity_if~tt_activity_header
                tt_mult_reported TYPE zeho_activity_if~tty_reported.

*
    METHODS check_multiple_post
      IMPORTING tt_act    TYPE zeho_activity_if~tt_activity_out
                tt_multi  TYPE zeho_activity_if~tt_activity_multi
      EXPORTING tt_report TYPE zeho_activity_if~tty_reported.


    METHODS clear_cache.

*    METHOD read_activity.
*                et_message         TYPE tt_message .

  PRIVATE SECTION.
    CLASS-DATA: go_instance TYPE REF TO lcl_buffer.
    DATA : tt_activity TYPE zeho_activity_if~ty_activities.
    DATA : tt_glitem   TYPE zeho_activity_if~lty_glitem.
    DATA : tt_post  TYPE zeho_tt_activities.
    DATA : lt_keys  TYPE zeho_activity_if~tty_act_with_key.
    DATA : tt_reread TYPE zeho_tt_activities.
    DATA : tt_reverse_key TYPE zeho_activity_if~tty_act_with_key.
    DATA : post_bapi_data TYPE zeho_activity_if~tty_bapi_data.
    DATA : tt_global_features TYPE zeho_activity_if~tty_feature.
    DATA : tt_mult_reported TYPE zeho_activity_if~tty_mult_reported.


ENDCLASS.


CLASS lcl_buffer IMPLEMENTATION.
  METHOD get_instance.
    IF go_instance IS NOT BOUND.
      go_instance = NEW #( ).
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.

  METHOD get_data.
    DATA lt_activity  TYPE TABLE FOR READ IMPORT zeho_i_activities.
    CLEAR lt_activity.
    IF it_activity IS SUPPLIED.
*       lt_activity = it_activity.
      IF tt_activity IS NOT INITIAL.

        LOOP AT it_activity ASSIGNING FIELD-SYMBOL(<fs_activity>).
          IF line_exists( tt_activity[  bankcode                   = <fs_activity>-bankcode
                                        bukrs                      = <fs_activity>-bukrs
                                        iban                       = <fs_activity>-iban
                                        branch                     = <fs_activity>-branch
                                        acccount_no                = <fs_activity>-acccount_no
                                        act_date                   = <fs_activity>-act_date
                                        act_time                   = <fs_activity>-act_time
                                        act_no                     = <fs_activity>-act_no  ] ) .
            IF line_exists( tt_reread[  bankcode                   = <fs_activity>-bankcode
                                        bukrs                      = <fs_activity>-bukrs
                                        iban                       = <fs_activity>-iban
                                        branch                     = <fs_activity>-branch
                                        acccount_no                = <fs_activity>-acccount_no
                                        act_date                   = <fs_activity>-act_date
                                        act_time                   = <fs_activity>-act_time
                                        act_no                     = <fs_activity>-act_no  ] ) .

              APPEND <fs_activity> TO lt_activity.
            ENDIF.



          ELSE.
            APPEND <fs_activity> TO lt_activity.

          ENDIF.
        ENDLOOP.

      ELSE.
        lt_activity = it_activity.
      ENDIF.

      IF lt_activity IS NOT INITIAL.
        zeho_cl_post_buffer=>get_instance(  )->get_data(
          EXPORTING
            it_activity        = lt_activity
          IMPORTING
            et_acitivity       = et_acitivity
            et_activity_failed = et_activity_failed
        ).
      ENDIF.

      IF et_acitivity IS NOT INITIAL.
        DATA: lt_activity_update TYPE zeho_activity_if~ty_activities.
        CLEAR lt_activity_update.
        LOOP AT et_acitivity ASSIGNING FIELD-SYMBOL(<fs_act>).
          READ TABLE tt_activity ASSIGNING FIELD-SYMBOL(<fs_activ>)
                               WITH KEY    bankcode                   = <fs_act>-bankcode
                                           bukrs                      = <fs_act>-bukrs
                                           iban                       = <fs_act>-iban
                                           branch                     = <fs_act>-branch
                                           acccount_no                = <fs_act>-acccount_no
                                           act_date                   = <fs_act>-act_date
                                           act_time                   = <fs_act>-act_time
                                           act_no                     = <fs_act>-act_no   .
          IF sy-subrc = 0.
            <fs_activ> = CORRESPONDING #( <fs_act> ).
          ELSE.
            APPEND INITIAL LINE TO tt_activity ASSIGNING <fs_activ>.
            <fs_activ> = CORRESPONDING #( <fs_act> ).
          ENDIF.
        ENDLOOP.
*      ENDIF.
**        tt_activity = CORRESPONDING #( BASE (  tt_activity ) et_acitivity ).
      ELSE.
        et_acitivity = VALUE #( FOR ls_act IN it_activity
                                FOR act IN tt_activity
                                WHERE   (   bankcode                  = ls_act-bankcode
                                    AND    bukrs                      = ls_act-bukrs
                                    AND    iban                       = ls_act-iban
                                    AND    branch                     = ls_act-branch
                                    AND    acccount_no                = ls_act-acccount_no
                                    AND    act_date                   = ls_act-act_date
                                    AND    act_time                   = ls_act-act_time
                                    AND    act_no                     = ls_act-act_no   )

                                (    CORRESPONDING #( act  )  )

                            ).

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_post.
    IF tt_post IS NOT INITIAL.
      tt_act = tt_post.
    ENDIF.
  ENDMETHOD.
  METHOD set_reverse.

    IF tt_act IS NOT INITIAL.
      tt_reverse_key = tt_act.
    ENDIF.

  ENDMETHOD..

  METHOD get_reverse.
    IF tt_reverse_key IS NOT INITIAL.
      tt_act = tt_reverse_key.
    ENDIF.
  ENDMETHOD.

  METHOD set_post.
    IF tt_act IS NOT INITIAL.
      tt_post = tt_act.
    ENDIF.
  ENDMETHOD.

  METHOD get_keys.
    IF lt_keys IS NOT INITIAL.
      tt_keys = lt_keys.
    ENDIF.
  ENDMETHOD.

  METHOD set_keys.
    IF tt_keys IS NOT INITIAL.
      lt_keys = tt_keys.
    ENDIF.
  ENDMETHOD.

  METHOD set_reread.
    IF tt_act IS NOT INITIAL.
      tt_reread = tt_act.
    ENDIF.
  ENDMETHOD.

  METHOD get_bapi_data.
    IF post_bapi_data IS NOT INITIAL.
      tt_bapi_data = post_bapi_data.
    ENDIF.
  ENDMETHOD.

  METHOD set_bapi_data.
    IF tt_bapi_data IS NOT INITIAL.
      post_bapi_data = tt_bapi_data.
    ENDIF.
  ENDMETHOD.

  METHOD post_doc.

    DATA: lt_je_deep TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
          lv_cid     TYPE abp_behv_cid,
          ls_je      TYPE STRUCTURE FOR ACTION IMPORT i_journalentrytp~post.
    CLEAR:  lt_je_deep , ls_je.


    DATA : cl_document TYPE REF TO zeho_cl_document_processing.


    IF cl_document IS NOT BOUND.
      CREATE OBJECT cl_document.
    ENDIF.

    DATA ls_aa TYPE zeho_i_activities.
    LOOP AT tt_act ASSIGNING FIELD-SYMBOL(<fs_aa>).

      lt_je_deep   = VALUE #( tt_bapidata[
                       bankcode = <fs_aa>-bankcode
                       bukrs    = <fs_aa>-bukrs
                       iban     = <fs_aa>-iban
                       branch   = <fs_aa>-branch
                       act_no   = <fs_aa>-act_no
                       act_date = <fs_aa>-act_date
                       act_time = <fs_aa>-act_time
                       acccount_no = <fs_aa>-acccount_no
                      ]-bapi_data  OPTIONAL ).

      IF lt_je_deep IS NOT INITIAL.
*        TRY.
*            lv_cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
*          CATCH cx_uuid_error.
*            ASSERT 1 = 0.
*        ENDTRY.
*        ls_je-%cid = lv_cid.

        ls_aa = CORRESPONDING #( <fs_aa> ).



**      cl_document->fill_header(
**        EXPORTING
**          rd_aa = ls_aa
**        CHANGING
**          e_je  = ls_je
**      ).
**
**      cl_document->fill_gl_account(
**        EXPORTING
**
**          rd_aa     = ls_aa
**          e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
**                      THEN 'S' ELSE 'H'  )
**          e_hkont   = <fs_aa>-hkont
**        CHANGING
**          tt_glitems = ls_je-%param-_glitems
**      ).
**
**      IF <fs_aa>-secondgl_acc IS NOT INITIAL.
**
**
**        cl_document->fill_secondgl_account(
**        EXPORTING
**          rd_aa     = ls_aa
**          e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
**                      THEN 'S' ELSE 'H'  )
**          e_hkont   = <fs_aa>-secondgl_acc
**        CHANGING
**          tt_glitems = ls_je-%param-_glitems
**            ).
**      ELSEIF <fs_aa>-lifnr IS NOT INITIAL.
**
**        cl_document->fill_supplier(
**          EXPORTING
**            rd_aa      = ls_aa
**            e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
**                      THEN 'S' ELSE 'H'  )
**          CHANGING
**            tt_apitems = ls_je-%param-_apitems
**        ).
**      ELSEIF <fs_aa>-kunnr IS NOT INITIAL.
**        cl_document->fill_customer(
**          EXPORTING
**            rd_aa      = ls_aa
**            e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
**                         THEN 'S' ELSE 'H'  )
**          CHANGING
**            tt_aritems =  ls_je-%param-_aritems
**        ).
**      ENDIF.
**
**      APPEND ls_je TO lt_je_deep.

        MODIFY ENTITIES OF i_journalentrytp
          ENTITY journalentry
          EXECUTE post FROM lt_je_deep
          FAILED FINAL(ls_failed_deep)
          REPORTED FINAL(ls_reported_deep)
          MAPPED FINAL(ls_mapped_deep) .


        IF ls_failed_deep IS NOT INITIAL.
*        APPEND INITIAL LINE TO tt_keys ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
*        <fs_temp_key>-act = CORRESPONDING #( ls_aa ).
*        <fs_temp_key>-cid = lv_cid.
*        <fs_temp_key>-pid = VALUE #( ls_mapped_deep-journalentry[ %cid = lv_cid ]-%pid OPTIONAL ).
          LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
            DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).

            APPEND VALUE #( bankcode = ls_aa-bankcode
                            bukrs    = ls_aa-bukrs
                            iban     = ls_aa-iban
                            branch   = ls_aa-branch
                            act_no   = ls_aa-act_no
                            act_date = ls_aa-act_date
                            act_time = ls_aa-act_time
                            acccount_no = ls_aa-acccount_no
                        %msg = <ls_reported_deep>-%msg ) TO tt_reported.

*         APPEND VALUE #( bankcode = ls_aa-bankcode
*                          = if_abap_behv=>mk-on
*                          %is_draft = if_abap_behv=>mk-on
*                          %msg = ls_report-%msg ) TO tt_reported.
          ENDLOOP.
        ELSE.

          APPEND INITIAL LINE TO tt_keys  ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
          <fs_temp_key>-act = CORRESPONDING #( ls_aa ).
          <fs_temp_key>-cid = lt_je_deep[ 1 ]-%cid.
          <fs_temp_key>-pid = VALUE #( ls_mapped_deep-journalentry[ %cid = <fs_temp_key>-cid ]-%pid OPTIONAL ).

        ENDIF.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_multiple_post.

    DATA lv_amount TYPE zeho_a_aa-amount.


    LOOP AT tt_act ASSIGNING FIELD-SYMBOL(<fs_act>).

*    lv_amount = REDUCE #(  )


      IF <fs_act>-blart IS INITIAL.

        LOOP AT tt_multi TRANSPORTING NO FIELDS WHERE  bankcode = <fs_act>-bankcode
                                                  AND  bukrs    = <fs_act>-bukrs
                                                  AND  iban     = <fs_act>-iban
                                                  AND  branch   = <fs_act>-branch
                                                  AND  act_no   = <fs_act>-act_no
                                                  AND  act_date = <fs_act>-act_date
                                                  AND  act_time = <fs_act>-act_time
                                                  AND  acccount_no = <fs_act>-acccount_no
                                                  AND  blart <> ' '    .




        ENDLOOP.
        IF sy-subrc <> 0.

          APPEND VALUE #(

           bankcode = <fs_act>-bankcode
           bukrs    = <fs_act>-bukrs
           iban     = <fs_act>-iban
           branch   = <fs_act>-branch
           act_no   = <fs_act>-act_no
           act_date = <fs_act>-act_date
           act_time = <fs_act>-act_time
           acccount_no = <fs_act>-acccount_no
           %msg =  NEW zeho_cl_messages(
                      textid   = zeho_cl_messages=>document_type_missing
                      severity = if_abap_behv_message=>severity-error
                 )

             ) TO  tt_report .

        ENDIF.

      ENDIF.


      LOOP AT tt_multi TRANSPORTING NO FIELDS WHERE  bankcode = <fs_act>-bankcode
                                               AND  bukrs    = <fs_act>-bukrs
                                               AND  iban     = <fs_act>-iban
                                               AND  branch   = <fs_act>-branch
                                               AND  act_no   = <fs_act>-act_no
                                               AND  act_date = <fs_act>-act_date
                                               AND  act_time = <fs_act>-act_time
                                               AND  acccount_no = <fs_act>-acccount_no.
        EXIT.

      ENDLOOP.
      IF sy-subrc <> 0.

        APPEND VALUE #(

           bankcode = <fs_act>-bankcode
           bukrs    = <fs_act>-bukrs
           iban     = <fs_act>-iban
           branch   = <fs_act>-branch
           act_no   = <fs_act>-act_no
           act_date = <fs_act>-act_date
           act_time = <fs_act>-act_time
           acccount_no = <fs_act>-acccount_no
           %msg =  NEW zeho_cl_messages(
                      textid   = zeho_cl_messages=>item_not_found
                      severity = if_abap_behv_message=>severity-error
                 )

             ) TO  tt_report .

      ENDIF.


      lv_amount = REDUCE #( INIT val_101 TYPE zeho_de_amount
                       FOR wa IN tt_multi
                     WHERE (  bankcode EQ <fs_act>-bankcode
                         AND  bukrs    EQ <fs_act>-bukrs
                         AND  iban     EQ <fs_act>-iban
                         AND  branch   EQ <fs_act>-branch
                         AND  act_no   EQ <fs_act>-act_no
                         AND  act_date EQ <fs_act>-act_date
                         AND  act_time EQ <fs_act>-act_time
                         AND  acccount_no EQ <fs_act>-acccount_no  )
                      NEXT val_101 = val_101 +  COND #( WHEN  wa-shkzg = 'S'
                                                          THEN wa-amount
                                                           ELSE wa-amount * -1  )

                           ).

      IF lv_amount IS NOT INITIAL.

        APPEND VALUE #(

               bankcode = <fs_act>-bankcode
               bukrs    = <fs_act>-bukrs
               iban     = <fs_act>-iban
               branch   = <fs_act>-branch
               act_no   = <fs_act>-act_no
               act_date = <fs_act>-act_date
               act_time = <fs_act>-act_time
               acccount_no = <fs_act>-acccount_no
               %msg =  NEW zeho_cl_messages(
                          textid   = zeho_cl_messages=>amount_cannot_be_null
                          severity = if_abap_behv_message=>severity-error
                          mv_amount = lv_amount
                     )

                 ) TO  tt_report .

      ENDIF.


      DATA(multi_temp) = tt_multi.
      DELETE multi_temp WHERE    bankcode <> <fs_act>-bankcode
                          AND    bukrs    <> <fs_act>-bukrs
                          AND    iban     <> <fs_act>-iban
                          AND    branch   <> <fs_act>-branch
                          AND    act_no   <> <fs_act>-act_no
                          AND    act_date <> <fs_act>-act_date
                          AND    act_time <> <fs_act>-act_time
                          AND    acccount_no <> <fs_act>-acccount_no    .
      SORT multi_temp BY waers.
      DELETE ADJACENT DUPLICATES FROM multi_temp COMPARING waers.
      IF lines( multi_temp ) GT 1.
        APPEND VALUE #(

         bankcode = <fs_act>-bankcode
         bukrs    = <fs_act>-bukrs
         iban     = <fs_act>-iban
         branch   = <fs_act>-branch
         act_no   = <fs_act>-act_no
         act_date = <fs_act>-act_date
         act_time = <fs_act>-act_time
         acccount_no = <fs_act>-acccount_no
         %msg =  NEW zeho_cl_messages(
                    textid   = zeho_cl_messages=>multiple_currency
                    severity = if_abap_behv_message=>severity-error
                    mv_amount = lv_amount
               )
           ) TO  tt_report .
        DELETE multi_temp WHERE waers IS INITIAL.
        IF multi_temp IS INITIAL.
          APPEND VALUE #(

         bankcode = <fs_act>-bankcode
         bukrs    = <fs_act>-bukrs
         iban     = <fs_act>-iban
         branch   = <fs_act>-branch
         act_no   = <fs_act>-act_no
         act_date = <fs_act>-act_date
         act_time = <fs_act>-act_time
         acccount_no = <fs_act>-acccount_no
         %msg =  NEW zeho_cl_messages(
                    textid   = zeho_cl_messages=>fill_currency
                    severity = if_abap_behv_message=>severity-error
                    mv_amount = lv_amount
               )
           ) TO  tt_report .
        ENDIF.
        endif.

      ENDLOOP.
    ENDMETHOD.

    METHOD check_mult_fields.

      LOOP AT tt_mult ASSIGNING FIELD-SYMBOL(<entity>).
        SELECT COUNT( * )
        FROM zeho_a_log
        WHERE bankcode    = @<entity>-bankcode
          AND bukrs       = @<entity>-bukrs
          AND iban        = @<entity>-iban
          AND branch      = @<entity>-branch
          AND acccount_no = @<entity>-acccount_no
          AND act_date    = @<entity>-act_date
          AND act_time    = @<entity>-act_time
          AND act_no      = @<entity>-act_no
          AND ( belnr <> ' ' OR cancel_process <> ' ' ).
          IF sy-subrc = 0.

            APPEND VALUE #(
            bankcode    =       <entity>-bankcode
            bukrs       =       <entity>-bukrs
            iban        =       <entity>-iban
            branch      =       <entity>-branch
            acccount_no =      <entity>-acccount_no
            act_date    =       <entity>-act_date
            act_time    =       <entity>-act_time
            act_no      =       <entity>-act_no
            %msg        =  NEW zeho_cl_messages(
                     textid   = zeho_cl_messages=>activity_posted_or_canceled
                     severity = if_abap_behv_message=>severity-error
                     )
            ) TO tt_report.
          ENDIF.
        ENDLOOP.

      ENDMETHOD.

      METHOD change_updated_fields.
        DATA lt_mult TYPE TABLE OF zeho_a_aa_mult.

        FIELD-SYMBOLS : <fs_old_value> TYPE any,
                        <fs_new_value> TYPE any,
                        <fs_control>   TYPE any.
        CONSTANTS : c_acc TYPE tabname VALUE 'ZEHO_A_AA_MULT'.
        CONSTANTS c_field TYPE char20 VALUE '%CONTROL-'.
        CONSTANTS c_updated TYPE char2 VALUE '01'.
        DATA lv_fieldname_full TYPE char30.
        SELECT *
        FROM zeho_a_aa_mult
        FOR ALL ENTRIES IN @tt_upd_entities
                WHERE  bankcode    =      @tt_upd_entities-bankcode
                  AND  bukrs       =      @tt_upd_entities-bukrs
                  AND  iban        =      @tt_upd_entities-iban
                  AND  branch      =      @tt_upd_entities-branch
                  AND  acccount_no =      @tt_upd_entities-acccount_no
                  AND  act_date    =      @tt_upd_entities-act_date
                  AND  act_time    =      @tt_upd_entities-act_time
                  AND  act_no      =      @tt_upd_entities-act_no
                  AND  item_no     =      @tt_upd_entities-item_no
        INTO CORRESPONDING FIELDS OF TABLE @tt_mult.

          DATA : go_struct TYPE REF TO cl_abap_structdescr,
                 gt_comp   TYPE abap_component_tab,
                 gs_comp   TYPE abap_componentdescr.

          go_struct ?= cl_abap_typedescr=>describe_by_name( c_acc ).
          gt_comp = go_struct->get_components(  ).


          LOOP AT tt_mult ASSIGNING FIELD-SYMBOL(<fs_mult>).

            READ TABLE tt_upd_entities ASSIGNING FIELD-SYMBOL(<fs_entity>) WITH KEY  bankcode    =      <fs_mult>-bankcode
                                                                                     bukrs       =      <fs_mult>-bukrs
                                                                                     iban        =      <fs_mult>-iban
                                                                                     branch      =      <fs_mult>-branch
                                                                                     acccount_no =      <fs_mult>-acccount_no
                                                                                     act_date    =      <fs_mult>-act_date
                                                                                     act_time    =      <fs_mult>-act_time
                                                                                     act_no      =      <fs_mult>-act_no
                                                                                     item_no     =      <fs_mult>-item_no      .
            IF sy-subrc = 0.

              LOOP AT gt_comp INTO gs_comp.

                lv_fieldname_full = c_field  && gs_comp-name.

                ASSIGN COMPONENT lv_fieldname_full OF STRUCTURE <fs_entity> TO <fs_control>.
                IF <fs_control> IS ASSIGNED.

                  IF <fs_control> = c_updated .

                    ASSIGN COMPONENT gs_comp-name OF STRUCTURE <fs_entity> TO <fs_new_value>.
                    ASSIGN COMPONENT gs_comp-name OF STRUCTURE <fs_mult>   TO <fs_old_value>.

                    IF <fs_new_value> IS ASSIGNED AND <fs_old_value> IS ASSIGNED.

                      IF <fs_new_value> <> <fs_old_value> .
                        <fs_old_value>  = <fs_new_value>.
                      ENDIF.
                    ENDIF.
                    UNASSIGN : <fs_new_value> , <fs_old_value>.

                  ELSE.
                    UNASSIGN <fs_control>.
                  ENDIF.


                ENDIF.


              ENDLOOP.


            ENDIF.

          ENDLOOP.

          LOOP AT tt_mult ASSIGNING <fs_mult>.

            IF <fs_mult>-amount IS INITIAL .
              APPEND VALUE #(

            bankcode = <fs_mult>-bankcode
            bukrs    = <fs_mult>-bukrs
            iban     = <fs_mult>-iban
            branch   = <fs_mult>-branch
            act_no   = <fs_mult>-act_no
            act_date = <fs_mult>-act_date
            act_time = <fs_mult>-act_time
            acccount_no = <fs_mult>-acccount_no
            item_no     = <fs_mult>-item_no
            %msg =  NEW zeho_cl_messages(
                       textid   = zeho_cl_messages=>amount_cannot_be_null
                       severity = if_abap_behv_message=>severity-error
              )
          ) TO tt_mult_reported.
            ENDIF.

            IF <fs_mult>-secondgl_acc IS INITIAL AND
               <fs_mult>-lifnr IS INITIAL AND
               <fs_mult>-kunnr IS INITIAL.


              APPEND VALUE #(

                    bankcode = <fs_mult>-bankcode
                    bukrs    = <fs_mult>-bukrs
                    iban     = <fs_mult>-iban
                    branch   = <fs_mult>-branch
                    act_no   = <fs_mult>-act_no
                    act_date = <fs_mult>-act_date
                    act_time = <fs_mult>-act_time
                    acccount_no = <fs_mult>-acccount_no
                    %msg =  NEW zeho_cl_messages(
                               textid   = zeho_cl_messages=>fill_account
                               severity = if_abap_behv_message=>severity-error
                      )
                ) TO tt_mult_reported.

            ENDIF.



          ENDLOOP.



        ENDMETHOD.



        METHOD clear_cache.
          CLEAR :   tt_post , lt_keys, tt_reread , post_bapi_data.

        ENDMETHOD.



        METHOD get_global_features.

          IF tt_global_features IS NOT INITIAL.
            tt_features = tt_global_features.
          ENDIF.


        ENDMETHOD.

        METHOD set_global_features.
          IF tt_features IS NOT INITIAL.
            tt_global_features = tt_features.
          ENDIF.
        ENDMETHOD.

        METHOD get_mult_reported.
          IF tt_mult_reported IS NOT INITIAL.
            tt_reported = tt_mult_reported.
          ENDIF.
        ENDMETHOD.

        METHOD set_mult_reported.
          IF tt_reported IS NOT INITIAL.
            tt_mult_reported = tt_reported.
          ENDIF.
        ENDMETHOD.
        METHOD clear_mult_reported.
          CLEAR tt_mult_reported.
        ENDMETHOD.

        METHOD change_updated_fields_header.

          DATA lt_mult TYPE TABLE OF zeho_i_activities.
*    FIELD-SYMBOLS : <fs_entity> TYPE zeho_i_activities.
          FIELD-SYMBOLS : <fs_old_value> TYPE any,
                          <fs_new_value> TYPE any,
                          <fs_control>   TYPE any.
          CONSTANTS : c_acc TYPE tabname VALUE 'ZEHO_I_ACTIVITIES'.
          CONSTANTS c_field TYPE char20 VALUE '%CONTROL-'.
          CONSTANTS c_updated TYPE char2 VALUE '01'.
          DATA lv_fieldname_full TYPE char30.
          SELECT *
          FROM zeho_i_activities
          FOR ALL ENTRIES IN @tt_upd_entities
                  WHERE  bankcode    =      @tt_upd_entities-bankcode
                    AND  bukrs       =      @tt_upd_entities-bukrs
                    AND  iban        =      @tt_upd_entities-iban
                    AND  branch      =      @tt_upd_entities-branch
                    AND  acccount_no =      @tt_upd_entities-acccount_no
                    AND  act_date    =      @tt_upd_entities-act_date
                    AND  act_time    =      @tt_upd_entities-act_time
                    AND  act_no      =      @tt_upd_entities-act_no
          INTO CORRESPONDING FIELDS OF TABLE @tt_mult.

            DATA : go_struct TYPE REF TO cl_abap_structdescr,
                   gt_comp   TYPE abap_component_tab,
                   gs_comp   TYPE abap_componentdescr.

            go_struct ?= cl_abap_typedescr=>describe_by_name( c_acc ).
            gt_comp = go_struct->get_components(  ).


            LOOP AT tt_mult ASSIGNING FIELD-SYMBOL(<fs_mult>).

              READ TABLE tt_upd_entities ASSIGNING FIELD-SYMBOL(<fs_entity>) WITH KEY  bankcode    =      <fs_mult>-bankcode
                                                                                       bukrs       =      <fs_mult>-bukrs
                                                                                       iban        =      <fs_mult>-iban
                                                                                       branch      =      <fs_mult>-branch
                                                                                       acccount_no =      <fs_mult>-acccount_no
                                                                                       act_date    =      <fs_mult>-act_date
                                                                                       act_time    =      <fs_mult>-act_time
                                                                                       act_no      =      <fs_mult>-act_no.
*                                                                               item_no     =      <fs_mult>-item_no      .
              IF sy-subrc = 0.

                LOOP AT gt_comp INTO gs_comp.

                  lv_fieldname_full = c_field  && gs_comp-name.

                  ASSIGN COMPONENT lv_fieldname_full OF STRUCTURE <fs_entity> TO <fs_control>.
                  IF <fs_control> IS ASSIGNED.

                    IF <fs_control> = c_updated .

                      ASSIGN COMPONENT gs_comp-name OF STRUCTURE <fs_entity> TO <fs_new_value>.
                      ASSIGN COMPONENT gs_comp-name OF STRUCTURE <fs_mult>   TO <fs_old_value>.

                      IF <fs_new_value> IS ASSIGNED AND <fs_old_value> IS ASSIGNED.

                        IF <fs_new_value> <> <fs_old_value> .
                          <fs_old_value>  = <fs_new_value>.
                        ENDIF.
                      ENDIF.
                      UNASSIGN : <fs_new_value> , <fs_old_value>.

                    ELSE.
                      UNASSIGN <fs_control>.
                    ENDIF.


                  ENDIF.


                ENDLOOP.


              ENDIF.

            ENDLOOP.

*    LOOP AT tt_mult ASSIGNING <fs_mult>.
*
*      IF <fs_mult>-amount IS INITIAL .
*        APPEND VALUE #(
*
*      bankcode = <fs_mult>-bankcode
*      bukrs    = <fs_mult>-bukrs
*      iban     = <fs_mult>-iban
*      branch   = <fs_mult>-branch
*      act_no   = <fs_mult>-act_no
*      act_date = <fs_mult>-act_date
*      act_time = <fs_mult>-act_time
*      acccount_no = <fs_mult>-acccount_no
**      item_no     = <fs_mult>-item_no
*      %msg =  NEW zeho_cl_messages(
*                 textid   = zeho_cl_messages=>amount_cannot_be_null
*                 severity = if_abap_behv_message=>severity-error
*        )
*    ) TO tt_mult_reported.
*      ENDIF.
*
*      IF <fs_mult>-secondgl_acc IS INITIAL AND
*         <fs_mult>-lifnr IS INITIAL AND
*         <fs_mult>-kunnr IS INITIAL.
*
*
*        APPEND VALUE #(
*
*              bankcode = <fs_mult>-bankcode
*              bukrs    = <fs_mult>-bukrs
*              iban     = <fs_mult>-iban
*              branch   = <fs_mult>-branch
*              act_no   = <fs_mult>-act_no
*              act_date = <fs_mult>-act_date
*              act_time = <fs_mult>-act_time
*              acccount_no = <fs_mult>-acccount_no
*              %msg =  NEW zeho_cl_messages(
*                         textid   = zeho_cl_messages=>fill_account
*                         severity = if_abap_behv_message=>severity-error
*                )
*          ) TO tt_mult_reported.
*
*      ENDIF.
*
*
*
*    ENDLOOP.


          ENDMETHOD.

ENDCLASS.


CLASS lhc_activities DEFINITION INHERITING FROM cl_abap_behavior_handler.

PUBLIC SECTION.
  INTERFACES zeho_activity_if.


  DATA : tt_act_key TYPE zeho_activity_if~tty_act_with_key.
  DATA instance TYPE REF TO zeho_badi_activity.
  DATA badi_imp_exists TYPE REF TO zeho_badi_activity.
  DATA imp_class_tab  TYPE STANDARD TABLE OF REF TO zif_ex_eho_activity_imp .
  DATA imp_class      TYPE REF TO zif_ex_eho_activity_imp.

PRIVATE SECTION.


  METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
    IMPORTING keys REQUEST requested_authorizations FOR activities RESULT result.


  METHODS read FOR READ
    IMPORTING keys FOR READ activities RESULT result.

  METHODS lock FOR LOCK
    IMPORTING keys FOR LOCK activities.

  METHODS multiplesave FOR MODIFY
    IMPORTING keys FOR ACTION activities~multiplesave .
  METHODS reverseactivity FOR MODIFY
    IMPORTING keys FOR ACTION activities~reverseactivity. "RESULT result.

  METHODS reverseactivityself FOR MODIFY
    IMPORTING keys FOR ACTION activities~reverseactivityself RESULT result.

  METHODS get_instance_features FOR INSTANCE FEATURES
    IMPORTING keys REQUEST requested_features FOR activities RESULT result.
  METHODS update FOR MODIFY
    IMPORTING entities FOR UPDATE activities.
  METHODS checkmaintenance FOR MODIFY
    IMPORTING keys FOR ACTION activities~checkmaintenance .
  METHODS saveactivity FOR MODIFY
    IMPORTING keys FOR ACTION activities~saveactivity.
  METHODS checkmaintenanceself FOR MODIFY
    IMPORTING keys FOR ACTION activities~checkmaintenanceself RESULT result.
  METHODS saveactivityself FOR MODIFY
    IMPORTING keys FOR ACTION activities~saveactivityself RESULT result.
  METHODS rba_mult FOR READ
    IMPORTING keys_rba FOR READ activities\_mult FULL result_requested RESULT result LINK association_links.

  METHODS cba_mult FOR MODIFY
    IMPORTING entities_cba FOR CREATE activities\_mult.
  METHODS multiplesaveself FOR MODIFY
    IMPORTING keys FOR ACTION activities~multiplesaveself RESULT result.


ENDCLASS.

CLASS lhc_activities IMPLEMENTATION.



METHOD get_instance_authorizations.
ENDMETHOD.



METHOD read.

  IF 1 = 2.
  ENDIF.
  DATA(lo_buffer) = lcl_buffer=>get_instance( ).

  lo_buffer->get_data(
    EXPORTING
      it_activity        = keys
    IMPORTING
      et_acitivity       = result
      et_activity_failed = failed-activities
*         et_message         =
  ).
ENDMETHOD.

METHOD lock.
ENDMETHOD.

METHOD multiplesave.

  DATA:
    lt_je_validate TYPE TABLE FOR FUNCTION IMPORT i_journalentrytp~validate,
    ls_je_validate TYPE STRUCTURE FOR FUNCTION IMPORT i_journalentrytp~validate,
    lt_je_deep     TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
    lv_cid         TYPE abp_behv_cid,
    ls_je          TYPE STRUCTURE FOR ACTION IMPORT  i_journalentrytp~post.
  DATA : cl_document TYPE REF TO zeho_cl_document_processing.
  DATA tt_bapidata TYPE  zeho_activity_if~tty_bapi_data.

  READ ENTITIES OF zeho_i_activities IN LOCAL MODE
   ENTITY activities
   ALL FIELDS
   WITH CORRESPONDING #( keys )
   RESULT FINAL(activities)
   ENTITY activities
   BY \_mult
   ALL FIELDS WITH CORRESPONDING #( keys )
   RESULT FINAL(activities_mult).

  DATA : lt_reported TYPE TABLE FOR REPORTED zeho_i_activities\\activities.

  CLEAR lt_reported.

  DATA(lo_buffer) = lcl_buffer=>get_instance( ).

  lo_buffer->check_multiple_post(
    EXPORTING
      tt_act    = activities
      tt_multi  = activities_mult
    IMPORTING
      tt_report = lt_reported
  ).

  IF lt_reported IS NOT INITIAL.
    reported-activities = CORRESPONDING #( lt_reported ).
  ENDIF.
  CHECK lt_reported IS INITIAL.


  DATA tt_act TYPE  zeho_tt_activities.
  tt_act = CORRESPONDING #(  activities  ).

  CLEAR:  lt_je_deep , ls_je , tt_bapidata.

  IF cl_document IS NOT BOUND.
    CREATE OBJECT cl_document.
  ENDIF.

  DATA ls_aa TYPE zeho_i_activities.

  LOOP AT tt_act ASSIGNING FIELD-SYMBOL(<fs_aa>).


    CLEAR  : lt_je_deep , lt_je_validate , ls_je , ls_je_validate.
    TRY.
        lv_cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
      CATCH cx_uuid_error.
        ASSERT 1 = 0.
    ENDTRY.
    ls_je-%cid = lv_cid.



    IF  <fs_aa>-blart IS INITIAL.

      LOOP AT activities_mult ASSIGNING FIELD-SYMBOL(<fs_mult>) WHERE bankcode = <fs_aa>-bankcode
                                                                  AND bukrs    = <fs_aa>-bukrs
                                                                  AND iban     = <fs_aa>-iban
                                                                  AND branch   = <fs_aa>-branch
                                                                  AND act_no   = <fs_aa>-act_no
                                                                  AND act_date = <fs_aa>-act_date
                                                                  AND act_time = <fs_aa>-act_time
                                                                  AND blart    <> ' '.

        <fs_aa>-blart = <fs_mult>-blart.
        EXIT.

      ENDLOOP.
    ENDIF.

    ls_aa = CORRESPONDING #( <fs_aa> ).

    cl_document->fill_header(
      EXPORTING
        rd_aa = ls_aa
      CHANGING
        e_je  = ls_je
    ).


    LOOP AT activities_mult ASSIGNING <fs_mult> WHERE bankcode = <fs_aa>-bankcode
                                                  AND bukrs    = <fs_aa>-bukrs
                                                  AND iban     = <fs_aa>-iban
                                                  AND branch   = <fs_aa>-branch
                                                  AND act_no   = <fs_aa>-act_no
                                                  AND act_date = <fs_aa>-act_date
                                                  AND act_time = <fs_aa>-act_time.

      ls_aa = CORRESPONDING #( <fs_mult> ).

      IF <fs_mult>-secondgl_acc IS NOT INITIAL.


        cl_document->fill_secondgl_account(
        EXPORTING
          rd_aa     = ls_aa
          e_dc      = <fs_mult>-shkzg
          e_hkont   = <fs_mult>-secondgl_acc
        CHANGING
          tt_glitems = ls_je-%param-_glitems
            ).
      ELSEIF <fs_mult>-lifnr IS NOT INITIAL.

        cl_document->fill_supplier(
          EXPORTING
            rd_aa      = ls_aa
            e_dc      = <fs_mult>-shkzg
          CHANGING
            tt_apitems = ls_je-%param-_apitems
        ).
      ELSEIF <fs_mult>-kunnr IS NOT INITIAL.
        cl_document->fill_customer(
          EXPORTING
            rd_aa      = ls_aa
            e_dc      = <fs_mult>-shkzg
          CHANGING
            tt_aritems =  ls_je-%param-_aritems
        ).
      ENDIF.

    ENDLOOP.
    APPEND ls_je TO lt_je_deep.

    lt_je_validate = CORRESPONDING #( lt_je_deep ).

    READ ENTITIES OF i_journalentrytp
    ENTITY journalentry
    EXECUTE validate FROM lt_je_validate
    RESULT DATA(lt_check_result)
    FAILED DATA(ls_failed_deep)
    REPORTED DATA(ls_reported_deep).


    IF ls_failed_deep IS NOT INITIAL.

      LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
        DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).

        APPEND VALUE #( bankcode = <fs_aa>-bankcode
                        bukrs    = <fs_aa>-bukrs
                        iban     = <fs_aa>-iban
                        branch   = <fs_aa>-branch
                        act_no   = <fs_aa>-act_no
                        act_date = <fs_aa>-act_date
                        act_time = <fs_aa>-act_time
                        acccount_no = <fs_aa>-acccount_no
                        %msg = <ls_reported_deep>-%msg ) TO reported-activities.

      ENDLOOP.
    ELSE.

      APPEND VALUE #(

         bankcode = <fs_aa>-bankcode
         bukrs    = <fs_aa>-bukrs
         iban     = <fs_aa>-iban
         branch   = <fs_aa>-branch
         act_no   = <fs_aa>-act_no
         act_date = <fs_aa>-act_date
         act_time = <fs_aa>-act_time
         acccount_no = <fs_aa>-acccount_no
         bapi_data   = CORRESPONDING #( lt_je_deep )
      ) TO tt_bapidata.


    ENDIF.

  ENDLOOP.

  """""""""""""""""""""""""""""""""""""""""""""""""""""

  CLEAR tt_act.
  LOOP AT activities ASSIGNING FIELD-SYMBOL(<fs_act>).

    IF NOT line_exists( reported-activities[

        bankcode = <fs_act>-bankcode
        bukrs    = <fs_act>-bukrs
        iban     = <fs_act>-iban
        branch   = <fs_act>-branch
        act_no   = <fs_act>-act_no
        act_date = <fs_act>-act_date
        act_time = <fs_act>-act_time
        acccount_no = <fs_act>-acccount_no
    ] ).
      tt_act = VALUE #(  BASE tt_act

              ( CORRESPONDING #( <fs_act> ) )
      ).

    ENDIF.

  ENDLOOP.

  lo_buffer->set_post( tt_act ).
  lo_buffer->set_bapi_data( tt_bapidata ).

  mapped-activities = CORRESPONDING #( activities ).


ENDMETHOD.

METHOD reverseactivity.

  DATA: lt_jr TYPE TABLE FOR ACTION IMPORT i_journalentrytp~reverse.
  DATA : lv_cid     TYPE abp_behv_cid.
  DATA : tt_keys        TYPE zeho_activity_if~tty_act_with_key,
         lt_je_validate TYPE TABLE FOR FUNCTION IMPORT i_journalentrytp~validate,
         ls_je_validate TYPE STRUCTURE FOR FUNCTION IMPORT i_journalentrytp~validate.
  DATA ls_aa TYPE zeho_i_activities.
  DATA : tt_bapidata_rev TYPE zeho_activity_if~tty_bapi_data_reverse.

  CLEAR : tt_bapidata_rev , tt_keys.
  READ ENTITIES OF zeho_i_activities IN LOCAL MODE
  ENTITY activities
  ALL FIELDS WITH
  CORRESPONDING #( keys )
  RESULT DATA(activities)
  FAILED failed.
***
***
***    """"""""""""""""""""""""""""""""""""""""""""""""""""""""
***    LOOP AT activities ASSIGNING FIELD-SYMBOL(<fs_act>).
***      CLEAR lt_jr.
***      ls_aa = CORRESPONDING #( <fs_act> ).
***      APPEND INITIAL LINE TO lt_jr ASSIGNING FIELD-SYMBOL(<jr>).
***      <jr>-companycode = <fs_act>-bukrs.
***      <jr>-fiscalyear  = <fs_act>-gjahr.
***      <jr>-accountingdocument = <fs_act>-belnr.
***      <jr>-%param = VALUE #(
***      postingdate = <fs_act>-act_date
***      reversalreason = '01'
***      createdbyuser = sy-uname
***      ).
***
***
***      lt_je_validate = CORRESPONDING #( lt_jr ).
***      READ ENTITIES OF i_journalentrytp
***      ENTITY journalentry
***      EXECUTE validate FROM lt_je_validate
***      RESULT DATA(lt_check_result)
***      FAILED DATA(ls_failed_deep)
***      REPORTED DATA(ls_reported_deep).
***
***
***      IF ls_failed_deep IS NOT INITIAL.
***
***        LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
***          DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).
***
***          APPEND VALUE #(
***
***                 bankcode = ls_aa-bankcode
***                 bukrs    = ls_aa-bukrs
***                 iban     = ls_aa-iban
***                 branch   = ls_aa-branch
***                 act_no   = ls_aa-act_no
***                 act_date = ls_aa-act_date
***                 act_time = ls_aa-act_time
***                 acccount_no = ls_aa-acccount_no
***
***              ) TO failed-activities.
***
***        ENDLOOP.
***      ELSE.
***
***        APPEND VALUE #(
***
***           bankcode = ls_aa-bankcode
***           bukrs    = ls_aa-bukrs
***           iban     = ls_aa-iban
***           branch   = ls_aa-branch
***           act_no   = ls_aa-act_no
***           act_date = ls_aa-act_date
***           act_time = ls_aa-act_time
***           acccount_no = ls_aa-acccount_no
***           belnr       = ls_aa-belnr
***           gjahr       = ls_aa-gjahr
***           bapi_data   = CORRESPONDING #( lt_jr )
***        ) TO tt_bapidata_rev.
***
***      ENDIF.
***
***
***
***    ENDLOOP.
***
***    LOOP  AT tt_bapidata_rev ASSIGNING FIELD-SYMBOL(<fs_rev>).
***
***      MODIFY ENTITIES OF i_journalentrytp
***      ENTITY journalentry
***      EXECUTE reverse FROM <fs_rev>-bapi_data
***      FAILED DATA(ls_failed)
***      REPORTED DATA(ls_reported)
***      MAPPED DATA(ls_mapped).
***
***
***      IF ls_mapped-journalentry IS NOT INITIAL.
***
***
***        APPEND INITIAL LINE TO tt_keys  ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
***        <fs_temp_key>-act = CORRESPONDING #( <fs_act> ).
***        <fs_temp_key>-cid = lv_cid.
***        <fs_temp_key>-pid = VALUE #( ls_mapped-journalentry[ %cid = lv_cid ]-%pid OPTIONAL ).
***
***      ENDIF.
***
***    ENDLOOP.
***
***    IF tt_keys IS NOT INITIAL.
***      DATA(lo_buffer) = lcl_buffer=>get_instance( ).
***      lo_buffer->set_reverse(  tt_act = tt_keys  ).
***    ENDIF.
***
***    mapped-activities = CORRESPONDING #( activities ).


  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""



  """"""""""""""""""""""""""""""""""""""""""""""""""""""""

  LOOP AT activities ASSIGNING FIELD-SYMBOL(<fs_act>).
    CLEAR lt_jr.


    APPEND INITIAL LINE TO lt_jr ASSIGNING FIELD-SYMBOL(<jr>).

    <jr>-companycode = <fs_act>-bukrs.
    <jr>-fiscalyear  = <fs_act>-gjahr.
    <jr>-accountingdocument = <fs_act>-belnr.
    <jr>-%param = VALUE #(
    postingdate = <fs_act>-act_date
    reversalreason = '01'
    createdbyuser = sy-uname

    ).

    MODIFY ENTITIES OF i_journalentrytp
      ENTITY journalentry
      EXECUTE reverse FROM lt_jr
      FAILED DATA(ls_failed)
      REPORTED DATA(ls_reported)
      MAPPED DATA(ls_mapped).

    LOOP AT ls_reported-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
      DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).

      APPEND VALUE #( bankcode = <fs_act>-bankcode
                      bukrs    = <fs_act>-bukrs
                      iban     = <fs_act>-iban
                      branch   = <fs_act>-branch
                      act_no   = <fs_act>-act_no
                      act_date = <fs_act>-act_date
                      act_time = <fs_act>-act_time
                      acccount_no = <fs_act>-acccount_no
             %msg = <ls_reported_deep>-%msg ) TO reported-activities.


    ENDLOOP.

    IF ls_mapped-journalentry IS NOT INITIAL.


      APPEND INITIAL LINE TO tt_keys  ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
      <fs_temp_key>-act = CORRESPONDING #( <fs_act> ).
      <fs_temp_key>-cid = lv_cid.
      <fs_temp_key>-pid = VALUE #( ls_mapped-journalentry[ %cid = lv_cid ]-%pid OPTIONAL ).

    ENDIF.


  ENDLOOP.

  IF tt_keys IS NOT INITIAL.
    DATA(lo_buffer) = lcl_buffer=>get_instance( ).
    lo_buffer->set_reverse(  tt_act = tt_keys  ).
  ENDIF.
  mapped-activities = CORRESPONDING #( activities ).



ENDMETHOD.

METHOD reverseactivityself.
  DATA: lt_jr TYPE TABLE FOR ACTION IMPORT i_journalentrytp~reverse.
  DATA : lv_cid     TYPE abp_behv_cid.
  DATA : tt_keys        TYPE zeho_activity_if~tty_act_with_key,
         lt_je_validate TYPE TABLE FOR FUNCTION IMPORT i_journalentrytp~validate,
         ls_je_validate TYPE STRUCTURE FOR FUNCTION IMPORT i_journalentrytp~validate.
  DATA ls_aa TYPE zeho_i_activities.
  DATA : tt_bapidata_rev TYPE zeho_activity_if~tty_bapi_data_reverse.
  DATA  tt_activities TYPE zeho_tt_activities.

  CLEAR : tt_bapidata_rev , tt_keys.
  READ ENTITIES OF zeho_i_activities IN LOCAL MODE
  ENTITY activities
  ALL FIELDS WITH
  CORRESPONDING #( keys )
  RESULT DATA(activities)
  FAILED failed.

  tt_activities = CORRESPONDING #( activities ).

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""
***    LOOP AT activities ASSIGNING FIELD-SYMBOL(<fs_act>).
***      CLEAR lt_jr.
***      ls_aa = CORRESPONDING #( <fs_act> ).
***      APPEND INITIAL LINE TO lt_jr ASSIGNING FIELD-SYMBOL(<jr>).
***      <jr>-companycode = <fs_act>-bukrs.
***      <jr>-fiscalyear  = <fs_act>-gjahr.
***      <jr>-accountingdocument = <fs_act>-belnr.
***      <jr>-%param = VALUE #(
***      postingdate = <fs_act>-act_date
***      reversalreason = '01'
***      createdbyuser = sy-uname
***      ).
***
***
***      lt_je_validate = CORRESPONDING #( lt_jr ).
***      READ ENTITIES OF i_journalentrytp
***      ENTITY journalentry
***      EXECUTE validate FROM lt_je_validate
***      RESULT DATA(lt_check_result)
***      FAILED DATA(ls_failed_deep)
***      REPORTED DATA(ls_reported_deep).
***
***
***      IF ls_failed_deep IS NOT INITIAL.
***
***        LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
***          DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).
***
***          APPEND VALUE #(
***
***                 bankcode = ls_aa-bankcode
***                 bukrs    = ls_aa-bukrs
***                 iban     = ls_aa-iban
***                 branch   = ls_aa-branch
***                 act_no   = ls_aa-act_no
***                 act_date = ls_aa-act_date
***                 act_time = ls_aa-act_time
***                 acccount_no = ls_aa-acccount_no
***
***              ) TO failed-activities.
***
***        ENDLOOP.
***      ELSE.
***
***        APPEND VALUE #(
***
***           bankcode = ls_aa-bankcode
***           bukrs    = ls_aa-bukrs
***           iban     = ls_aa-iban
***           branch   = ls_aa-branch
***           act_no   = ls_aa-act_no
***           act_date = ls_aa-act_date
***           act_time = ls_aa-act_time
***           acccount_no = ls_aa-acccount_no
***           belnr       = ls_aa-belnr
***           gjahr       = ls_aa-gjahr
***           bapi_data   = CORRESPONDING #( lt_jr )
***        ) TO tt_bapidata_rev.
***
***      ENDIF.
***
***
***
***    ENDLOOP.
***
***    LOOP  AT tt_bapidata_rev ASSIGNING FIELD-SYMBOL(<fs_rev>).
***
***      MODIFY ENTITIES OF i_journalentrytp
***      ENTITY journalentry
***      EXECUTE reverse FROM <fs_rev>-bapi_data
***      FAILED DATA(ls_failed)
***      REPORTED DATA(ls_reported)
***      MAPPED DATA(ls_mapped).
***
***
***      IF ls_mapped-journalentry IS NOT INITIAL.
***
***
***        APPEND INITIAL LINE TO tt_keys  ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
***        <fs_temp_key>-act = CORRESPONDING #( <fs_act> ).
***        <fs_temp_key>-cid = lv_cid.
***        <fs_temp_key>-pid = VALUE #( ls_mapped-journalentry[ %cid = lv_cid ]-%pid OPTIONAL ).
***
***      ENDIF.
***
***    ENDLOOP.
***
***    IF tt_keys IS NOT INITIAL.
***      DATA(lo_buffer) = lcl_buffer=>get_instance( ).
***      lo_buffer->set_reverse(  tt_act = tt_keys  ).
***    ENDIF.



  LOOP AT activities ASSIGNING FIELD-SYMBOL(<fs_act>).
    CLEAR lt_jr.


    APPEND INITIAL LINE TO lt_jr ASSIGNING FIELD-SYMBOL(<jr>).

    <jr>-companycode = <fs_act>-bukrs.
    <jr>-fiscalyear  = <fs_act>-gjahr.
    <jr>-accountingdocument = <fs_act>-belnr.
    <jr>-%param = VALUE #(
    postingdate = <fs_act>-act_date
    reversalreason = '01'
    createdbyuser = sy-uname

    ).

    MODIFY ENTITIES OF i_journalentrytp
      ENTITY journalentry
      EXECUTE reverse FROM lt_jr
      FAILED DATA(ls_failed)
      REPORTED DATA(ls_reported)
      MAPPED DATA(ls_mapped).

    LOOP AT ls_reported-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
      DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).

      APPEND VALUE #( bankcode = <fs_act>-bankcode
                      bukrs    = <fs_act>-bukrs
                      iban     = <fs_act>-iban
                      branch   = <fs_act>-branch
                      act_no   = <fs_act>-act_no
                      act_date = <fs_act>-act_date
                      act_time = <fs_act>-act_time
                      acccount_no = <fs_act>-acccount_no
             %msg = <ls_reported_deep>-%msg ) TO reported-activities.


    ENDLOOP.

    IF ls_mapped-journalentry IS NOT INITIAL.


      APPEND INITIAL LINE TO tt_keys  ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
      <fs_temp_key>-act = CORRESPONDING #( <fs_act> ).
      <fs_temp_key>-cid = lv_cid.
      <fs_temp_key>-pid = VALUE #( ls_mapped-journalentry[ %cid = lv_cid ]-%pid OPTIONAL ).

    ENDIF.


  ENDLOOP.


  IF tt_keys IS NOT INITIAL.
    DATA(lo_buffer) = lcl_buffer=>get_instance( ).
    lo_buffer->set_reverse(  tt_act = tt_keys  ).
    lo_buffer->set_reread(  tt_act = tt_activities ).
  ENDIF.

  result = VALUE #( FOR activity IN activities
(
                    bankcode = activity-bankcode
                    bukrs    = activity-bukrs
                    iban     = activity-iban
                    branch   = activity-branch
                    acccount_no = activity-acccount_no
                    act_date = activity-act_date
                    act_time = activity-act_time
                    act_no   = activity-act_no
                    %param   = activity

                     )
                                   ).
ENDMETHOD.



METHOD get_instance_features.
  DATA : a TYPE abp_behv_op_ctrl .
  READ ENTITIES OF zeho_i_activities IN LOCAL MODE
ENTITY activities
ALL FIELDS WITH
 CORRESPONDING #( keys )
RESULT DATA(activities)
FAILED failed.

*    IF requested_features-%assoc-_Mult <> '01'.

  SELECT  mult~bankcode   ,
 mult~bukrs      ,
 mult~iban       ,
 mult~branch     ,
 mult~acccount_no,
 mult~act_date   ,
 mult~act_time   ,
 mult~act_no     ,
 mult~item_no     ,
 data~belnr,
 data~cancel_process
FROM zeho_a_aa_mult AS mult
INNER JOIN  @activities AS data
ON   mult~bankcode                   = data~bankcode
AND  mult~bukrs                      = data~bukrs
AND  mult~iban                       = data~iban
AND  mult~branch                     = data~branch
AND  mult~acccount_no                = data~acccount_no
AND  mult~act_date                   = data~act_date
AND  mult~act_time                   = data~act_time
AND  mult~act_no                     = data~act_no
INTO TABLE @DATA(lt_multi)    .

    result = VALUE #( FOR activity IN activities
               ( bankcode                   = activity-bankcode
                 bukrs                      = activity-bukrs
                 iban                       = activity-iban
                 branch                     = activity-branch
                 acccount_no                = activity-acccount_no
                 act_date                   = activity-act_date
                 act_time                   = activity-act_time
                 act_no                     = activity-act_no

                 %features-%update      = COND #( WHEN  activity-belnr <> ' '
                                                 THEN if_abap_behv=>fc-o-disabled
*                                                     WHEN activity-cancel_process <> abap_false
*                                                     THEN if_abap_behv=>fc-o-disabled
                                                  ELSE if_abap_behv=>fc-o-enabled   )
                 %action-saveactivity           = COND #( WHEN  activity-belnr <> ' '
                                                 THEN if_abap_behv=>fc-o-disabled
                                                   WHEN activity-cancel_process <> abap_false
                                                 THEN if_abap_behv=>fc-o-disabled
                                                  ELSE if_abap_behv=>fc-o-enabled   )
               %action-saveactivityself           = COND #( WHEN  activity-belnr <> ' '
                                                 THEN if_abap_behv=>fc-o-disabled
                                                   WHEN activity-cancel_process <> abap_false
                                                 THEN if_abap_behv=>fc-o-disabled
                                                  ELSE if_abap_behv=>fc-o-enabled   )
                 %action-multiplesave           = COND #( WHEN  activity-belnr <> ' '
                                                 THEN if_abap_behv=>fc-o-disabled
                                                   WHEN activity-cancel_process <> abap_false
                                                 THEN if_abap_behv=>fc-o-disabled

*                                                   WHEN NOT line_exists( lt_multi[
*                                                       bankcode                   = activity-bankcode
*                                                       bukrs                      = activity-bukrs
*                                                       iban                       = activity-iban
*                                                       branch                     = activity-branch
*                                                       acccount_no                = activity-acccount_no
*                                                       act_date                   = activity-act_date
*                                                       act_time                   = activity-act_time
*                                                       act_no                     = activity-act_no
*                                                      ] ) THEN if_abap_behv=>fc-o-disabled
                                                 ELSE if_abap_behv=>fc-o-enabled   )
                 %action-multiplesaveself    =  COND #( WHEN  activity-belnr <> ' '
                                                 THEN if_abap_behv=>fc-o-disabled
                                                   WHEN activity-cancel_process <> abap_false
                                                 THEN if_abap_behv=>fc-o-disabled

*                                                   WHEN NOT line_exists( lt_multi[
*                                                       bankcode                   = activity-bankcode
*                                                       bukrs                      = activity-bukrs
*                                                       iban                       = activity-iban
*                                                       branch                     = activity-branch
*                                                       acccount_no                = activity-acccount_no
*                                                       act_date                   = activity-act_date
*                                                       act_time                   = activity-act_time
*                                                       act_no                     = activity-act_no
*                                                      ] ) THEN if_abap_behv=>fc-o-disabled
                                                 ELSE if_abap_behv=>fc-o-enabled   )
                 %action-reverseactivity        = COND #( WHEN  activity-belnr <> ' '
                                                 THEN if_abap_behv=>fc-o-enabled
                                                   WHEN activity-cancel_process = abap_false
                                                 THEN if_abap_behv=>fc-o-disabled
                                                 ELSE if_abap_behv=>fc-o-disabled   )
                 %action-reverseactivityself        = COND #( WHEN  activity-belnr <> ' '
                                                 THEN if_abap_behv=>fc-o-enabled
                                                   WHEN activity-cancel_process = abap_false
                                                 THEN if_abap_behv=>fc-o-disabled
                                                 ELSE if_abap_behv=>fc-o-disabled   )
                 %action-checkmaintenance      = COND #( WHEN  activity-belnr = ' ' AND  activity-cancel_process = abap_true
                                                   THEN if_abap_behv=>fc-o-disabled
                                                   WHEN  activity-belnr <> ' ' AND  activity-cancel_process = abap_false
                                                   THEN  if_abap_behv=>fc-o-disabled

*                                                       WHEN activity-cancel_process <> abap_false
*                                                      THEN if_abap_behv=>fc-o-disabled
                                                  ELSE if_abap_behv=>fc-o-enabled   )
              %action-checkmaintenanceself      = COND #( WHEN  activity-belnr = ' ' AND  activity-cancel_process = abap_true
                                                 THEN if_abap_behv=>fc-o-disabled
                                                 WHEN  activity-belnr <> ' ' AND  activity-cancel_process = abap_false
                                                 THEN  if_abap_behv=>fc-o-disabled
*                                                     WHEN activity-cancel_process <> abap_false
*                                                    THEN if_abap_behv=>fc-o-disabled
                                                ELSE if_abap_behv=>fc-o-enabled   )
              %features-%assoc-_mult = COND #(     WHEN  activity-belnr = ' ' AND  activity-cancel_process = abap_true
                                                 THEN if_abap_behv=>fc-o-disabled
                                                 WHEN  activity-belnr <> ' ' AND  activity-cancel_process = abap_false
                                                 THEN  if_abap_behv=>fc-o-disabled
                                                 ELSE if_abap_behv=>fc-o-enabled
*                                                 WHEN line_exists( lt_multi[
*                                                       bankcode                   = activity-bankcode
*                                                       bukrs                      = activity-bukrs
*                                                       iban                       = activity-iban
*                                                       branch                     = activity-branch
*                                                       acccount_no                = activity-acccount_no
*                                                       act_date                   = activity-act_date
*                                                       act_time                   = activity-act_time
*                                                       act_no                     = activity-act_no
*
*
*                                                      ] ) THEN if_abap_behv=>fc-o-enabled
             )

             ) ).

**    DATA tt_features TYPE zeho_activity_if~tty_feature.
**
**
**
**    DATA(lo_buffer) = lcl_buffer=>get_instance(  ).
**    tt_features = VALUE #( FOR activity IN activities
**
**                  (
**                  create  =   COND #( WHEN  activity-belnr = ' ' AND  activity-cancel_process = abap_true
**                               THEN if_abap_behv=>fc-o-disabled
**                               WHEN  activity-belnr <> ' ' AND  activity-cancel_process = abap_false
**                               THEN  if_abap_behv=>fc-o-disabled
***                                   WHEN activity-cancel_process <> abap_false
***                                  THEN if_abap_behv=>fc-o-disabled
**                              ELSE if_abap_behv=>fc-o-enabled   )
**
**                 delete  =   COND #( WHEN  activity-belnr = ' ' AND  activity-cancel_process = abap_true
**                              THEN if_abap_behv=>fc-o-disabled
**                              WHEN  activity-belnr <> ' ' AND  activity-cancel_process = abap_false
**                              THEN  if_abap_behv=>fc-o-disabled
***                                  WHEN activity-cancel_process <> abap_false
***                                 THEN if_abap_behv=>fc-o-disabled
**                             ELSE if_abap_behv=>fc-o-enabled   )
**
**                 update  =   COND #( WHEN  activity-belnr = ' ' AND  activity-cancel_process = abap_true
**                                  THEN if_abap_behv=>fc-o-disabled
**                                  WHEN  activity-belnr <> ' ' AND  activity-cancel_process = abap_false
**                                  THEN  if_abap_behv=>fc-o-disabled
***                                      WHEN activity-cancel_process <> abap_false
***                                     THEN if_abap_behv=>fc-o-disabled
**                                 ELSE if_abap_behv=>fc-o-enabled   )
**                                                )
**                ).
**    IF lines( tt_features ) = 1.
**      lo_buffer->set_global_features( tt_features  ).
***           LOOP AT result ASSIGNING FIELD-SYMBOL(<fs_result>).
***           <fs_result>-%features-%assoc-_Mult
***           endloop.
**M
**    ENDIF.
***    ENDIF.


  ENDMETHOD.

  METHOD update.

    DATA lt_log TYPE TABLE OF zeho_a_log.
    DATA lt_update TYPE TABLE FOR UPDATE zeho_i_activities\\activities.
    DATA(lo_buffer) = lcl_buffer=>get_instance( ).
    DATA lt_data TYPE zeho_activity_if~tt_activity_header.
    DATA lt_reported TYPE zeho_activity_if~tty_reported.
*    lo_buffer->get_mult_reported( IMPORTING tt_reported =  lt_reported_buffer ).
*    DATA lt_entities FOR UPDATE
*    DELETE entities WHERE item_no is initial.
*    IF lt_reported_buffer IS INITIAL .
    lo_buffer->change_updated_fields_header(
      EXPORTING
        tt_upd_entities = entities
      IMPORTING
        tt_mult         = lt_data
        tt_mult_reported = lt_reported
    ).



    lt_log = VALUE #( FOR entity IN lt_data
                        ( CORRESPONDING #( entity MAPPING client = DEFAULT sy-mandt ) )
                         ).

    MODIFY zeho_a_log FROM TABLE @lt_log.

*    mapped-activities = CORRESPONDING #( lt_log ).
*
*
*     READ ENTITIES OF zeho_i_activities IN LOCAL MODE
*    ENTITY Activities
*    ALL FIELDS WITH
*    CORRESPONDING #( lt_log )
*    RESULT DATA(activities)
*    FAILED failed.
    CLEAR lt_update.

*    lt_update = CORRESPONDING #( lt_data ).
*      MODIFY ENTITIES OF zeho_i_activities IN LOCAL MODE
*      ENTITY Activities
*      UPDATE FIELDS ( hkont
*                      belnr
*                      gjahr
*                      blart
*                      lifnr
*                      kunnr
*                      name1
*                      secondgl_acc
*                      kostl
*                      prctr
*                      gsber
*                      umskz
*                      mwskz
*                      cancel_process
*                      customization_type
*                      affacted_priority
*                      local_amount
*                      Status
*                      StatusText
*                       )
*    WITH lt_update
*    .


  ENDMETHOD.

  METHOD checkmaintenance.

    DATA rd_activity TYPE REF TO zeho_s_activity.
    DATA : t_actt        TYPE  zeho_tt_actt,
           t_exp         TYPE  zeho_tt_exp,
           t_cust        TYPE zeho_tt_customer,
           t_supl        TYPE zeho_tt_supplier,
           t_acc         TYPE zeho_tt_acc,
           tt_activities TYPE zeho_tt_activities.
    DATA : tt_bukrs_range TYPE zeho_tt_bukrs_range,
           lv_where       TYPE string.
    DATA : tt_aa_cust TYPE  zeho_tt_act_custom.
    DATA : lt_log TYPE TABLE OF zeho_a_log.

    READ ENTITIES OF zeho_i_activities IN LOCAL MODE
    ENTITY activities
    ALL FIELDS WITH
    CORRESPONDING #( keys )
    RESULT DATA(activities)
    FAILED failed.

    DATA(lt_act) =  activities[]. "CORRESPONDING zeho_i( activities MAPPING FROM ENTITY ).
    SORT lt_act BY bukrs.
    DELETE ADJACENT DUPLICATES FROM lt_act COMPARING bukrs.

    tt_bukrs_range = VALUE #( FOR comp IN lt_act
                             ( sign = 'I' option = 'EQ' low = comp-bukrs high = comp-bukrs )
                               ).

    lv_where = zeho_cl_seltab=>combine_seltabs(
                       it_named_seltabs = VALUE #(
                     (    name = 'BUKRS'    dref = REF #(  tt_bukrs_range    ) )
                      )
                     ).

    zeho_cl_amdp=>get_partners(
           EXPORTING
             i_where     = lv_where
           IMPORTING
             et_customer = t_cust
             et_supplier = t_supl
         ).

    SELECT DISTINCT *
     FROM zeho_a_acc AS acc
    INNER JOIN @activities AS data
       ON data~bankcode = acc~bankcode
      AND data~bukrs    = acc~bukrs
    INTO CORRESPONDING FIELDS OF TABLE @t_acc.

      CLEAR t_exp[].
      SELECT exp~*
      FROM zeho_a_exp  AS exp
      INNER JOIN @activities AS data
               ON data~bankcode = exp~bankcode
              AND data~bukrs    = exp~bukrs
             INTO CORRESPONDING FIELDS OF TABLE  @t_exp.


        CLEAR t_actt[].
        SELECT actt~* FROM zeho_a_actt  AS actt
        INNER JOIN @activities AS data
                 ON data~bankcode = actt~bankcode
                AND data~bukrs    = actt~bukrs
               INTO CORRESPONDING FIELDS OF TABLE  @t_actt.

          tt_activities = CORRESPONDING #( activities ).

          LOOP AT tt_activities REFERENCE INTO rd_activity   WHERE belnr = ' '.
*                                                         AND cancel_process = ' '.

            zeho_cl_document_processing=>fill_list(
              EXPORTING
                tt_exp   = t_exp
                tt_acct  = t_actt
                tt_cust  = t_cust
                tt_suppl = t_supl
                tt_account   = t_acc
              CHANGING
                rd_aa    = rd_activity
            ).

          ENDLOOP.

          tt_aa_cust = VALUE #( FOR ls_wa IN  tt_activities
                               WHERE ( belnr = ' ' )
                                ( CORRESPONDING #( ls_wa MAPPING client = DEFAULT sy-mandt )  )
                              ).
          lt_log = VALUE #( FOR ls_wa IN  tt_activities
                               WHERE ( belnr = ' ' )
                                ( CORRESPONDING #( ls_wa MAPPING client = DEFAULT sy-mandt )  )
                              ).

          MODIFY zeho_a_actcust FROM TABLE @tt_aa_cust.
          MODIFY zeho_a_log FROM TABLE @lt_log.

*    MODIFY ENTITIES OF zeho_i_activities IN LOCAL MODE
*    ENTITY Activities
*    UPDATE FIELDS ( hkont
*                    belnr
*                    gjahr
*                    blart
*                    lifnr
*                    kunnr
*                    name1
*                    secondgl_acc
*                    kostl
*                    prctr
*                    gsber
*                    umskz
*                    mwskz
*                    cancel_process
*                    customization_type
*                    affacted_priority
*                    local_amount
*                    )
*   WITH VALUE #( FOR activity IN tt_activities
*                    ( CORRESPONDING #( activity ) )
*                     ).
*    mapped-activities = VALUE #( for act in tt_activities
*                        ( CORRESPONDING #( act ) )
*                ).


        ENDMETHOD.


        METHOD saveactivity.

          DATA:
            lt_je_validate TYPE TABLE FOR FUNCTION IMPORT i_journalentrytp~validate,
            ls_je_validate TYPE STRUCTURE FOR FUNCTION IMPORT i_journalentrytp~validate,
            lt_je_deep     TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
            lv_cid         TYPE abp_behv_cid,
            ls_je          TYPE STRUCTURE FOR ACTION IMPORT  i_journalentrytp~post.
*          lt_gl      TYPE TABLE FOR ACTION IMPORT  i_journalentrytp~Post-
*          param      TYPE STRUCTURE FOR HIERARCHY d_journalentrypostparameter.
          DATA : cl_document TYPE REF TO zeho_cl_document_processing.
          DATA tt_bapidata TYPE  zeho_activity_if~tty_bapi_data.


          imp_class_tab = zeho_badi_activity=>get_instance(  )->imps.
          imp_class = VALUE #( imp_class_tab[ 1 ]  OPTIONAL ).


          READ ENTITIES OF zeho_i_activities IN LOCAL MODE
          ENTITY activities
          ALL FIELDS WITH
          CORRESPONDING #( keys )
          RESULT DATA(activities)
          FAILED failed.


          DATA(lo_buffer) = lcl_buffer=>get_instance( ).
          DATA tt_act TYPE  zeho_tt_activities.
          tt_act = CORRESPONDING #(  activities  ).

          """"""""""""""""""""""""""""""""""""""""""""""""""""""
          CLEAR:  lt_je_deep , ls_je , tt_bapidata.

          IF cl_document IS NOT BOUND.
            CREATE OBJECT cl_document.
          ENDIF.

          DATA ls_aa TYPE zeho_i_activities.

          LOOP AT tt_act ASSIGNING FIELD-SYMBOL(<fs_aa>).
            CLEAR  : lt_je_deep , lt_je_validate , ls_je , ls_je_validate.
            TRY.
                lv_cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
              CATCH cx_uuid_error.
                ASSERT 1 = 0.
            ENDTRY.
            ls_je-%cid = lv_cid.

            ls_aa = CORRESPONDING #( <fs_aa> ).

            cl_document->fill_header(
              EXPORTING
                rd_aa = ls_aa
              CHANGING
                e_je  = ls_je
            ).

            cl_document->fill_gl_account(
              EXPORTING

                rd_aa     = ls_aa
                e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
                            THEN 'S' ELSE 'H'  )
                e_hkont   = <fs_aa>-hkont
              CHANGING
                tt_glitems = ls_je-%param-_glitems
            ).



            IF <fs_aa>-secondgl_acc IS NOT INITIAL.


              cl_document->fill_secondgl_account(
              EXPORTING
                rd_aa     = ls_aa
                e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
                            THEN 'S' ELSE 'H'  )
                e_hkont   = <fs_aa>-secondgl_acc
              CHANGING
                tt_glitems = ls_je-%param-_glitems
                  ).


            ELSEIF <fs_aa>-lifnr IS NOT INITIAL.

              cl_document->fill_supplier(
                EXPORTING
                  rd_aa      = ls_aa
                  e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
                            THEN 'S' ELSE 'H'  )
                CHANGING
                  tt_apitems = ls_je-%param-_apitems
              ).
            ELSEIF <fs_aa>-kunnr IS NOT INITIAL.
              cl_document->fill_customer(
                EXPORTING
                  rd_aa      = ls_aa
                  e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
                               THEN 'S' ELSE 'H'  )
                CHANGING
                  tt_aritems =  ls_je-%param-_aritems
              ).
            ENDIF.

            APPEND ls_je TO lt_je_deep.


            IF imp_class IS BOUND.
              """ we are not modifying in here but
              "imp method called here because after validate is successful
              "we are using same bapi data for modify
              imp_class->before_modify_je(
                EXPORTING
                  rd_aa = ls_aa
                CHANGING
                  tt_bapidata = lt_je_deep
              ).
              """ we are not modifying in
            ENDIF.
            lt_je_validate = CORRESPONDING #( lt_je_deep ).



            READ ENTITIES OF i_journalentrytp
            ENTITY journalentry
            EXECUTE validate FROM lt_je_validate
            RESULT DATA(lt_check_result)
            FAILED DATA(ls_failed_deep)
            REPORTED DATA(ls_reported_deep).


            IF ls_failed_deep IS NOT INITIAL.

              LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
                DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).

                APPEND VALUE #( bankcode = ls_aa-bankcode
                                bukrs    = ls_aa-bukrs
                                iban     = ls_aa-iban
                                branch   = ls_aa-branch
                                act_no   = ls_aa-act_no
                                act_date = ls_aa-act_date
                                act_time = ls_aa-act_time
                                acccount_no = ls_aa-acccount_no
                                %msg = <ls_reported_deep>-%msg ) TO reported-activities.

              ENDLOOP.
            ELSE.

              APPEND VALUE #(

                 bankcode = ls_aa-bankcode
                 bukrs    = ls_aa-bukrs
                 iban     = ls_aa-iban
                 branch   = ls_aa-branch
                 act_no   = ls_aa-act_no
                 act_date = ls_aa-act_date
                 act_time = ls_aa-act_time
                 acccount_no = ls_aa-acccount_no
                 bapi_data   = CORRESPONDING #( lt_je_deep )
              ) TO tt_bapidata.



            ENDIF.


          ENDLOOP.



          """""""""""""""""""""""""""""""""""""""""""""""""""""

*    tt_act = CORRESPONDING #(  activities  ).
          CLEAR tt_act.
          LOOP AT activities ASSIGNING FIELD-SYMBOL(<fs_act>).

            IF NOT line_exists( reported-activities[

                bankcode = <fs_act>-bankcode
                bukrs    = <fs_act>-bukrs
                iban     = <fs_act>-iban
                branch   = <fs_act>-branch
                act_no   = <fs_act>-act_no
                act_date = <fs_act>-act_date
                act_time = <fs_act>-act_time
                acccount_no = <fs_act>-acccount_no
            ] ).
              tt_act = VALUE #(  BASE tt_act

                      ( CORRESPONDING #( <fs_act> ) )
              ).



            ENDIF.

          ENDLOOP.

          lo_buffer->set_post( tt_act ).
          lo_buffer->set_bapi_data( tt_bapidata ).

          mapped-activities = CORRESPONDING #( activities ).


        ENDMETHOD.

        METHOD checkmaintenanceself.

          DATA rd_activity TYPE REF TO zeho_s_activity.
          DATA : t_actt        TYPE  zeho_tt_actt,
                 t_exp         TYPE  zeho_tt_exp,
                 t_cust        TYPE zeho_tt_customer,
                 t_supl        TYPE zeho_tt_supplier,
                 t_acc         TYPE zeho_tt_acc,
                 tt_activities TYPE zeho_tt_activities.
          DATA : tt_bukrs_range TYPE zeho_tt_bukrs_range,
                 lv_where       TYPE string.
          DATA : tt_aa_cust TYPE  zeho_tt_act_custom.
          DATA : lt_log TYPE TABLE OF zeho_a_log.

          READ ENTITIES OF zeho_i_activities IN LOCAL MODE
          ENTITY activities
          ALL FIELDS WITH
          CORRESPONDING #( keys )
          RESULT DATA(activities)
          FAILED failed.

          DATA(lt_act) =  activities[]. "CORRESPONDING zeho_i( activities MAPPING FROM ENTITY ).
          SORT lt_act BY bukrs.
          DELETE ADJACENT DUPLICATES FROM lt_act COMPARING bukrs.

          tt_bukrs_range = VALUE #( FOR comp IN lt_act
                                   ( sign = 'I' option = 'EQ' low = comp-bukrs high = comp-bukrs )
                                     ).

          lv_where = zeho_cl_seltab=>combine_seltabs(
                             it_named_seltabs = VALUE #(
                           (    name = 'BUKRS'    dref = REF #(  tt_bukrs_range    ) )
                            )
                           ).

          zeho_cl_amdp=>get_partners(
                 EXPORTING
                   i_where     = lv_where
                 IMPORTING
                   et_customer = t_cust
                   et_supplier = t_supl
               ).

          SELECT DISTINCT *
           FROM zeho_a_acc AS acc
          INNER JOIN @activities AS data
             ON data~bankcode = acc~bankcode
            AND data~bukrs    = acc~bukrs
          INTO CORRESPONDING FIELDS OF TABLE @t_acc.

            CLEAR t_exp[].
            SELECT exp~*
            FROM zeho_a_exp  AS exp
            INNER JOIN @activities AS data
                     ON data~bankcode = exp~bankcode
                    AND data~bukrs    = exp~bukrs
                   INTO CORRESPONDING FIELDS OF TABLE  @t_exp.


              CLEAR t_actt[].
              SELECT actt~* FROM zeho_a_actt  AS actt
              INNER JOIN @activities AS data
                       ON data~bankcode = actt~bankcode
                      AND data~bukrs    = actt~bukrs
                     INTO CORRESPONDING FIELDS OF TABLE  @t_actt.

                tt_activities = CORRESPONDING #( activities ).

                LOOP AT tt_activities REFERENCE INTO rd_activity   WHERE belnr = ' '.
*                                                         AND cancel_process = ' '.

                  zeho_cl_document_processing=>fill_list(
                    EXPORTING
                      tt_exp   = t_exp
                      tt_acct  = t_actt
                      tt_cust  = t_cust
                      tt_suppl = t_supl
                      tt_account   = t_acc
                    CHANGING
                      rd_aa    = rd_activity
                  ).

                ENDLOOP.

                tt_aa_cust = VALUE #( FOR ls_wa IN  tt_activities
                                     WHERE ( belnr = ' ' )
                                      ( CORRESPONDING #( ls_wa MAPPING client = DEFAULT sy-mandt )  )
                                    ).
                lt_log = VALUE #( FOR ls_wa IN  tt_activities
                                     WHERE ( belnr = ' ' )
                                      ( CORRESPONDING #( ls_wa MAPPING client = DEFAULT sy-mandt )  )
                                    ).

                MODIFY zeho_a_actcust FROM TABLE @tt_aa_cust.
                MODIFY zeho_a_log FROM TABLE @lt_log.

                DATA(lo_buffer) = lcl_buffer=>get_instance( ).
                lo_buffer->set_reread(  tt_act = tt_activities ).


                READ ENTITIES OF zeho_i_activities IN LOCAL MODE
            ENTITY activities
            ALL FIELDS WITH
            CORRESPONDING #( keys )
            RESULT DATA(activities_updated)
            FAILED failed.

                result = VALUE #( FOR activit_updated IN activities_updated
                (
                                         bankcode = activit_updated-bankcode
                                         bukrs    = activit_updated-bukrs
                                         iban     = activit_updated-iban
                                         branch   = activit_updated-branch
                                         acccount_no = activit_updated-acccount_no
                                         act_date = activit_updated-act_date
                                         act_time = activit_updated-act_time
                                         act_no   = activit_updated-act_no
                                         %param   = activit_updated

                                          )
                                                        ).

              ENDMETHOD.

              METHOD saveactivityself.

                DATA:
                  lt_je_validate TYPE TABLE FOR FUNCTION IMPORT i_journalentrytp~validate,
                  ls_je_validate TYPE STRUCTURE FOR FUNCTION IMPORT i_journalentrytp~validate,
                  lt_je_deep     TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
                  lv_cid         TYPE abp_behv_cid,
                  ls_je          TYPE STRUCTURE FOR ACTION IMPORT  i_journalentrytp~post.
*          lt_gl      TYPE TABLE FOR ACTION IMPORT  i_journalentrytp~Post-
*          param      TYPE STRUCTURE FOR HIERARCHY d_journalentrypostparameter.
                DATA : cl_document TYPE REF TO zeho_cl_document_processing.
                DATA tt_bapidata TYPE  zeho_activity_if~tty_bapi_data.


                READ ENTITIES OF zeho_i_activities IN LOCAL MODE
                ENTITY activities
                ALL FIELDS WITH
                CORRESPONDING #( keys )
                RESULT DATA(activities)
                FAILED failed.


                DATA(lo_buffer) = lcl_buffer=>get_instance( ).
                DATA tt_act TYPE  zeho_tt_activities.
                tt_act = CORRESPONDING #(  activities  ).

                """"""""""""""""""""""""""""""""""""""""""""""""""""""
*         DATA: lt_je_deep TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
*          lv_cid     TYPE abp_behv_cid,
*          ls_je      TYPE STRUCTURE FOR ACTION IMPORT i_journalentrytp~post.
                CLEAR:  lt_je_deep , ls_je , tt_bapidata.


*    DATA : cl_document TYPE REF TO zeho_cl_document_processing.


                IF cl_document IS NOT BOUND.
                  CREATE OBJECT cl_document.
                ENDIF.

                DATA ls_aa TYPE zeho_i_activities.

                LOOP AT tt_act ASSIGNING FIELD-SYMBOL(<fs_aa>).
                  CLEAR  : lt_je_deep , lt_je_validate , ls_je , ls_je_validate.
                  TRY.
                      lv_cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
                    CATCH cx_uuid_error.
                      ASSERT 1 = 0.
                  ENDTRY.
                  ls_je-%cid = lv_cid.

                  ls_aa = CORRESPONDING #( <fs_aa> ).

                  cl_document->fill_header(
                    EXPORTING
                      rd_aa = ls_aa
                    CHANGING
                      e_je  = ls_je
                  ).

                  cl_document->fill_gl_account(
                    EXPORTING

                      rd_aa     = ls_aa
                      e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
                                  THEN 'S' ELSE 'H'  )
                      e_hkont   = <fs_aa>-hkont
                    CHANGING
                      tt_glitems = ls_je-%param-_glitems
                  ).

                  IF <fs_aa>-secondgl_acc IS NOT INITIAL.


                    cl_document->fill_secondgl_account(
                    EXPORTING
                      rd_aa     = ls_aa
                      e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
                                  THEN 'S' ELSE 'H'  )
                      e_hkont   = <fs_aa>-secondgl_acc
                    CHANGING
                      tt_glitems = ls_je-%param-_glitems
                        ).
                  ELSEIF <fs_aa>-lifnr IS NOT INITIAL.

                    cl_document->fill_supplier(
                      EXPORTING
                        rd_aa      = ls_aa
                        e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
                                  THEN 'S' ELSE 'H'  )
                      CHANGING
                        tt_apitems = ls_je-%param-_apitems
                    ).
                  ELSEIF <fs_aa>-kunnr IS NOT INITIAL.
                    cl_document->fill_customer(
                      EXPORTING
                        rd_aa      = ls_aa
                        e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
                                     THEN 'S' ELSE 'H'  )
                      CHANGING
                        tt_aritems =  ls_je-%param-_aritems
                    ).
                  ENDIF.

                  APPEND ls_je TO lt_je_deep.

                  lt_je_validate = CORRESPONDING #( lt_je_deep ).

                  READ ENTITIES OF i_journalentrytp
                  ENTITY journalentry
                  EXECUTE validate FROM lt_je_validate
                  RESULT DATA(lt_check_result)
                  FAILED DATA(ls_failed_deep)
                  REPORTED DATA(ls_reported_deep).


                  IF ls_failed_deep IS NOT INITIAL.
*        APPEND INITIAL LINE TO tt_keys ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
*        <fs_temp_key>-act = CORRESPONDING #( ls_aa ).
*        <fs_temp_key>-cid = lv_cid.
*        <fs_temp_key>-pid = VALUE #( ls_mapped_deep-journalentry[ %cid = lv_cid ]-%pid OPTIONAL ).
                    LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
                      DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).

                      APPEND VALUE #( bankcode = ls_aa-bankcode
                                      bukrs    = ls_aa-bukrs
                                      iban     = ls_aa-iban
                                      branch   = ls_aa-branch
                                      act_no   = ls_aa-act_no
                                      act_date = ls_aa-act_date
                                      act_time = ls_aa-act_time
                                      acccount_no = ls_aa-acccount_no
                                      %msg = <ls_reported_deep>-%msg ) TO reported-activities.

*          APPEND VALUE #(
*
*                 bankcode = ls_aa-bankcode
*                 bukrs    = ls_aa-bukrs
*                 iban     = ls_aa-iban
*                 branch   = ls_aa-branch
*                 act_no   = ls_aa-act_no
*                 act_date = ls_aa-act_date
*                 act_time = ls_aa-act_time
*                 acccount_no = ls_aa-acccount_no
*
*              ) TO failed-activities.

                    ENDLOOP.
                  ELSE.

                    APPEND VALUE #(

                       bankcode = ls_aa-bankcode
                       bukrs    = ls_aa-bukrs
                       iban     = ls_aa-iban
                       branch   = ls_aa-branch
                       act_no   = ls_aa-act_no
                       act_date = ls_aa-act_date
                       act_time = ls_aa-act_time
                       acccount_no = ls_aa-acccount_no
                       bapi_data   = CORRESPONDING #( lt_je_deep )
                    ) TO tt_bapidata.

**        APPEND INITIAL LINE TO tt_keys  ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
**        <fs_temp_key>-act = CORRESPONDING #( ls_aa ).
**        <fs_temp_key>-cid = lv_cid.
**        <fs_temp_key>-pid = VALUE #( ls_mapped_deep-journalentry[ %cid = lv_cid ]-%pid OPTIONAL ).

                  ENDIF.
                ENDLOOP.

*    tt_act = CORRESPONDING #(  activities  ).
                CLEAR tt_act.
                LOOP AT activities ASSIGNING FIELD-SYMBOL(<fs_act>).

                  IF NOT line_exists( reported-activities[

                      bankcode = <fs_act>-bankcode
                      bukrs    = <fs_act>-bukrs
                      iban     = <fs_act>-iban
                      branch   = <fs_act>-branch
                      act_no   = <fs_act>-act_no
                      act_date = <fs_act>-act_date
                      act_time = <fs_act>-act_time
                      acccount_no = <fs_act>-acccount_no
                  ] ).
                    tt_act = VALUE #(  BASE tt_act

                            ( CORRESPONDING #( <fs_act> ) )
                    ).
                  ENDIF.
                ENDLOOP.
                """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

*        CLEAR tt_act
*        tt_act = CORRESPONDING #(  activities  ).
                lo_buffer->set_post( tt_act ).


                lo_buffer->set_reread(  tt_act = tt_act ).
                lo_buffer->set_bapi_data( tt_bapidata ).
                result = VALUE #( FOR activity IN activities
             (
                                      bankcode = activity-bankcode
                                      bukrs    = activity-bukrs
                                      iban     = activity-iban
                                      branch   = activity-branch
                                      acccount_no = activity-acccount_no
                                      act_date = activity-act_date
                                      act_time = activity-act_time
                                      act_no   = activity-act_no
                                      %param   = activity

                                       )
                                                     ).

              ENDMETHOD.

              METHOD rba_mult.

                SELECT *
            FROM zeho_a_aa_mult
            FOR ALL ENTRIES IN @keys_rba
            WHERE  bankcode    =  @keys_rba-bankcode
              AND  bukrs       =  @keys_rba-bukrs
              AND  iban        =  @keys_rba-iban
              AND  branch      =  @keys_rba-branch
              AND  acccount_no =  @keys_rba-acccount_no
              AND  act_date    =  @keys_rba-act_date
              AND  act_time    =  @keys_rba-act_time
              AND  act_no      =  @keys_rba-act_no
*      AND item_no      =  @keys_rba-item_no
              INTO TABLE @DATA(lt_mult).

                  result = CORRESPONDING #( lt_mult ).


                ENDMETHOD.

                METHOD cba_mult.

                  DATA lt_target TYPE TABLE FOR CREATE zeho_i_activities\\_mult.
                  DATA ls_target LIKE LINE OF lt_target.
                  DATA lt_aa_multi TYPE TABLE OF zeho_a_aa_mult.
                  DATA : lv_itemno TYPE zeho_a_aa_mult-item_no.

                  DATA : lt_reported_buffer TYPE zeho_activity_if~tty_mult_reported.
                  DATA(lo_buffer) = lcl_buffer=>get_instance( ).
                  lo_buffer->get_mult_reported( IMPORTING tt_reported =  lt_reported_buffer ).

                  IF lt_reported_buffer IS INITIAL .

                    CLEAR lt_aa_multi.
                    LOOP AT entities_cba ASSIGNING FIELD-SYMBOL(<fs_multi>).
                      CLEAR lt_target.

                      lt_target = <fs_multi>-%target.
                      ls_target = lt_target[ 1 ].

                      IF ls_target-amount IS INITIAL .
                        APPEND VALUE #(

                      bankcode = ls_target-bankcode
                      bukrs    = ls_target-bukrs
                      iban     = ls_target-iban
                      branch   = ls_target-branch
                      act_no   = ls_target-act_no
                      act_date = ls_target-act_date
                      act_time = ls_target-act_time
                      acccount_no = ls_target-acccount_no
                      %msg =  NEW zeho_cl_messages(
                                 textid   = zeho_cl_messages=>amount_cannot_be_null
                                 severity = if_abap_behv_message=>severity-error
                        )
                  ) TO reported-_mult.
                      ENDIF.

                      IF ls_target-secondgl_acc IS INITIAL AND
                         ls_target-lifnr IS INITIAL AND
                         ls_target-kunnr IS INITIAL.


                        APPEND VALUE #(

                              bankcode = ls_target-bankcode
                              bukrs    = ls_target-bukrs
                              iban     = ls_target-iban
                              branch   = ls_target-branch
                              act_no   = ls_target-act_no
                              act_date = ls_target-act_date
                              act_time = ls_target-act_time
                              acccount_no = ls_target-acccount_no
                              %msg =  NEW zeho_cl_messages(
                                         textid   = zeho_cl_messages=>fill_account
                                         severity = if_abap_behv_message=>severity-error
                                )
                          ) TO reported-_mult.

                      ENDIF.


                    ENDLOOP.
                    IF reported-_mult IS INITIAL.


                      lt_aa_multi = VALUE #( FOR mult IN  entities_cba
                                             FOR target IN mult-%target
                                      (  CORRESPONDING #( target MAPPING client = DEFAULT sy-mandt )  )
                          ).
                      LOOP AT lt_aa_multi ASSIGNING FIELD-SYMBOL(<fs_mul>) WHERE item_no = '00'.
                        CLEAR lv_itemno.
                        SELECT MAX( item_no )
                        FROM zeho_a_aa_mult
                        WHERE   bankcode = @<fs_mul>-bankcode
                          AND   bukrs    = @<fs_mul>-bukrs
                          AND   iban     = @<fs_mul>-iban
                          AND   branch   = @<fs_mul>-branch
                          AND   act_no   = @<fs_mul>-act_no
                          AND   act_date = @<fs_mul>-act_date
                          AND   act_time = @<fs_mul>-act_time
                          AND   acccount_no = @<fs_mul>-acccount_no
                          INTO @lv_itemno.
                          IF sy-subrc = 0.
                            <fs_mul>-item_no = lv_itemno + 1.
                          ELSE.
                            <fs_mul>-item_no = 1.
                          ENDIF.


                        ENDLOOP.

                        IF lt_aa_multi IS NOT INITIAL.

                          MODIFY zeho_a_aa_mult FROM TABLE @lt_aa_multi.
                          APPEND VALUE #(

                            bankcode = ls_target-bankcode
                            bukrs    = ls_target-bukrs
                            iban     = ls_target-iban
                            branch   = ls_target-branch
                            act_no   = ls_target-act_no
                            act_date = ls_target-act_date
                            act_time = ls_target-act_time
                            acccount_no = ls_target-acccount_no
                            %msg =  NEW zeho_cl_messages(
                                       textid   = zeho_cl_messages=>new_item_saved_succesfully
                                       severity = if_abap_behv_message=>severity-success
                              )
                        ) TO reported-_mult.

                          mapped-activities = CORRESPONDING #( lt_aa_multi ).

                        ENDIF.

                      ENDIF.

                    ENDIF.
                    lo_buffer->clear_mult_reported(  ).

                  ENDMETHOD.

                  METHOD multiplesaveself.

                    DATA:
                      lt_je_validate TYPE TABLE FOR FUNCTION IMPORT i_journalentrytp~validate,
                      ls_je_validate TYPE STRUCTURE FOR FUNCTION IMPORT i_journalentrytp~validate,
                      lt_je_deep     TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
                      lv_cid         TYPE abp_behv_cid,
                      ls_je          TYPE STRUCTURE FOR ACTION IMPORT  i_journalentrytp~post.
                    DATA : cl_document TYPE REF TO zeho_cl_document_processing.
                    DATA tt_bapidata TYPE  zeho_activity_if~tty_bapi_data.

                    READ ENTITIES OF zeho_i_activities IN LOCAL MODE
                     ENTITY activities
                     ALL FIELDS
                     WITH CORRESPONDING #( keys )
                     RESULT FINAL(activities)
                     ENTITY activities
                     BY \_mult
                     ALL FIELDS WITH CORRESPONDING #( keys )
                     RESULT FINAL(activities_mult).

                    DATA : lt_reported TYPE TABLE FOR REPORTED zeho_i_activities\\activities.

                    CLEAR lt_reported.

                    DATA(lo_buffer) = lcl_buffer=>get_instance( ).

                    lo_buffer->check_multiple_post(
                      EXPORTING
                        tt_act    = activities
                        tt_multi  = activities_mult
                      IMPORTING
                        tt_report = lt_reported
                    ).

                    IF lt_reported IS NOT INITIAL.
                      reported-activities = CORRESPONDING #( lt_reported ).
                    ENDIF.
                    CHECK lt_reported IS INITIAL.


                    DATA tt_act TYPE  zeho_tt_activities.
                    tt_act = CORRESPONDING #(  activities  ).

                    CLEAR:  lt_je_deep , ls_je , tt_bapidata.

                    IF cl_document IS NOT BOUND.
                      CREATE OBJECT cl_document.
                    ENDIF.

                    DATA ls_aa TYPE zeho_i_activities.

                    LOOP AT tt_act ASSIGNING FIELD-SYMBOL(<fs_aa>).


                      CLEAR  : lt_je_deep , lt_je_validate , ls_je , ls_je_validate.
                      TRY.
                          lv_cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
                        CATCH cx_uuid_error.
                          ASSERT 1 = 0.
                      ENDTRY.
                      ls_je-%cid = lv_cid.



                      IF  <fs_aa>-blart IS INITIAL.

                        LOOP AT activities_mult ASSIGNING FIELD-SYMBOL(<fs_mult>) WHERE bankcode = <fs_aa>-bankcode
                                                                                    AND bukrs    = <fs_aa>-bukrs
                                                                                    AND iban     = <fs_aa>-iban
                                                                                    AND branch   = <fs_aa>-branch
                                                                                    AND act_no   = <fs_aa>-act_no
                                                                                    AND act_date = <fs_aa>-act_date
                                                                                    AND act_time = <fs_aa>-act_time
                                                                                    AND blart    <> ' '.

                          <fs_aa>-blart = <fs_mult>-blart.
                          EXIT.

                        ENDLOOP.
                      ENDIF.

                      ls_aa = CORRESPONDING #( <fs_aa> ).

                      cl_document->fill_header(
                        EXPORTING
                          rd_aa = ls_aa
                        CHANGING
                          e_je  = ls_je
                      ).


                      LOOP AT activities_mult ASSIGNING <fs_mult> WHERE bankcode = <fs_aa>-bankcode
                                                                    AND bukrs    = <fs_aa>-bukrs
                                                                    AND iban     = <fs_aa>-iban
                                                                    AND branch   = <fs_aa>-branch
                                                                    AND act_no   = <fs_aa>-act_no
                                                                    AND act_date = <fs_aa>-act_date
                                                                    AND act_time = <fs_aa>-act_time.

                        ls_aa = CORRESPONDING #( <fs_mult> ).

                        IF <fs_mult>-secondgl_acc IS NOT INITIAL.


                          cl_document->fill_secondgl_account(
                          EXPORTING
                            rd_aa     = ls_aa
                            e_dc      = <fs_mult>-shkzg
                            e_hkont   = <fs_mult>-secondgl_acc
                          CHANGING
                            tt_glitems = ls_je-%param-_glitems
                              ).
                        ELSEIF <fs_mult>-lifnr IS NOT INITIAL.

                          cl_document->fill_supplier(
                            EXPORTING
                              rd_aa      = ls_aa
                              e_dc      = <fs_mult>-shkzg
                            CHANGING
                              tt_apitems = ls_je-%param-_apitems
                          ).
                        ELSEIF <fs_mult>-kunnr IS NOT INITIAL.
                          cl_document->fill_customer(
                            EXPORTING
                              rd_aa      = ls_aa
                              e_dc      = <fs_mult>-shkzg
                            CHANGING
                              tt_aritems =  ls_je-%param-_aritems
                          ).
                        ENDIF.

                      ENDLOOP.
                      APPEND ls_je TO lt_je_deep.

                      lt_je_validate = CORRESPONDING #( lt_je_deep ).

                      READ ENTITIES OF i_journalentrytp
                      ENTITY journalentry
                      EXECUTE validate FROM lt_je_validate
                      RESULT DATA(lt_check_result)
                      FAILED DATA(ls_failed_deep)
                      REPORTED DATA(ls_reported_deep).


                      IF ls_failed_deep IS NOT INITIAL.

                        LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
                          DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).

                          APPEND VALUE #( bankcode = <fs_aa>-bankcode
                                          bukrs    = <fs_aa>-bukrs
                                          iban     = <fs_aa>-iban
                                          branch   = <fs_aa>-branch
                                          act_no   = <fs_aa>-act_no
                                          act_date = <fs_aa>-act_date
                                          act_time = <fs_aa>-act_time
                                          acccount_no = <fs_aa>-acccount_no
                                          %msg = <ls_reported_deep>-%msg ) TO reported-activities.

                        ENDLOOP.
                      ELSE.

                        APPEND VALUE #(

                           bankcode = <fs_aa>-bankcode
                           bukrs    = <fs_aa>-bukrs
                           iban     = <fs_aa>-iban
                           branch   = <fs_aa>-branch
                           act_no   = <fs_aa>-act_no
                           act_date = <fs_aa>-act_date
                           act_time = <fs_aa>-act_time
                           acccount_no = <fs_aa>-acccount_no
                           bapi_data   = CORRESPONDING #( lt_je_deep )
                        ) TO tt_bapidata.


                      ENDIF.

                    ENDLOOP.

                    """""""""""""""""""""""""""""""""""""""""""""""""""""

                    CLEAR tt_act.
                    LOOP AT activities ASSIGNING FIELD-SYMBOL(<fs_act>).

                      IF NOT line_exists( reported-activities[

                          bankcode = <fs_act>-bankcode
                          bukrs    = <fs_act>-bukrs
                          iban     = <fs_act>-iban
                          branch   = <fs_act>-branch
                          act_no   = <fs_act>-act_no
                          act_date = <fs_act>-act_date
                          act_time = <fs_act>-act_time
                          acccount_no = <fs_act>-acccount_no
                      ] ).
                        tt_act = VALUE #(  BASE tt_act

                                ( CORRESPONDING #( <fs_act> ) )
                        ).

                      ENDIF.

                    ENDLOOP.

*    lo_buffer->set_post( tt_act ).
*    lo_buffer->set_bapi_data( tt_bapidata ).
*
*    mapped-activities = CORRESPONDING #( activities ).

                    lo_buffer->set_post( tt_act ).


                    lo_buffer->set_reread(  tt_act = tt_act ).
                    lo_buffer->set_bapi_data( tt_bapidata ).
                    result = VALUE #( FOR activity IN activities
                 (
                                          bankcode = activity-bankcode
                                          bukrs    = activity-bukrs
                                          iban     = activity-iban
                                          branch   = activity-branch
                                          acccount_no = activity-acccount_no
                                          act_date = activity-act_date
                                          act_time = activity-act_time
                                          act_no   = activity-act_no
                                          %param   = activity

                                           )
                                                         ).


                  ENDMETHOD.

ENDCLASS.

CLASS lsc_zeho_i_activities DEFINITION INHERITING FROM cl_abap_behavior_saver.
PUBLIC SECTION.
  INTERFACES zeho_activity_if.

  DATA instance TYPE REF TO zeho_badi_activity.
  DATA badi_imp_exists TYPE REF TO zeho_badi_activity.
  DATA imp_class_tab  TYPE STANDARD TABLE OF REF TO zif_ex_eho_activity_imp .
  DATA imp_class      TYPE REF TO zif_ex_eho_activity_imp.


  TYPES : tty_bapi_data TYPE TABLE OF zeho_activity_if~ty_bapi_data.


  DATA : tt_act_key TYPE zeho_activity_if~tty_act_with_key.


PROTECTED SECTION.

  METHODS finalize REDEFINITION.

  METHODS check_before_save REDEFINITION.

  METHODS save REDEFINITION.

  METHODS cleanup REDEFINITION.

  METHODS cleanup_finalize REDEFINITION.
  METHODS adjust_numbers REDEFINITION.

*    METHODS adjust_numbers REDEFINITION.

ENDCLASS.

CLASS lsc_zeho_i_activities IMPLEMENTATION.

METHOD finalize.
  DATA lt_temp_key TYPE zeho_activity_if~tty_act_with_key.
  DATA : tt_act TYPE zeho_tt_activities.
  DATA(lo_buffer) = lcl_buffer=>get_instance( ).
  DATA lt_failed TYPE TABLE FOR FAILED zeho_i_activities.
  DATA lt_reported TYPE TABLE FOR REPORTED zeho_i_activities.
  DATA lt_bapidata TYPE tty_bapi_data.


  lo_buffer->get_post( IMPORTING tt_act = tt_act ).

  DATA: lt_je_deep TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
        lv_cid     TYPE abp_behv_cid,
        ls_je      TYPE STRUCTURE FOR ACTION IMPORT i_journalentrytp~post.
*          lt_gl      TYPE TABLE FOR ACTION IMPORT  i_journalentrytp~Post-
*          param      TYPE STRUCTURE FOR HIERARCHY d_journalentrypostparameter.
  DATA : cl_document TYPE REF TO zeho_cl_document_processing.



  IF tt_act IS NOT INITIAL.
    lo_buffer->get_bapi_data( IMPORTING tt_bapi_data = lt_bapidata  ).
    IF lt_bapidata IS NOT INITIAL.
      CLEAR lt_temp_key.
      lo_buffer->post_doc( EXPORTING tt_act = tt_act
                                     tt_bapidata = lt_bapidata
                    IMPORTING tt_keys = lt_temp_key
                              tt_failed = lt_failed
                              tt_reported = lt_reported ).
      lo_buffer->set_keys(  tt_keys = lt_temp_key ).
      reported-activities =  CORRESPONDING #( lt_reported ).
      CLEAR:  lt_je_deep , ls_je.
    ENDIF.

  ENDIF.



ENDMETHOD.

METHOD check_before_save.
ENDMETHOD.

METHOD save.
  TYPES: BEGIN OF ty_temp_key,
           cid TYPE abp_behv_cid,
           pid TYPE abp_behv_pid,
         END OF ty_temp_key,
         tt_temp_key TYPE STANDARD TABLE OF ty_temp_key WITH DEFAULT KEY.
  DATA : lt_log TYPE TABLE OF zeho_a_log.
  DATA:     ltt_temp_key TYPE zeho_activity_if~tty_act_with_key.
  DATA:     lt_reverse_key TYPE zeho_activity_if~tty_act_with_key.
  DATA : ls_temp_key LIKE LINE OF ltt_temp_key.
*    DATA : lt_cid TYPE TABLE OF zeho_a_doc_cid.

  CLEAR  : lt_log, ltt_temp_key , lt_reverse_key.
  DATA(lo_buffer) = lcl_buffer=>get_instance( ).
  lo_buffer->get_keys(  IMPORTING tt_keys = ltt_temp_key  ).
  lo_buffer->get_reverse( IMPORTING tt_act = lt_reverse_key ).

  IF ltt_temp_key IS NOT INITIAL.

    LOOP AT ltt_temp_key ASSIGNING FIELD-SYMBOL(<fs_key>).
      CONVERT KEY OF i_journalentrytp
    FROM <fs_key>-pid
    TO FINAL(lv_root_key).
      <fs_key>-act-belnr = lv_root_key-accountingdocument.
      <fs_key>-act-bukrs = lv_root_key-companycode.
      <fs_key>-act-gjahr = lv_root_key-fiscalyear.


**        reported-activities = VALUE #( (
**                            bankcode = <fs_key>-act-bankcode
**                            bukrs    = <fs_key>-act-bukrs
**                            iban     = <fs_key>-act-iban
**                            branch   = <fs_key>-act-branch
**                            act_no   = <fs_key>-act-act_no
**                            act_date = <fs_key>-act-act_date
**                            act_time = <fs_key>-act-act_time
**                            acccount_no = <fs_key>-act-acccount_no
**
**                           %msg  = new_message_with_text(
**                                     severity = if_abap_behv_message=>severity-success
**                                     text     =  'Belge olusturuldu'
**                                   ) ) ) .




      APPEND INITIAL LINE TO reported-activities ASSIGNING FIELD-SYMBOL(<fs_reported>).
      <fs_reported> = CORRESPONDING #( <fs_key>-act ).
      <fs_reported>-%tky = CORRESPONDING #( <fs_key>-act ).
*            <fs_reported>-%state_area = 'SAVE'.
      <fs_reported>-%msg =  NEW zeho_cl_messages(
                       textid   = zeho_cl_messages=>document_created
                       severity = if_abap_behv_message=>severity-success
                       mv_bukrs  = <fs_key>-act-bukrs
                       mv_gjahr  = <fs_key>-act-gjahr
                       mv_belnr  = <fs_key>-act-belnr ).


    ENDLOOP.

    lt_log = VALUE #( FOR key IN ltt_temp_key
               ( CORRESPONDING #( key-act MAPPING client = DEFAULT sy-mandt )
               )
         ).

*       lt_cid = VALUE #( FOR key IN ltt_temp_key
*                 ( belnr = key-act-belnr
*                   bukrs = key-act-bukrs
*                   gjahr = key-act-gjahr
*                   client = sy-mandt
*                 )
*
*           ).


    MODIFY zeho_a_log FROM TABLE @lt_log.

    imp_class_tab = zeho_badi_activity=>get_instance(  )->imps.
    imp_class = VALUE #( imp_class_tab[ 1 ]  OPTIONAL ).
    IF imp_class IS BOUND.
      imp_class->after_modify_je( tt_log = lt_log ).
    ENDIF.

*      MODIFY zeho_a_doc_cid FROM TABLe @lt_cid.

  ENDIF.

  IF  lt_reverse_key IS NOT INITIAL.

    LOOP AT lt_reverse_key ASSIGNING <fs_key>.
      CONVERT KEY OF i_journalentrytp FROM <fs_key>-pid TO DATA(lv_key).



      APPEND INITIAL LINE TO reported-activities ASSIGNING <fs_reported>.
      <fs_reported> = CORRESPONDING #( <fs_key>-act ).
      <fs_reported>-%tky = CORRESPONDING #( <fs_key>-act ).
      <fs_reported>-%msg =  NEW zeho_cl_messages(
                       textid   = zeho_cl_messages=>document_created
                       severity = if_abap_behv_message=>severity-success
                       mv_bukrs  = lv_key-companycode
                       mv_gjahr  = lv_key-fiscalyear
                       mv_belnr  = lv_key-accountingdocument ).



    ENDLOOP.

    lt_log = VALUE #( FOR key IN lt_reverse_key
        ( CORRESPONDING #( key-act MAPPING client = DEFAULT sy-mandt EXCEPT belnr gjahr )
        )
      ).
    MODIFY zeho_a_log FROM TABLE @lt_log.

  ENDIF.



ENDMETHOD.

METHOD cleanup.
  DATA(lo_buffer) = lcl_buffer=>get_instance( ).
  lo_buffer->clear_cache( ).
ENDMETHOD.

METHOD cleanup_finalize.
ENDMETHOD.


METHOD adjust_numbers.
ENDMETHOD.

ENDCLASS.






CLASS lhc__mult DEFINITION INHERITING FROM cl_abap_behavior_handler.

PUBLIC SECTION.
  INTERFACES zeho_activity_if.



PRIVATE SECTION.

  METHODS create FOR MODIFY
    IMPORTING entities FOR CREATE _mult.

  METHODS update FOR MODIFY
    IMPORTING entities FOR UPDATE _mult.

  METHODS delete FOR MODIFY
    IMPORTING keys FOR DELETE _mult.

  METHODS read FOR READ
    IMPORTING keys FOR READ _mult RESULT result.

  METHODS rba_activities FOR READ
    IMPORTING keys_rba FOR READ _mult\activities FULL result_requested RESULT result LINK association_links.
  METHODS defaultforcreate FOR READ
    IMPORTING keys FOR FUNCTION _mult~defaultforcreate RESULT result.
  METHODS get_global_features FOR GLOBAL FEATURES
    IMPORTING REQUEST requested_features FOR _mult RESULT result.
  METHODS precheck_create FOR PRECHECK
    IMPORTING entities FOR CREATE _mult.
  METHODS precheck_update FOR PRECHECK
    IMPORTING entities FOR UPDATE _mult.

  METHODS precheck_delete FOR PRECHECK
    IMPORTING keys FOR DELETE _mult.

ENDCLASS.

CLASS lhc__mult IMPLEMENTATION.



METHOD create.

  IF 1 = 2.
  ENDIF.

ENDMETHOD.

METHOD update.
  DATA lt_log TYPE TABLE OF zeho_a_aa_mult.
  DATA lt_mult TYPE zeho_activity_if~tt_activity_multi.
  DATA lt_reported TYPE zeho_activity_if~tty_mult_reported.
  DATA : lt_reported_buffer TYPE zeho_activity_if~tty_mult_reported.


  DATA(lo_buffer) = lcl_buffer=>get_instance( ).
  lo_buffer->get_mult_reported( IMPORTING tt_reported =  lt_reported_buffer ).
*    DATA lt_entities FOR UPDATE
*    DELETE entities WHERE item_no is initial.
  IF lt_reported_buffer IS INITIAL .
    lo_buffer->change_updated_fields(
      EXPORTING
        tt_upd_entities = entities
      IMPORTING
        tt_mult         = lt_mult
        tt_mult_reported = lt_reported
    ).

    IF lt_reported IS INITIAL.
      CLEAR lt_log.
      lt_log = VALUE #( FOR entity IN lt_mult
                         WHERE ( item_no IS NOT INITIAL )
                          ( CORRESPONDING #( entity MAPPING client = DEFAULT sy-mandt ) )
                           ).

      IF lt_log IS NOT INITIAL.
        MODIFY zeho_a_aa_mult FROM TABLE @lt_log.
      ENDIF.
    ELSE.
      reported-_mult = CORRESPONDING #( lt_reported ).
    ENDIF.
  ENDIF.
  lo_buffer->clear_mult_reported(  ).

ENDMETHOD.

METHOD delete.
  DATA lt_multi_temp TYPE TABLE OF zeho_a_aa_mult.
  DATA : lt_reported_buffer TYPE zeho_activity_if~tty_mult_reported.
  DATA(lo_buffer) = lcl_buffer=>get_instance( ).
  lo_buffer->get_mult_reported( IMPORTING tt_reported =  lt_reported_buffer ).

  IF lt_reported_buffer IS INITIAL .

    READ ENTITIES OF zeho_i_activities IN LOCAL MODE
    ENTITY _mult
    ALL FIELDS WITH CORRESPONDING #(  keys )
   RESULT DATA(lt_multi).


    lt_multi_temp = CORRESPONDING #( lt_multi MAPPING client = DEFAULT sy-mandt ).

    DELETE   zeho_a_aa_mult FROM TABLE @lt_multi_temp .

  ENDIF.

  lo_buffer->clear_mult_reported(  ).

ENDMETHOD.

METHOD read.

  SELECT *
  FROM zeho_a_aa_mult
  FOR ALL ENTRIES IN @keys
  WHERE  bankcode    =  @keys-bankcode
    AND  bukrs       =  @keys-bukrs
    AND  iban        =  @keys-iban
    AND  branch      =  @keys-branch
    AND  acccount_no =  @keys-acccount_no
    AND  act_date    =  @keys-act_date
    AND  act_time    =  @keys-act_time
    AND  act_no      =  @keys-act_no
    AND item_no      =  @keys-item_no
    INTO TABLE @DATA(lt_mult).

    result = CORRESPONDING #( lt_mult ).




  ENDMETHOD.

  METHOD rba_activities.

    IF 1 = 2.

    ENDIF.

  ENDMETHOD.

  METHOD defaultforcreate.
    DATA : lt_default TYPE TABLE FOR READ RESULT zeho_i_activities\\_mult.

    READ ENTITIES OF zeho_i_activities IN LOCAL MODE
    ENTITY activities
    ALL FIELDS WITH CORRESPONDING #(  keys )
  RESULT DATA(lt_activities).

    APPEND INITIAL LINE TO lt_default ASSIGNING FIELD-SYMBOL(<fs_default>).
*    <fs_default>-%data
*    result-
    lt_default = VALUE #( FOR act IN lt_activities

               ( bankcode    = act-bankcode
                 bukrs       = act-bukrs
                 iban        = act-iban
                 branch      = act-branch
                 acccount_no = act-acccount_no
                 act_date    = act-act_date
                 act_time    = act-act_time
                 act_no      = act-act_no
*                 item_no     = act-item_no
                 waers       = act-waers
                    )
                    ).
    result = CORRESPONDING #( lt_default ).

  ENDMETHOD.

  METHOD get_global_features.

  ENDMETHOD.

  METHOD precheck_create.

    DATA : tt_mult  TYPE zeho_activity_if~tt_activity_multi.
    DATA : tt_reported TYPE zeho_activity_if~tty_mult_reported.
    DATA : lt_reported TYPE zeho_activity_if~tty_mult_reported.
    DATA(lo_buffer) = lcl_buffer=>get_instance( ).

    CLEAR : tt_mult , tt_reported.
    tt_mult = CORRESPONDING #( entities ) .

    lo_buffer->check_mult_fields(
      EXPORTING
        tt_mult   = tt_mult
        i_action  = zeho_activity_if~c_act_create
      IMPORTING
        tt_report = tt_reported
    ).

*    reported-_mult =  CORRESPONDING #( tt_reported ) .
    reported-_mult =  CORRESPONDING #( tt_reported ) .
    lt_reported = CORRESPONDING #( reported-_mult ).
    lo_buffer->set_mult_reported( tt_reported = lt_reported ).

  ENDMETHOD.

  METHOD precheck_update.

    DATA : tt_mult  TYPE zeho_activity_if~tt_activity_multi.
    DATA : tt_reported TYPE zeho_activity_if~tty_mult_reported.
    DATA : lt_reported TYPE zeho_activity_if~tty_mult_reported.
    DATA(lo_buffer) = lcl_buffer=>get_instance( ).

    CLEAR : tt_mult , tt_reported.
    tt_mult = CORRESPONDING #( entities ) .

    lo_buffer->check_mult_fields(
      EXPORTING
        tt_mult   = tt_mult
        i_action  = zeho_activity_if~c_act_update
      IMPORTING
        tt_report = tt_reported
    ).

    reported-_mult =  CORRESPONDING #( tt_reported ) .
    lt_reported = CORRESPONDING #( reported-_mult ).
    lo_buffer->set_mult_reported( tt_reported = lt_reported ).


  ENDMETHOD.

  METHOD precheck_delete.

    DATA : tt_mult  TYPE zeho_activity_if~tt_activity_multi.
    DATA : tt_reported TYPE zeho_activity_if~tty_mult_reported.
    DATA : lt_reported TYPE zeho_activity_if~tty_mult_reported.
    DATA(lo_buffer) = lcl_buffer=>get_instance( ).

    CLEAR : tt_mult , tt_reported.
    tt_mult = CORRESPONDING #( keys ) .

    lo_buffer->check_mult_fields(
      EXPORTING
        tt_mult   = tt_mult
        i_action  = zeho_activity_if~c_act_delete
      IMPORTING
        tt_report = tt_reported
    ).

    reported-_mult =  CORRESPONDING #( tt_reported ) .
    lt_reported = CORRESPONDING #( reported-_mult ).
    lo_buffer->set_mult_reported( tt_reported = lt_reported ).




  ENDMETHOD.

ENDCLASS.
