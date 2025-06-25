CLASS zeho_cl_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zeho_service_if .
*    INTERFACES zif_ex_eho_service_imp.
    DATA instance TYPE REF TO zeho_badi_service.

    DATA badi_imp_exists TYPE REF TO zeho_badi_service.
    DATA imp_class_Tab  TYPE STANDARD TABLE OF REF TO zif_ex_eho_service_imp .
    DATA imp_class      TYPE REF TO zif_ex_eho_service_imp.
    METHODS constructor
      IMPORTING
        !bankcode TYPE zeho_tt_bankcode_range OPTIONAL
        !bukrs    TYPE zeho_tt_bukrs_range OPTIONAL
        !account  TYPE zeho_tt_acc_range OPTIONAL
*      !LOG type ref to ZSOA_EHO_CL_APP_LOG optional
        !begdate  TYPE datum
        !enddate  TYPE datum
        !bal_log  TYPE REF TO if_bali_log OPTIONAL
      RAISING
        zeho_cl_messages .


  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_customization
      IMPORTING
        !bankcode TYPE zeho_tt_bankcode_range OPTIONAL
        !bukrs    TYPE zeho_tt_bukrs_range OPTIONAL
        !account  TYPE zeho_tt_acc_range OPTIONAL
      CHANGING
        !t_reqm   TYPE zeho_tt_reqm
        !t_respm  TYPE zeho_tt_respm
        !t_serv   TYPE zeho_tt_serv
        !t_acc    TYPE zeho_tt_acc
        !t_cust   TYPE zeho_tt_customer
        !t_suppl  TYPE zeho_tt_supplier
      RAISING
        zeho_cl_messages.
    METHODS   xml_parser .
    METHODS map_xmltable_to_db
      IMPORTING
        !rd_acc TYPE REF TO zeho_s_acc
      RAISING
        zeho_cl_messages.
    METHODS get_actt
      IMPORTING
        !t_aa   TYPE zeho_tt_aa
      CHANGING
        !t_actt TYPE zeho_tt_actt.

    METHODS get_exp
      IMPORTING
        !t_aa  TYPE zeho_tt_aa
      CHANGING
        !t_exp TYPE zeho_tt_exp.

    METHODS fill_posting_rules_2_db
      IMPORTING
        t_actt TYPE  zeho_tt_actt
        t_exp  TYPE  zeho_tt_exp
        t_cust TYPE zeho_tt_customer
        t_supl TYPE zeho_tt_supplier
        t_acc  TYPE zeho_tt_acc
      CHANGING
        t_aa   TYPE zeho_tt_aa.


ENDCLASS.



CLASS ZEHO_CL_SERVICE IMPLEMENTATION.


  METHOD constructor.
    DATA : lrd_accounts TYPE REF TO zeho_s_acc.
    DATA : dummy TYPE c.
    DATA : lt_account TYPE  zeho_tt_acc.
    CLEAR imp_class_Tab.

    me->get_customization(
      EXPORTING
        bankcode = bankcode
        bukrs    = bukrs
        account  = account
      CHANGING
        t_reqm   = zeho_service_if~t_reqm
        t_respm  = zeho_service_if~t_respm
        t_serv   = zeho_service_if~t_serv
        t_acc    = zeho_service_if~t_accounts
        t_cust   = zeho_service_if~t_cust
        t_suppl  = zeho_service_if~t_suppl
    ).

*    GET BADI me->instance.
    DATA g_exception TYPE REF TO cx_root.
    TRY.
        me->instance =  zeho_badi_service=>get_instance( ).
        imp_class_Tab = zeho_badi_service=>get_instance(  )->imps.
        imp_class = VALUE #( imp_class_Tab[ 1 ]  OPTIONAL ).
      CATCH zeho_cl_messages INTO g_exception.

       """ added by eakyuz on 27.03.2025
        DATA(ls_item_message) =  NEW zeho_cl_messages(
                             textid   = zeho_cl_messages=>item_not_found
                             severity = if_abap_behv_message=>severity-error
                        ).
        DATA lv_message(200).
*        TYPES : BEGIN OF ty_item,
*                  severity  TYPE if_abap_behv_message=>t_severity,
*                  text(200),
*                END OF ty_item.
*        DATA ls_item TYPe REF TO IF_BALI_FREE_TEXT_SETTER.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          INTO lv_message
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*          ls_item-severity = if_abap_behv_message=>severity-error.
        DATA(ls_item_message2) = cl_bali_free_text_setter=>create(
                          severity = if_bali_constants=>c_severity_information
                          text     = lv_message
                        ).
*         CREATE OBJECT ls_item.
        TRY.
            bal_log->add_item( item =  ls_item_message2 ).
          CATCH cx_bali_runtime.
            "handle exception
        ENDTRY.
          """ added by eakyuz on 27.03.2025


        RAISE EXCEPTION TYPE zeho_cl_messages
          EXPORTING
            textid = zeho_cl_messages=>badi_implementation_missing.

    ENDTRY.


    LOOP AT zeho_service_if~t_accounts REFERENCE INTO lrd_accounts.
      IF lrd_accounts->unique_number IS INITIAL.
        LOOP AT zeho_service_if~t_accounts TRANSPORTING NO FIELDS
                                         WHERE bankcode = lrd_accounts->bankcode
                                           AND bukrs = lrd_accounts->bukrs
                                           AND iban <> lrd_accounts->iban
                                           AND unique_number <> lrd_accounts->unique_number.
* TRANSPORTING NO FIELDS
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
          RAISE EXCEPTION TYPE zeho_cl_messages
            EXPORTING
              textid      = zeho_cl_messages=>unique_maintenance_is_wrong
*             previous    = previous
              mv_bankcode = lrd_accounts->bankcode
              mv_bukrs    = lrd_accounts->bukrs.
        ENDIF.
      ELSE.
        LOOP AT zeho_service_if~t_accounts  TRANSPORTING NO FIELDS
                                        WHERE bankcode = lrd_accounts->bankcode
                                          AND bukrs = lrd_accounts->bukrs
                                          AND iban <> lrd_accounts->iban
                                          AND ( unique_number = lrd_accounts->unique_number
                                           OR unique_number IS INITIAL ).
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
          RAISE EXCEPTION TYPE zeho_cl_messages
            EXPORTING
              textid      = zeho_cl_messages=>unique_maintenance_is_wrong
*             previous    = previous
              mv_bankcode = lrd_accounts->bankcode
              mv_bukrs    = lrd_accounts->bukrs.
        ENDIF.
      ENDIF.

    ENDLOOP.


    CLEAR lt_account.
    lt_account[]  = CORRESPONDING #( zeho_service_if~t_accounts ).
    SORT lt_account BY bankcode bukrs unique_number.
    DELETE ADJACENT DUPLICATES FROM lt_account COMPARING bankcode bukrs unique_number.


    LOOP AT lt_account REFERENCE INTO lrd_accounts.

      zeho_service_if~create_req(
      rd_acc     = lrd_accounts
      begdate     = begdate
      enddate     = enddate
*        log         = log
         ).

*      DATA g_exception TYPE REF TO cx_root.
      TRY .

          zeho_service_if~send_req(
            EXPORTING
              begdate = begdate
              enddate = enddate
              rd_acc  = lrd_accounts
               ).
        CATCH zeho_cl_messages INTO g_exception.

          RAISE EXCEPTION TYPE zeho_cl_messages
            EXPORTING
              textid      = zeho_cl_messages=>request_send_error
*             previous    = previous
              mv_bankcode = lrd_accounts->bankcode
              mv_bukrs    = lrd_accounts->bukrs
              mv_iban     = lrd_accounts->iban.



      ENDTRY.

      zeho_service_if~get_res(
       rd_acc =  lrd_accounts

      ).

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_posting_rules_2_db.
    DATA tt_activities TYPE zeho_tt_activities.
    DATA rd_activity TYPE REF TO zeho_s_activity.
    DATA lt_cust TYPE TABLE OF zeho_a_actcust.
    CLEAR lt_cust.
    tt_activities = CORRESPONDING #( t_aa ).

    LOOP AT tt_activities REFERENCE INTO rd_activity WHERE belnr = ' '
                                                   AND cancel_process = ' '.

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

    lt_cust = CORRESPONDING #( tt_activities MAPPING client = DEFAULT sy-mandt ).

    MODIFY zeho_a_actcust FROM TABLE @lt_cust.

  ENDMETHOD.


  METHOD get_actt.

    CLEAR t_actt[].
    SELECT * FROM ZEHO_a_actt  AS actt
    INNER JOIN @t_aa AS data
             ON data~bankcode = actt~bankcode
            AND data~bukrs    = actt~bukrs
           INTO CORRESPONDING FIELDS OF TABLE  @t_actt.

  ENDMETHOD.


  METHOD get_customization.
    DATA lv_where TYPE string.
    DATA : tt_bukrs_range TYPE zeho_tt_bukrs_range.
    SELECT *
     FROM zeho_a_acc
    WHERE bankcode IN @bankcode
    AND bukrs IN @bukrs
    AND acccount_no IN @account
    INTO CORRESPONDING FIELDS OF TABLE @t_acc.

    IF sy-subrc = 0.

      SELECT bankcode           ,
             bukrs              ,
             host               ,
             content_type       ,
             soap_action        ,
             serv_url
       FROM zeho_a_serv
      WHERE bankcode IN @bankcode
      INTO TABLE @t_serv.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zeho_cl_messages
          EXPORTING
            textid = zeho_cl_messages=>service_not_found.

      ENDIF.



      SELECT bankcode ,
             input_tag ,
             input_value
        FROM zeho_a_req_m
      WHERE bankcode IN @bankcode
      INTO TABLE @t_reqm.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zeho_cl_messages
          EXPORTING
            textid = zeho_cl_messages=>request_mapping_not_found.
      ENDIF.

      SELECT bankcode ,
        resp_tag ,
        output_value
        FROM zeho_a_resp_m
       WHERE bankcode IN @bankcode
        INTO TABLE @t_respm.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zeho_cl_messages
          EXPORTING
            textid = zeho_cl_messages=>response_mapping_not_found.
      ENDIF.

      tt_bukrs_range = VALUE #( FOR comp IN t_acc
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
               et_customer = zeho_service_if~t_cust
               et_supplier = zeho_service_if~t_suppl
           ).



*       Implement Exception here
    ENDIF.
  ENDMETHOD.


  METHOD get_exp.
    CLEAR t_exp[].
    SELECT * FROM ZEHO_a_exp  AS exp
    INNER JOIN @t_aa AS data
             ON data~bankcode = exp~bankcode
            AND data~bukrs    = exp~bukrs
           INTO CORRESPONDING FIELDS OF TABLE  @t_exp.

  ENDMETHOD.


  METHOD map_xmltable_to_db.
    DATA lines TYPE i.
    DATA l_tabix TYPE i.
    DATA lt_respm TYPE zeho_tt_respm.
    DATA lrd_accact TYPE REF TO zeho_s_aa.
    CLEAR zeho_service_if~t_aa[].
    FIELD-SYMBOLS : <fs_val> TYPE any.

    IF imp_class IS BOUND.
      imp_class->get_res_processing(
        EXPORTING
          rd_acc    = rd_acc
        CHANGING
          xml_table = zeho_service_if~t_xmltab
          xresponse = zeho_service_if~xml_resx
          t_acc     = zeho_service_if~t_aa
      ).
    ENDIF.
    CLEAR lt_respm.
    CLEAR l_tabix.
    lt_respm = VALUE #( FOR respm IN zeho_service_if~t_respm
                     WHERE ( bankcode = rd_acc->bankcode )
                         ( bankcode = respm-bankcode
                           resp_tag = respm-resp_tag
                           output_value = respm-output_value  )
                                           ).
    IF lt_respm IS INITIAL.
      DATA(lrd_map) = REF #( lt_respm[ 1 ]  OPTIONAL ).
      LOOP AT zeho_service_if~t_xmltab REFERENCE INTO DATA(lrd_xml) WHERE cname EQ lrd_map->resp_tag.
        l_tabix = l_tabix + 1.

      ENDLOOP.
      IF rd_acc->unique_number IS NOT INITIAL AND zeho_service_if~t_aa IS INITIAL.
        APPEND INITIAL LINE TO zeho_service_if~t_aa REFERENCE INTO lrd_accact.
        lrd_accact->bankcode = rd_acc->bankcode.
        lrd_accact->bukrs = rd_acc->bukrs.
        lrd_accact->branch = rd_acc->branch.
        lrd_accact->acccount_no = rd_acc->acccount_no.
        lrd_accact->iban = rd_acc->iban.
        lrd_accact->waers = rd_acc->waers.
        LOOP AT lt_respm REFERENCE INTO  lrd_map.
          lrd_xml = REF #( zeho_service_if~t_xmltab[ cname = lrd_map->resp_tag  ]  OPTIONAL ).
          IF lrd_xml->cname IS NOT INITIAL .
            l_tabix = sy-tabix.
            ASSIGN COMPONENT lrd_map->output_value OF STRUCTURE lrd_accact->* TO <fs_val>.
            IF <fs_val> IS ASSIGNED.
              <fs_val> =  lrd_xml->cvalue.
            ENDIF.
            DELETE zeho_service_if~t_xmltab INDEX l_tabix.
          ENDIF.
        ENDLOOP.

      ELSEIF   rd_acc->unique_number IS NOT INITIAL  AND zeho_service_if~t_aa IS NOT INITIAL.
        IF imp_class IS NOT BOUND.
          imp_class->non_unique_parsing(
          EXPORTING
            rd_acc = rd_acc
            accounts_t = zeho_service_if~t_accounts
          CHANGING
            t_aa =  zeho_service_if~t_aa
          ).
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE zeho_cl_messages
          EXPORTING
            textid      = zeho_cl_messages=>unique_mapping_not_developed
*           previous    = previous
            mv_bankcode = rd_acc->bankcode
            mv_bukrs    = rd_acc->bukrs
            mv_iban     = rd_acc->iban.
      ENDIF.



      DELETE zeho_service_if~t_aa WHERE amount IS INITIAL.

      IF imp_class IS NOT BOUND.
        me->get_actt(
          EXPORTING
         t_aa =  zeho_service_if~t_aa
            CHANGING
        t_actt = zeho_service_if~t_actt  ).

        me->get_exp(
        EXPORTING
          t_aa = zeho_service_if~t_aa
        CHANGING
          t_exp = zeho_service_if~t_exp ).


        me->fill_posting_rules_2_db(
        EXPORTING
        t_actt = zeho_service_if~t_actt[]
        t_exp  = zeho_service_if~t_exp[]
        t_cust = zeho_service_if~t_cust[]
        t_supl = zeho_service_if~t_suppl[]
        t_acc  = zeho_service_if~t_accounts[]
      CHANGING
        t_aa  = zeho_service_if~t_aa ).



        imp_class->final_processing(
       EXPORTING
         t_actt = zeho_service_if~t_actt[]
         t_exp  = zeho_service_if~t_exp[]
         t_cust = zeho_service_if~t_cust[]
         t_supl = zeho_service_if~t_suppl[]
       CHANGING
         t_aa  = zeho_service_if~t_aa ).

      ENDIF.

      DATA lt_aa TYPE TABLE OF zeho_a_aa.
      CLEAR lt_aa.
      lt_aa = CORRESPONDING #( zeho_service_if~t_aa  MAPPING client = DEFAULT sy-mandt  ).

      MODIFY zeho_a_aa FROM TABLE @lt_aa.
      COMMIT WORK AND WAIT.



    ELSE.
      RAISE EXCEPTION TYPE zeho_cl_messages
        EXPORTING
          textid      = zeho_cl_messages=>response_mapping_not_found
          mv_bankcode = rd_acc->bankcode.
    ENDIF.





  ENDMETHOD.


  METHOD xml_parser.
    DATA : attributes TYPE if_sxml_attribute=>attributes.
    DATA l_node TYPE REF TO if_ixml_node.
    DATA return    TYPE bapiretct.
    DATA last_open_tag TYPE REF TO if_sxml_open_element.

    CLEAR zeho_service_if~t_xmltab.

    DATA(reader) = cl_sxml_string_reader=>create( zeho_service_if~xml_resx ).
    TRY.
        DO.
          DATA(node) = reader->read_next_node( ).
          IF reader->node_type = if_sxml_node=>co_nt_final.
            EXIT.
          ENDIF.

*      IF reader->node_type = if_sxml_node=>co_nt_element_open.
*        last_open_tag = CAST if_sxml_open_element( node ).
*      ENDIF.

          IF reader->node_type <> if_sxml_node=>co_nt_value.
            CONTINUE.
          ENDIF.

          APPEND INITIAL LINE TO zeho_service_if~t_xmltab ASSIGNING FIELD-SYMBOL(<fs_xml>).
          <fs_xml>-cname  = reader->name.
          <fs_xml>-cvalue = reader->value.


        ENDDO.
      CATCH cx_sxml_state_error INTO DATA(error_parse_token).
        DATA(error_text) = error_parse_token->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zeho_service_if~create_req.


    FIELD-SYMBOLS <fs_val> TYPE any.

    IF imp_class IS BOUND.
      imp_class->get_request(
      EXPORTING
        rd_acc  = rd_acc
      CHANGING
        request =  zeho_service_if~xml
    ).
    ENDIF.

    CHECK zeho_service_if~xml IS NOT INITIAL.

    LOOP AT zeho_service_if~t_reqm ASSIGNING FIELD-SYMBOL(<fs_mapping>).
      ASSIGN COMPONENT <fs_mapping>-input_value OF STRUCTURE rd_acc->* TO <fs_val>.
      IF <fs_val> IS ASSIGNED.
        CASE <fs_mapping>-input_tag.
          WHEN 'BEGDATE_FORMAT'.
            REPLACE <fs_mapping>-input_tag IN zeho_service_if~xml WITH <fs_val>.
            REPLACE 'YYYY' IN  zeho_service_if~xml WITH begdate(4).
            REPLACE 'MM' IN  zeho_service_if~xml WITH begdate+4(2).
            REPLACE 'DD' IN  zeho_service_if~xml WITH begdate+6(2).
            REPLACE 'HH' IN  zeho_service_if~xml WITH '00'.
            REPLACE 'MM' IN  zeho_service_if~xml WITH '00'.
            REPLACE 'SS' IN  zeho_service_if~xml WITH '00'.
            REPLACE 'TSTSTS' IN  zeho_service_if~xml WITH '000000'.
          WHEN 'ENDDATE_FORMAT'.
            REPLACE <fs_mapping>-input_tag IN zeho_service_if~xml WITH <fs_val>.
            REPLACE 'YYYY' IN  zeho_service_if~xml WITH enddate(4).
            REPLACE 'MM' IN  zeho_service_if~xml WITH enddate+4(2).
            REPLACE 'DD' IN  zeho_service_if~xml WITH enddate+6(2).
            REPLACE 'HH' IN  zeho_service_if~xml WITH '23'.
            REPLACE 'MM' IN  zeho_service_if~xml WITH '59'.
            REPLACE 'SS' IN  zeho_service_if~xml WITH '59'.
            REPLACE 'TSTSTS' IN  zeho_service_if~xml WITH '000000'.
          WHEN OTHERS.
            REPLACE <fs_mapping>-input_tag IN zeho_service_if~xml WITH <fs_val>.

        ENDCASE.
        UNASSIGN <fs_val>.
      ENDIF.

    ENDLOOP.


    IF imp_class IS BOUND.
*      instance->imp->get_request(
*        EXPORTING
*          rd_acc  = rd_acc
*        CHANGING
*          request =  zeho_service_if~xml
*      ).
      imp_class->create_req_processing(
        EXPORTING
          rd_acc  = rd_acc
          input_xml =  zeho_service_if~t_reqm
        CHANGING
          xml       =  zeho_service_if~xml
      ).
    ENDIF.



  ENDMETHOD.


  METHOD zeho_service_if~get_res.
    DATA lv_http_code TYPE i.
    TRY.
        zeho_service_if~response = zeho_service_if~http_client->execute( i_method = if_web_http_client=>post ).
      CATCH cx_web_http_client_error.
        "handle exception
    ENDTRY.

    zeho_service_if~response->get_last_error(
      RECEIVING
        rc = lv_http_code
    ).


    IF lv_http_code BETWEEN 200 AND 299.
      "Succesfull

    ELSEIF lv_http_code GE 100.
      "Error
      EXIT.
    ENDIF.


    zeho_service_if~xml_resx = zeho_service_if~response->get_binary(  ).
    DATA(lv_response) = zeho_service_if~response->get_text( ).
*    IF its soap service we will convert response to xml internal table
    FIND zeho_service_if~c_soap IN lv_response.
    IF sy-subrc  = 0.
      xml_parser( ).
    ENDIF.
*    IF its soap service we will convert response to xml internal table

    map_xmltable_to_db( rd_acc = rd_acc ).

  ENDMETHOD.


  METHOD zeho_service_if~send_req.


    DATA: url         TYPE string,
          username    TYPE string,
          password    TYPE string,
          host        TYPE string,
          contenttype TYPE string,
          slenght     TYPE i,
          lenght      TYPE string,
          soapaction  TYPE string,
          accept      TYPE string,
          ls_serv     TYPE zeho_s_serv.

    ls_serv = VALUE #( zeho_service_if~t_serv[ bankcode = rd_acc->bankcode
                                               bukrs    = rd_acc->bukrs ]  OPTIONAL ).

    url         = ls_serv-serv_url.
    username    = rd_acc->username.
    password    = rd_acc->password.
    host        = ls_serv-host.
    contenttype = ls_serv-content_type.
    soapaction  = ls_serv-soap_action.
    slenght     = strlen( zeho_service_if~xml ).

    lenght = slenght.
    TRY.
        zeho_service_if~http_client = cl_web_http_client_manager=>create_by_http_destination( cl_http_destination_provider=>create_by_url( url ) ).
      CATCH cx_web_http_client_error cx_http_dest_provider_error.
        "handle exception
    ENDTRY.
    zeho_service_if~request = zeho_service_if~http_client->get_http_request( ).
    zeho_service_if~request->set_header_field(
        EXPORTING
    i_name  = zeho_service_if~c_reqvalue
    i_value = zeho_service_if~c_post
    ).

    zeho_service_if~request->set_header_field(
    EXPORTING
    i_name  = zeho_service_if~c_serverprotocol
    i_value = zeho_service_if~c_http
).

    IF host IS NOT INITIAL.
      zeho_service_if~request->set_header_field(
     EXPORTING
     i_name  = zeho_service_if~c_host
     i_value = host
 ).
    ENDIF.


    IF contenttype IS NOT INITIAL.
      zeho_service_if~request->set_content_type( content_type = contenttype ).
    ENDIF.
    IF soapaction IS NOT INITIAL.
      zeho_service_if~request->set_header_field(
        EXPORTING
          i_name  = zeho_service_if~c_soapaction
          i_value = soapaction
      ).
    ENDIF.

    zeho_service_if~request->set_header_field(
      EXPORTING
        i_name  = zeho_service_if~c_accept
        i_value = zeho_service_if~c_acceptval11
    ).

    zeho_service_if~request->set_text( EXPORTING i_text = zeho_service_if~xml
                                    i_length = slenght
                                    i_offset = 0 ).


    IF imp_class IS BOUND.

      imp_class->send_req_processing(
        EXPORTING
          rd_acc      = rd_acc
          input_xml   = zeho_service_if~t_serv
          accounts    =  zeho_service_if~t_accounts
          begdate     = begdate
          enddate     = enddate
        CHANGING
          http_client = zeho_service_if~http_client
          request     = zeho_service_if~request
      ).

    ENDIF.


  ENDMETHOD.
ENDCLASS.
