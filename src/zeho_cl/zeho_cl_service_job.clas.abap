CLASS zeho_cl_service_job DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_apj_dt_exec_object .
    INTERFACES if_apj_rt_exec_object .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZEHO_CL_SERVICE_JOB IMPLEMENTATION.


  METHOD if_apj_dt_exec_object~get_parameters.
    et_parameter_def = VALUE #(
      ( selname = 'SO_BUKRS'  kind = if_apj_dt_exec_object=>select_option datatype = 'C' length = 4  param_text = 'Company Code' changeable_ind = abap_true   )
      ( selname = 'SO_BANKK'  kind = if_apj_dt_exec_object=>select_option datatype = 'C' length = 15 param_text = 'Bankcode'     changeable_ind = abap_true  )
      ( selname = 'SO_ACC'  kind = if_apj_dt_exec_object=>select_option datatype = 'C' length = 30   param_text = 'Account No'    changeable_ind = abap_true  )
      ( selname = 'P_BDATE'  kind = if_apj_dt_exec_object=>parameter datatype = 'D' length = 8   param_text = 'Begin Date'      changeable_ind = abap_true mandatory_ind = abap_true  )
      ( selname = 'P_EDATE'  kind = if_apj_dt_exec_object=>parameter datatype = 'D' length = 8   param_text = 'End Date'      changeable_ind = abap_true mandatory_ind = abap_true  )
     ).






  ENDMETHOD.


  METHOD if_apj_rt_exec_object~execute.
    DATA : tt_bankcode TYPE zeho_tt_bankcode_range,
           tt_bukrs    TYPE zeho_tt_bukrs_range,
           tt_account  TYPE zeho_tt_acc_range,
           p_begdate   TYPE datum,
           p_enddate   TYPE datum.
    DATA lcl_service TYPE REF TO zeho_cl_service.

    TRY .
        DATA(l_log) = cl_bali_log=>create_with_header( header =  cl_bali_header_setter=>create(
                                                                    object      = 'ZEHO_LO_SERVICE'
                                                                    subobject   = 'ZEHO_sO_SERVICE'
                                                                  )  ).
      CATCH cx_bali_runtime.

    ENDTRY.

    DATA(ls_item_message) = cl_bali_free_text_setter=>create(
                              severity = if_bali_constants=>c_severity_information
                              text     = 'Job Started'

                            ).

    TRY.
        l_log->add_item(  item = ls_item_message ).
      CATCH cx_bali_runtime.
    ENDTRY.

    LOOP AT it_parameters ASSIGNING FIELD-SYMBOL(<fs_parameter>).
      CASE <fs_parameter>-selname.
        WHEN 'SO_BUKRS'.
          APPEND VALUE #(
              sign = <fs_parameter>-sign
              option = <fs_parameter>-option
              low    = <fs_parameter>-low
              high   = <fs_parameter>-high
          ) TO tt_bukrs.
        WHEN 'SO_BANKK'.
          APPEND VALUE #(
            sign = <fs_parameter>-sign
            option = <fs_parameter>-option
            low    = <fs_parameter>-low
            high   = <fs_parameter>-high
          ) TO tt_bankcode.
        WHEN 'SO_ACC'.
          APPEND VALUE #(
           sign   = <fs_parameter>-sign
           option = <fs_parameter>-option
           low    = <fs_parameter>-low
           high   = <fs_parameter>-high
         ) TO tt_account.
        WHEN 'P_BDATE'.
          p_begdate = <fs_parameter>-low.
        WHEN 'P_EDATE'.
          p_enddate = <fs_parameter>-low.
      ENDCASE.
    ENDLOOP.
    TRY .
        CREATE OBJECT lcl_service
          EXPORTING
            bankcode = tt_bankcode
            bukrs    = tt_bukrs
            account  = tt_account
            begdate  = p_begdate
            enddate  = p_enddate.
      CATCH zeho_cl_messages.

    ENDTRY.


    try.
        cl_bali_log_db=>get_instance(  )->save_log( log =  l_log  assign_to_current_appl_job = abap_true
        ).
      catch cx_bali_runtime .

    endtry.



  ENDMETHOD.
ENDCLASS.
