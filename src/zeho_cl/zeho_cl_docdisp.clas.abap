CLASS zeho_cl_docdisp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.


    INTERFACES if_sadl_exit .
    INTERFACES if_sadl_exit_calc_element_read .

    DATA cv_hostname TYPE string.
    METHODS get_hostname RETURNING VALUE(rv_hostname) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZEHO_CL_DOCDISP IMPLEMENTATION.


  METHOD get_hostname.
    rv_hostname = xco_cp=>current->tenant( )->get_url( xco_cp_tenant=>url_type->ui )->get_host( ).
  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~calculate.
*    ct_calculated_data = it_original_data.
    DATA lt_Data TYPE TABLE OF zeho_c_activities.
    lt_Data = CORRESPONDING #( it_original_data ).
    DATA lv_tabix TYPE i.
    LOOP AT lt_Data ASSIGNING FIELD-SYMBOL(<fs_data>).
      lv_tabix = sy-tabix.
      IF <fs_data>-belnr IS NOT INITIAL AND <fs_data>-gjahr IS NOT INITIAL.

        <fs_data>-DocumentDisplayURL = 'https://' && get_hostname( ) && '/ui#'.
        <fs_data>-DocumentDisplayURL =  <fs_data>-DocumentDisplayURL &&
                                           'GLAccount-displayGLLineItemReportingView?AccountingDocument=' && <fs_data>-belnr &&
                                           '&CompanyCode=' && <fs_data>-bukrs &&
                                           '&FiscalYear=' && <fs_data>-gjahr.

      ENDIF.
*    <fs_data>-DocumentDisplayUrl =
    ENDLOOP.
    ct_calculated_data = CORRESPONDING #( lt_data ).

  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~get_calculation_info.

  ENDMETHOD.
ENDCLASS.
