FUNCTION zeho_fm_post.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TT_ACT) TYPE  ZEHO_TT_ACTIVITIES
*"----------------------------------------------------------------------
*  RAISING
*    CX_SY_ZERODIVIDE
*    RESUMABLE(CX_SY_ASSIGN_CAST_ERROR).


    DATA: lt_je_deep TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
          lv_cid     TYPE abp_behv_cid,
          ls_je      TYPE STRUCTURE FOR ACTION IMPORT i_journalentrytp~post.
*          lt_gl      TYPE TABLE FOR ACTION IMPORT  i_journalentrytp~Post-
*          param      TYPE STRUCTURE FOR HIERARCHY d_journalentrypostparameter.
    DATA : cl_document TYPE REF TO zeho_cl_document_processing.




     CLEAR:  lt_je_deep , ls_je.

    TRY.
        lv_cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
      CATCH cx_uuid_error.
        ASSERT 1 = 0.
    ENDTRY.
    ls_je-%cid = lv_cid.

    IF cl_document IS NOT BOUND.
      CREATE OBJECT cl_document.
    ENDIF.

    DATA ls_aa TYPE zeho_i_activities.
    LOOP AT tt_act ASSIGNING FIELD-SYMBOL(<fs_aa>).

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
          e_hkont   = <fs_aa>-hkont
        CHANGING
          tt_glitems = ls_je-%param-_glitems
      ).

      IF <fs_aa>-secondgl_acc IS NOT INITIAL.


        cl_document->fill_gl_account(
        EXPORTING
          rd_aa     = ls_aa
          e_hkont   = <fs_aa>-secondgl_acc
        CHANGING
          tt_glitems = ls_je-%param-_glitems
            ).
      ELSEIF <fs_aa>-lifnr IS NOT INITIAL.

        cl_document->fill_supplier(
          EXPORTING
            rd_aa      = ls_aa
          CHANGING
            tt_apitems = ls_je-%param-_apitems
        ).
      ELSEIF <fs_aa>-kunnr IS NOT INITIAL.
        cl_document->fill_customer(
          EXPORTING
            rd_aa      = ls_aa
          CHANGING
            tt_aritems =  ls_je-%param-_aritems
        ).
      ENDIF.


        APPEND ls_je TO lt_je_deep.

      MODIFY ENTITIES OF i_journalentrytp
        ENTITY journalentry
        EXECUTE post FROM lt_je_deep
        FAILED DATA(ls_failed_deep)
        REPORTED DATA(ls_reported_deep)
        MAPPED DATA(ls_mapped_deep).

      IF ls_failed_deep IS NOT INITIAL.

        LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
          DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).

        ENDLOOP.
      ELSE.

      ENDIF.



    ENDLOOP.









ENDFUNCTION.
