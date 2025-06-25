CLASS lhc_Singleton DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS augment_cba_Bank FOR MODIFY
       IMPORTING
          ENTITIES_CREATE FOR CREATE Singleton\_Bank
          ENTITIES_UPDATE FOR UPDATE Bank.
*    METHODS augment_cba_Comps FOR MODIFY
*      IMPORTING entities_Crete FOR CREATE Bank\_Comps.
*                entities_update FOR UPDATE Comp.

ENDCLASS.

CLASS lhc_Singleton IMPLEMENTATION.

  METHOD augment_cba_Bank.
    DATA: text_for_new_entity      TYPE TABLE FOR CREATE zeho_i_bank\_Comps,
          text_for_existing_entity TYPE TABLE FOR CREATE zeho_i_bank\_Comps,
          text_update              TYPE TABLE FOR UPDATE zeho_i_comps.
    DATA: relates_create TYPE abp_behv_relating_tab,
          relates_update TYPE abp_behv_relating_tab,
          relates_cba    TYPE abp_behv_relating_tab.
    DATA: text_tky_link  TYPE STRUCTURE FOR READ LINK zeho_i_bank\_Comps,
          text_tky       LIKE text_tky_link-target.


   LOOP AT entities_create INTO DATA(entity).
      DATA(tabix) = sy-tabix.
      LOOP AT entity-%TARGET ASSIGNING FIELD-SYMBOL(<target>).
        APPEND tabix TO relates_create.
        INSERT VALUE #( %CID_REF = <target>-%CID
                        %IS_DRAFT = <target>-%IS_DRAFT
                        %KEY-Bankcode = <target>-%KEY-Bankcode

                        %TARGET = VALUE #( (
                          %CID = |CREATETEXTCID{ tabix }_{ sy-tabix }|
                          %IS_DRAFT = <target>-%IS_DRAFT
                          Bankcode  = <target>-bankcode
*                          bank_desc = <target>-bank_desc
*                         bank_desc     = <target>-bank_desc
*                          bank_desc = <target>-bank_desc
                          %CONTROL-Bukrs = if_abap_behv=>mk-on
                          %CONTROL-Bankcode = <target>-%CONTROL-Bankcode ) ) )
                     INTO TABLE text_for_new_entity.
      ENDLOOP.
    ENDLOOP.


  ENDMETHOD.

*  METHOD augment_cba_Comps.
*    DATA: text_for_new_entity      TYPE TABLE FOR CREATE zeho_i_bank\_Comps,
*          text_for_existing_entity TYPE TABLE FOR CREATE zeho_i_bank\_Comps,
*          text_update              TYPE TABLE FOR UPDATE zeho_i_comps.
*    DATA: relates_create TYPE abp_behv_relating_tab,
*          relates_update TYPE abp_behv_relating_tab,
*          relates_cba    TYPE abp_behv_relating_tab.
*    DATA: text_tky_link  TYPE STRUCTURE FOR READ LINK zeho_i_bank\_Comps,
*          text_tky       LIKE text_tky_link-target.
*
*
*
*  ENDMETHOD.

ENDCLASS.

*CLASS lhc_Bank DEFINITION INHERITING FROM cl_abap_behavior_handler.
*  PRIVATE SECTION.
*
*    METHODS augment_update FOR MODIFY
*      IMPORTING entities FOR UPDATE Bank.
*
*ENDCLASS.

*CLASS lhc_Bank IMPLEMENTATION.
*
*  METHOD augment_update.
*  ENDMETHOD.
*
*ENDCLASS.
