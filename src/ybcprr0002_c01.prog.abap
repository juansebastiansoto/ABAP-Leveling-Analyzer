*&---------------------------------------------------------------------*
*&  Include           YBCPRR0002_C01
*&---------------------------------------------------------------------*

CLASS lcl_alv_0100 DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      evt_toolbar      FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      evt_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "lcl_alv_0100 DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_excel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_excel DEFINITION.

  PUBLIC SECTION.

    DATA: o_excel     TYPE REF TO zcl_excel,
          o_worksheet TYPE REF TO zcl_excel_worksheet.

    METHODS: constructor
      IMPORTING im_table TYPE ybctt0000,

      add_detail_table,

      save_file,

      send_mail.

  PROTECTED SECTION.

    DATA: t_dom_values TYPE STANDARD TABLE OF dd07v,
          t_result     TYPE ybctt0003.

    METHODS: file_save_get_name
      RETURNING VALUE(re_fullpath) TYPE string,

      conv_output_table
        EXPORTING ex_table TYPE ybctt0005,

      conv_icon_to_output
        IMPORTING im_value       TYPE ybcde0016
        RETURNING VALUE(re_text) TYPE val_text,

      read_domain_values.

ENDCLASS.                    "lcl_excel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_alv_0100 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_0100 IMPLEMENTATION.

  METHOD evt_toolbar.

    DATA: wl_toolbar TYPE LINE OF ttb_button.

    wl_toolbar-butn_type = 3.
    APPEND wl_toolbar TO e_object->mt_toolbar.

    wl_toolbar-butn_type = 0.
    wl_toolbar-function  = 'XLSX'.
    wl_toolbar-icon      = icon_xls.
    wl_toolbar-quickinfo = TEXT-q01.
    APPEND wl_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.                    "evt_toolbar

  METHOD evt_user_command.

    DATA: ol_excel TYPE REF TO lcl_excel.

    CASE e_ucomm.
      WHEN 'XLSX'.

        CREATE OBJECT ol_excel
          EXPORTING
            im_table = t_result.

        ol_excel->add_detail_table(  ).

        IF sy-batch EQ abap_true.
          ol_excel->send_mail( ).
        ELSE.
          ol_excel->save_file( ).
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "evt_user_command

ENDCLASS.                    "lcl_alv_0100 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_excel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_excel IMPLEMENTATION.

  METHOD constructor.

    me->t_result = im_table.

    CREATE OBJECT me->o_excel.

    me->read_domain_values( ).

  ENDMETHOD.                    "constructor

  METHOD add_detail_table.

    DATA: ol_cx_excel TYPE REF TO zcx_excel.

    DATA: tl_fcat  TYPE zexcel_t_fieldcatalog,
          tl_table TYPE ybctt0005.

    DATA: wl_settings TYPE zexcel_s_table_settings.

    DATA: vl_message  TYPE string.

    me->conv_output_table( IMPORTING ex_table = tl_table ).

    wl_settings-top_left_column = 'A'.
    wl_settings-top_left_row    = 1.

    tl_fcat = zcl_excel_common=>get_fieldcatalog( tl_table ).

    me->o_worksheet = me->o_excel->get_active_worksheet( ).

    TRY.

        me->o_worksheet->set_title( TEXT-s01 ).

        me->o_worksheet->bind_table( ip_table          = tl_table
                                     it_field_catalog  = tl_fcat
                                     is_table_settings = wl_settings ).

      CATCH zcx_excel INTO ol_cx_excel.

        vl_message = ol_cx_excel->if_message~get_text( ).

        MESSAGE vl_message TYPE 'E'.

    ENDTRY.


  ENDMETHOD.                    "add_table

  METHOD save_file.

    DATA: ol_writer   TYPE REF TO zif_excel_writer.

    DATA: tl_rawdata  TYPE solix_tab.

    DATA: vl_fullpath  TYPE string,
          vl_xdata     TYPE xstring,
          vl_bytecount TYPE i.

    vl_fullpath = me->file_save_get_name( ).

    IF vl_fullpath IS INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT ol_writer TYPE zcl_excel_writer_2007.

    vl_xdata     = ol_writer->write_file( me->o_excel ).
    vl_bytecount = xstrlen( vl_xdata ).
    tl_rawdata   = cl_bcs_convert=>xstring_to_solix( iv_xstring  = vl_xdata ).

    cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = vl_bytecount
                                                      filename     = vl_fullpath
                                                      filetype     = 'BIN'
                                             CHANGING data_tab     = tl_rawdata ).

  ENDMETHOD.                    "save_file

  METHOD file_save_get_name.

    DATA: vl_dummy             TYPE string,
          vl_default_file_name TYPE string.

    vl_default_file_name = p_devcl.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        file_filter       = zcl_excel_common=>c_xlsx_file_filter
        default_file_name = vl_default_file_name
      CHANGING
        filename          = vl_dummy
        path              = vl_dummy
        fullpath          = re_fullpath
      EXCEPTIONS
        OTHERS            = 0.

  ENDMETHOD.                    "file_save_get_name

  METHOD conv_output_table.

    DATA: wl_internal TYPE LINE OF ybctt0003,
          wl_output   TYPE LINE OF ybctt0005.

    LOOP AT me->t_result INTO wl_internal.

      MOVE-CORRESPONDING wl_internal TO wl_output.

      wl_output-qa_status  = me->conv_icon_to_output( wl_output-qa_status ).
      wl_output-prd_status = me->conv_icon_to_output( wl_output-prd_status ).
      wl_output-wrk_status = me->conv_icon_to_output( '@0A@' ).

      APPEND wl_output TO ex_table.

    ENDLOOP.

  ENDMETHOD.                    "conv_output_table

  METHOD conv_icon_to_output.

    DATA: wl_values TYPE dd07v.

    READ TABLE me->t_dom_values
    INTO wl_values
    WITH KEY domvalue_l = im_value.

    re_text = wl_values-ddtext.

  ENDMETHOD.                    "conv_icon_to_output

  METHOD read_domain_values.

    DATA: tl_dom_values_aux TYPE STANDARD TABLE OF dd07v.

    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname    = 'YBCDO0002'
      TABLES
        values_tab = me->t_dom_values
      EXCEPTIONS
        OTHERS     = 0.

    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname    = 'YBCDO0003'
      TABLES
        values_tab = tl_dom_values_aux
      EXCEPTIONS
        OTHERS     = 0.

    APPEND LINES OF tl_dom_values_aux TO me->t_dom_values.

  ENDMETHOD.                    "read_domain_values

  METHOD send_mail.

    DATA: ol_mail   TYPE REF TO zcl_mail_html,
          ol_writer TYPE REF TO zif_excel_writer.

    DATA: tl_rawdata           TYPE solix_tab,
          tl_attachment_header TYPE soli_tab.

    DATA: wl_att_header TYPE soli.

    DATA: vl_xdata     TYPE xstring,
          vl_bytecount TYPE sood-objlen,
          vl_body      TYPE string.

    CREATE OBJECT ol_writer TYPE zcl_excel_writer_2007.

    vl_xdata     = ol_writer->write_file( me->o_excel ).
    vl_bytecount = xstrlen( vl_xdata ).
    tl_rawdata   = cl_bcs_convert=>xstring_to_solix( iv_xstring  = vl_xdata ).

    vl_body = TEXT-001.

    READ TABLE s_mails[] INTO s_mails WITH KEY sign   = 'I'
                                               option = 'EQ'.

    CREATE OBJECT ol_mail
      EXPORTING
        ex_sender         = s_mails-low
        ex_subject        = TEXT-001
        ex_body           = vl_body
        ex_obj_tp         = 'RAW'
      EXCEPTIONS
        er_persistent     = 1
        er_document       = 2
        er_set_document   = 3
        er_create_address = 4
        er_set_sender     = 5
        er_set_subject    = 6
        er_set_attributes = 7
        er_attachment     = 8
        OTHERS            = 9.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    wl_att_header-line  = '&SO_FILENAME=Report.xlsx'.
    APPEND wl_att_header TO tl_attachment_header.

    CALL METHOD ol_mail->add_attachment
      EXPORTING
        im_type              = 'BIN'
        im_subject           = TEXT-001
        im_content           = tl_rawdata
        im_attachment_size   = vl_bytecount
        im_attachment_header = tl_attachment_header.

    LOOP AT s_mails[] INTO s_mails WHERE sign   EQ 'I'
                                     AND option EQ 'EQ'.

      CALL METHOD ol_mail->add_destination
        EXPORTING
          ex_recipient      = s_mails-low
        EXCEPTIONS
          er_create_address = 1
          er_recipient      = 2
          OTHERS            = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDLOOP.


    CALL METHOD ol_mail->send
      EXCEPTIONS
        er_sending     = 1
        er_immediately = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDMETHOD.                    "send_mail

ENDCLASS.                    "lcl_excel IMPLEMENTATION
