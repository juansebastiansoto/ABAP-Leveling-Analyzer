*&---------------------------------------------------------------------*
*&      Form  ANALYZE
*&---------------------------------------------------------------------*
FORM analyze.

  DATA: ol_analyzer TYPE REF TO ycl_bc_leveling,
        ol_t100_msg TYPE REF TO cx_t100_msg.

  DATA: wl_object TYPE cts_object_key.

  DATA: vl_total_object TYPE i,
        vl_percentage   TYPE i,
        vl_round        TYPE i, " Show by 10 steps
        vl_message      TYPE string.

  FIELD-SYMBOLS: <fsl_result> TYPE ybces0003.

  vl_total_object = lines( t_result ).

  LOOP AT t_result ASSIGNING <fsl_result>.

    TRY.

        vl_round = sy-tabix MOD 10.

        IF vl_round EQ 0.
          vl_percentage    = sy-tabix * 100 / vl_total_object.

          MESSAGE s003(ybc001) WITH sy-tabix vl_total_object INTO vl_message.
*       Analyzing object &1 of &2.

          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
            EXPORTING
              percentage = vl_percentage
              text       = vl_message.

        ENDIF.

        PERFORM get_pgmid_value USING <fsl_result>-object_type
                             CHANGING wl_object-pgmid.

        wl_object-object   = <fsl_result>-object_type.
        wl_object-obj_name = <fsl_result>-object_name.

        ol_analyzer ?= ycl_bc_leveling=>get_instance( wl_object ).

        ol_analyzer->analyze( ).

        <fsl_result> = ol_analyzer->get_result( ).

      CATCH cx_sy_create_object_error.

        MESSAGE s002(ybc001) INTO <fsl_result>-message.
*       Out of scope.

      CATCH cx_t100_msg INTO ol_t100_msg.

        IF ck_out EQ abap_true.

          MESSAGE s002(ybc001) INTO <fsl_result>-message.
*         Out of scope.

        ELSE.

          MESSAGE ID ol_t100_msg->t100_msgid
                TYPE 'S'
              NUMBER ol_t100_msg->t100_msgno
                WITH ol_t100_msg->t100_msgv1
                     ol_t100_msg->t100_msgv2
                     ol_t100_msg->t100_msgv3
                     ol_t100_msg->t100_msgv4
                INTO <fsl_result>-message.

        ENDIF.

      CATCH cx_cts_object_not_found.

        DELETE t_result WHERE object_type EQ <fsl_result>-object_type
                          AND object_name EQ <fsl_result>-object_name.

    ENDTRY.

  ENDLOOP.

  IF ck_out EQ abap_true.
    MESSAGE s002(ybc001) INTO vl_message.
    DELETE t_result WHERE message EQ vl_message.
  ENDIF.

  IF ck_hok EQ abap_true.
    DELETE t_result WHERE qa_status  EQ icon_checked
                      AND prd_status EQ icon_checked.
  ENDIF.

  DELETE t_result WHERE trkorr IN s_notr.

ENDFORM.                    " ANALYZE

*&---------------------------------------------------------------------*
*&      Form  PBO_0100
*&---------------------------------------------------------------------*
FORM pbo_0100.

  DATA: tl_fieldcat          TYPE lvc_t_fcat,
        tl_toolbar_excluding TYPE ui_functions.

  DATA: wl_layout   TYPE lvc_s_layo.

  PERFORM toolbar_0100  CHANGING tl_toolbar_excluding.
  PERFORM fieldcat_0100 CHANGING tl_fieldcat.
  PERFORM layout_0100   CHANGING wl_layout.

  IF o_alv_0100 IS BOUND.
    o_alv_0100->free( ).
    CLEAR o_alv_0100.
    o_cc_0100->free( ).
    CLEAR o_cc_0100.
  ENDIF.

  IF sy-batch EQ abap_false.
    CREATE OBJECT o_cc_0100
      EXPORTING
        container_name              = 'CC_0100'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  CREATE OBJECT o_alv_0100
    EXPORTING
      i_parent          = o_cc_0100
      i_name            = 'MAIN_ALV'
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  SET HANDLER lcl_alv_0100=>evt_user_command FOR o_alv_0100.
  SET HANDLER lcl_alv_0100=>evt_toolbar      FOR o_alv_0100.

  CALL METHOD o_alv_0100->set_table_for_first_display
    EXPORTING
      is_layout                     = wl_layout
      it_toolbar_excluding          = tl_toolbar_excluding
    CHANGING
      it_outtab                     = t_result
      it_fieldcatalog               = tl_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                                                    " PBO_0100

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_0100
*&---------------------------------------------------------------------*
FORM fieldcat_0100  CHANGING pt_fieldcat TYPE lvc_t_fcat.

  DATA: ol_alv_magic TYPE REF TO zcl_alv.

  DATA: wl_structure TYPE ybces0003,
        wl_fieldcat  TYPE LINE OF lvc_t_fcat.

  CREATE OBJECT ol_alv_magic.

  CALL METHOD ol_alv_magic->fill_catalog_merge
    EXPORTING
      structure = wl_structure
      texts     = abap_true
      convexit  = abap_true
      tabname   = 'T_RESULT'
    IMPORTING
      fieldcat  = pt_fieldcat.

  wl_fieldcat-icon = abap_true.

  MODIFY pt_fieldcat
  FROM wl_fieldcat
  TRANSPORTING icon
  WHERE fieldname EQ 'QA_STATUS'
     OR fieldname EQ 'PRD_STATUS'.

ENDFORM.                    " FIELDCAT_0100

*&---------------------------------------------------------------------*
*&      Form  LAYOUT_0100
*&---------------------------------------------------------------------*
FORM layout_0100  CHANGING pw_layout TYPE lvc_s_layo.

  pw_layout-zebra = pw_layout-cwidth_opt = abap_true.

ENDFORM.                    " LAYOUT_0100

*&---------------------------------------------------------------------*
*&      Form  OBJECT_SELECTION
*&---------------------------------------------------------------------*
FORM object_selection.

  TYPES: BEGIN OF tyl_e071,
           object   TYPE e071-object,
           obj_name TYPE e071-obj_name,
         END OF tyl_e071,
         BEGIN OF tyl_tadir,
           object   TYPE tadir-object,
           obj_name TYPE tadir-obj_name,
         END OF tyl_tadir.

  DATA: tl_e071  TYPE STANDARD TABLE OF tyl_e071,
        tl_tadir TYPE STANDARD TABLE OF tyl_tadir.

  DATA: wl_e071   TYPE tyl_e071,
        wl_tadir  TYPE tyl_tadir,
        wl_result TYPE ybces0003.

  MESSAGE s004(ybc001).
* Starting selection objects.

* Check minimum input
  IF  p_trk   IS INITIAL
  AND p_obtyp IS INITIAL
  AND s_obnam IS INITIAL
  AND p_devcl IS INITIAL.
    MESSAGE e001(ybc001).
*   Complete at least Transport Request or Object Type and Object Name.
  ENDIF.

  SELECT items~object items~obj_name
  FROM e070 AS header
  INNER JOIN e071 AS items
  ON header~trkorr EQ items~trkorr
  INTO TABLE tl_e071
  WHERE header~trkorr EQ p_trk " Transport Request
     OR items~trkorr  EQ p_trk " Task
     OR ( items~object EQ p_obtyp AND obj_name IN s_obnam ). " Free Object Selection

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  CLEAR t_result.

  LOOP AT tl_e071 INTO wl_e071.

    wl_result-object_type = wl_e071-object.
    wl_result-object_name = wl_e071-obj_name.
    APPEND wl_result TO t_result.

  ENDLOOP.

  SELECT object obj_name
  FROM tadir
  INTO TABLE tl_tadir
  WHERE devclass EQ p_devcl
    OR ( object  EQ p_obtyp AND obj_name IN s_obnam ). " Free Object Selection

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  LOOP AT tl_tadir INTO wl_tadir.

    wl_result-object_type = wl_tadir-object.
    wl_result-object_name = wl_tadir-obj_name.
    APPEND wl_result TO t_result.

  ENDLOOP.

  SORT t_result BY object_type object_name.
  DELETE ADJACENT DUPLICATES FROM t_result COMPARING ALL FIELDS.

ENDFORM.                    " OBJECT_SELECTION

*&---------------------------------------------------------------------*
*&      Form  SUBOBJECT_SELECTION
*&---------------------------------------------------------------------*
*  Get the main object and after get the subobjects
*----------------------------------------------------------------------*
FORM subobject_selection.

  DATA: tl_obj_tab    TYPE STANDARD TABLE OF vrso,
        tl_subobjects TYPE STANDARD TABLE OF ybces0003.

  DATA: wl_result     TYPE ybces0003,
        wl_e071       TYPE e071,
        wl_obj_tab    TYPE vrso,
        wl_subobjects TYPE ybces0003.

  DATA: vl_duplicate  TYPE boolean.

  LOOP AT t_result INTO wl_result WHERE object_type NE 'WEBS'.

    CLEAR: tl_obj_tab,
           wl_e071.

    wl_e071-object   = wl_result-object_type.
    wl_e071-obj_name = wl_result-object_name.

    CALL FUNCTION 'GET_R3TR_OBJECT_FROM_LIMU_OBJ'
      EXPORTING
        p_limu_objtype = wl_e071-object
        p_limu_objname = wl_e071-obj_name
      IMPORTING
        p_r3tr_objtype = wl_e071-object
        p_r3tr_objname = wl_e071-obj_name
      EXCEPTIONS
        no_mapping     = 1
        OTHERS         = 2.

    CHECK sy-subrc EQ 0.

    wl_e071-pgmid = 'R3TR'.

    CALL FUNCTION 'SVRS_RESOLVE_E071_OBJ'
      EXPORTING
        e071_obj        = wl_e071
      TABLES
        obj_tab         = tl_obj_tab
      EXCEPTIONS
        not_versionable = 1
        OTHERS          = 2.

    CHECK sy-subrc EQ 0.

    LOOP AT tl_obj_tab INTO wl_obj_tab.

      CLEAR: wl_subobjects,
             vl_duplicate.

      PERFORM check_duplicate_from_limu USING wl_obj_tab
                                     CHANGING vl_duplicate.

      CHECK vl_duplicate EQ abap_false.

      wl_subobjects-object_type = wl_obj_tab-objtype.
      wl_subobjects-object_name = wl_obj_tab-objname.
      APPEND wl_subobjects TO tl_subobjects.

    ENDLOOP.

  ENDLOOP.

  APPEND LINES OF tl_subobjects TO t_result.

  SORT t_result BY object_type object_name.
  DELETE ADJACENT DUPLICATES FROM t_result COMPARING object_type object_name.

ENDFORM.                    " SUBOBJECT_SELECTION

*&---------------------------------------------------------------------*
*&      Form  REMOVE_NOT_CUSTOM_OBJECT
*&---------------------------------------------------------------------*
FORM remove_not_custom_object.

  DATA: wl_result TYPE ybces0003.

  DATA: vl_devclass     TYPE tadir-devclass,
        vl_genflag      TYPE tadir-genflag,
        vl_limu_objtype TYPE trobjtype,
        vl_r3tr_objtype TYPE trobjtype,
        vl_limu_objname TYPE trobj_name,
        vl_r3tr_objname TYPE trobj_name.

  LOOP AT t_result INTO wl_result WHERE object_type NE 'WEBS'
                                    AND object_type NE 'ACGR'.

    CLEAR: vl_devclass, vl_genflag.

    vl_limu_objtype = wl_result-object_type.
    vl_limu_objname = wl_result-object_name.

    CALL FUNCTION 'GET_R3TR_OBJECT_FROM_LIMU_OBJ'
      EXPORTING
        p_limu_objtype = vl_limu_objtype
        p_limu_objname = vl_limu_objname
      IMPORTING
        p_r3tr_objtype = vl_r3tr_objtype
        p_r3tr_objname = vl_r3tr_objname
      EXCEPTIONS
        no_mapping     = 1
        OTHERS         = 2.

    IF sy-subrc NE 0.
      vl_r3tr_objtype = wl_result-object_type.
      vl_r3tr_objname = wl_result-object_name.
    ENDIF.

    SELECT SINGLE devclass genflag
    FROM tadir
    INTO (vl_devclass, vl_genflag)
    WHERE pgmid    EQ 'R3TR'
      AND object   EQ vl_r3tr_objtype
      AND obj_name EQ vl_r3tr_objname.

    CHECK sy-subrc EQ 0.

    CHECK vl_devclass(1) NA 'ZY'
       OR vl_genflag     EQ abap_true.

    DELETE t_result WHERE object_type EQ wl_result-object_type
                      AND object_name EQ wl_result-object_name.

  ENDLOOP.

  SORT t_result.
  DELETE ADJACENT DUPLICATES FROM t_result COMPARING ALL FIELDS.

ENDFORM.                    " REMOVE_NOT_CUSTOM_OBJECT

*&---------------------------------------------------------------------*
*&      Form  TOOLBAR_0100
*&---------------------------------------------------------------------*
FORM toolbar_0100  CHANGING pt_toolbar_excluding TYPE ui_functions.

  APPEND cl_gui_alv_grid=>mc_fc_info       TO pt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_graph      TO pt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_mb_export     TO pt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_mb_view       TO pt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_print_back TO pt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_mb_subtot     TO pt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_mb_sum        TO pt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_detail     TO pt_toolbar_excluding.

ENDFORM.                    " TOOLBAR_0100

*&---------------------------------------------------------------------*
*& Form REMOVE_DELETED_OBJECTS
*&---------------------------------------------------------------------*
FORM remove_deleted_objects.

  DATA: wl_result TYPE ybces0003.

  DATA: vl_delflag TYPE tadir-delflag.

  LOOP AT t_result INTO wl_result.

    SELECT SINGLE delflag
    FROM tadir
    INTO vl_delflag
    WHERE pgmid    EQ 'R3TR'
      AND object   EQ wl_result-object_type
      AND obj_name EQ wl_result-object_name.

    CHECK sy-subrc EQ 0.

    CHECK vl_delflag EQ abap_true.

    DELETE t_result INDEX sy-tabix.

    CLEAR vl_delflag.

  ENDLOOP.

ENDFORM.                    "remove_deleted_objects

*&---------------------------------------------------------------------*
*& Form GET_PGMID_VALUE
*&---------------------------------------------------------------------*
*&   Return R3TR or LIMU from the OBJECT_TYPE checking the value
*&---------------------------------------------------------------------*
*&      --> PV_OBJECT_TYPE
*&      <-- PV_OBJECT_PGMID
*&---------------------------------------------------------------------*
FORM get_pgmid_value  USING    pv_object_type  TYPE cts_object_key-object
                      CHANGING pv_object_pgmid TYPE cts_object_key-pgmid.

  STATICS: tl_object_list TYPE STANDARD TABLE OF ko100.

  DATA: wl_object_list TYPE ko100.

  IF tl_object_list IS INITIAL.

    CALL FUNCTION 'TRINT_OBJECT_TABLE'
      EXPORTING
        iv_complete  = abap_true
      TABLES
        tt_types_out = tl_object_list.

    SORT tl_object_list BY object.

  ENDIF.

  READ TABLE tl_object_list
  INTO wl_object_list
  WITH KEY object = pv_object_type
  BINARY SEARCH.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  pv_object_pgmid = wl_object_list-pgmid.

ENDFORM.                    "get_pgmid_value

*&---------------------------------------------------------------------*
*&      Form  FILL_OUT_OF_LEVELING
*&---------------------------------------------------------------------*
* Fill the out of leveling data (Example: Development for only DEV Env.
*----------------------------------------------------------------------*
FORM fill_out_of_leveling.

  SELECT sign opti low high
  FROM tvarvc
  INTO TABLE s_notr
  WHERE name EQ 'LEVELING_TR_OUT'
    AND type EQ 'S'.

ENDFORM.                    " FILL_OUT_OF_LEVELING

*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL
*&---------------------------------------------------------------------*
FORM send_mail.

  DATA: ol_event  TYPE REF TO lcl_alv_0100.

  IF sy-batch EQ abap_false.
    RETURN.
  ENDIF.

  CREATE OBJECT ol_event.

  ol_event->evt_user_command( e_ucomm = 'XLSX' ).

ENDFORM.                    " SEND_MAIL

*&---------------------------------------------------------------------*
*&      Form  CHECK_DUPLICATE_FROM_LIMU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_OBJ_TAB  text
*      <--P_VL_DUPLICATE  text
*----------------------------------------------------------------------*
FORM check_duplicate_from_limu  USING    pw_obj_tab   TYPE vrso
                                CHANGING pv_duplicate TYPE boolean.

  DATA: tl_result TYPE HASHED TABLE OF ybces0000 WITH UNIQUE KEY object_type object_name.

  tl_result = t_result.

  CASE pw_obj_tab-objtype.
    WHEN 'REPS'.

      READ TABLE tl_result
      WITH TABLE KEY object_type = 'PROG'
                     object_name = pw_obj_tab-objname
      TRANSPORTING NO FIELDS.

    WHEN 'CLSD'.

      READ TABLE tl_result
      WITH TABLE KEY object_type = 'CLAS'
                     object_name = pw_obj_tab-objname
      TRANSPORTING NO FIELDS.

    WHEN OTHERS.
      RETURN.
  ENDCASE.

  IF sy-subrc EQ 0.
    pv_duplicate = abap_true.
  ENDIF.

ENDFORM.                    " CHECK_DUPLICATE_FROM_LIMU
