class YCL_BC_LEVELING_TRAN definition
  public
  inheriting from YCL_BC_LEVELING
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !OBJECT type CTS_OBJECT_KEY
    raising
      CX_T100_MSG
      CX_SY_CREATE_OBJECT_ERROR .

  methods GET_OBJECT_LIST
    redefinition .
protected section.

  methods DETERMINE_TEXT_OBJECT_CODE
    redefinition .
  methods EXISTENCE_CHECK
    redefinition .
  methods GET_LOCK_ENTRY
    redefinition .
  methods GET_OBJECT_CONTENT
    redefinition .
private section.

  methods FIX_LOCKED_TX
    changing
      !DATA type /GRCPI/GRIA_T_TAB512 .
  methods FIX_AUTH_TX
    changing
      !DATA type /GRCPI/GRIA_T_TAB512 .
ENDCLASS.



CLASS YCL_BC_LEVELING_TRAN IMPLEMENTATION.


METHOD constructor.

  CALL METHOD super->constructor
    EXPORTING
      object = object.

  IF  object-object IN me->get_object_list( )
  AND me->existence_check( ) EQ abap_false.
    RAISE EXCEPTION TYPE cx_sy_create_object_error.
  ENDIF.

ENDMETHOD.


METHOD determine_text_object_code.

  object_type = 'TRAN'.

ENDMETHOD.


METHOD existence_check.

    CALL FUNCTION 'RPY_EXISTENCE_CHECK_TRAN'
      DESTINATION server
      EXPORTING
        limu_key  = me->object-obj_name
      EXCEPTIONS
        not_exist = 1
        OTHERS    = 2.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    exist = abap_true.

  ENDMETHOD.


METHOD fix_auth_tx.

  DATA: vl_objct    TYPE tobj-objct.

  FIELD-SYMBOLS: <fsl_data> TYPE LINE OF /grcpi/gria_t_tab512,
                 <fsl_auth> TYPE xuobject.

  LOOP AT data ASSIGNING <fsl_data>.

    UNASSIGN <fsl_auth>.
    ASSIGN <fsl_data>-wa+20(10) TO <fsl_auth>.
    CHECK <fsl_auth> IS ASSIGNED.

    SELECT SINGLE objct
    FROM tobj
    INTO vl_objct
    WHERE objct EQ <fsl_auth>.

    CHECK sy-subrc NE 0.

    DELETE data WHERE wa+20(10) EQ <fsl_auth>.

  ENDLOOP.

ENDMETHOD.


METHOD fix_locked_tx.

  FIELD-SYMBOLS: <fsl_data>  TYPE LINE OF /grcpi/gria_t_tab512,
                 <fsl_value> TYPE c.

  LOOP AT data ASSIGNING <fsl_data>.

    UNASSIGN <fsl_value>.
    ASSIGN <fsl_data>+69(1) TO <fsl_value>.

    CHECK <fsl_value> IS ASSIGNED.

    CASE <fsl_value>.
      WHEN '2'. " Dialog transaction / Locked / No Auth
        <fsl_value> = '0'.
      WHEN 'A'. " Report transaction / Locked / No Auth
        <fsl_value> = '8'.
      WHEN '0' OR '8' OR '9'. " It's ok
    ENDCASE.

  ENDLOOP.

ENDMETHOD.


METHOD get_lock_entry.

  DATA: vl_strkorr  TYPE e070-strkorr.

* Try the global logic (Only for entries in TR with status D)
  CALL METHOD super->get_lock_entry.

  IF me->output-trkorr IS NOT INITIAL.
    RETURN.
  ENDIF.

  SELECT strkorr trkorr
  UP TO 1 ROWS
  FROM ybcvb_ot_content
  INTO (vl_strkorr, me->output-trkorr)
  WHERE pgmid    EQ 'R3TR'
    AND object   EQ me->output-object_type
    AND obj_name EQ me->output-object_name
  ORDER BY as4date DESCENDING.
  ENDSELECT.

* If was a task, get the TR
  IF vl_strkorr IS NOT INITIAL.
    me->output-trkorr = vl_strkorr.
  ENDIF.

  SELECT SINGLE as4date as4user
  FROM e070
  INTO (me->output-as4date, me->output-as4user)
  WHERE trkorr EQ me->output-trkorr.

  SELECT SINGLE as4text
  FROM e07t
  INTO me->output-as4text
  WHERE trkorr EQ me->output-trkorr.

ENDMETHOD.


METHOD get_object_content.

  DATA: tl_options TYPE STANDARD TABLE OF rfc_db_opt,
        tl_dummy   TYPE STANDARD TABLE OF rfc_db_fld,
        tl_data    TYPE STANDARD TABLE OF tab512,
        tl_data_tstca TYPE STANDARD TABLE OF tab512.

  DATA: wl_options TYPE rfc_db_opt,
        wl_data    TYPE tab512.

  DATA: vl_lines    TYPE sy-tabix.

  CONCATENATE `TCODE EQ '`
              me->object-obj_name
              `'`
  INTO wl_options-text.

  APPEND wl_options TO tl_options.

  CALL FUNCTION 'RFC_READ_TABLE' " Main
    DESTINATION server
    EXPORTING
      query_table          = 'TSTC'
    TABLES
      options              = tl_options
      fields               = tl_dummy
      data                 = tl_data
    EXCEPTIONS
      table_not_available  = 1
      table_without_data   = 2
      option_not_valid     = 3
      field_not_valid      = 4
      not_authorized       = 5
      data_buffer_exceeded = 6
      OTHERS               = 7.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

  me->fix_locked_tx( CHANGING data = tl_data ).

  CLEAR tl_dummy.

  CALL FUNCTION 'RFC_READ_TABLE' " Description Text
    DESTINATION server
    EXPORTING
      query_table          = 'TSTCT'
    TABLES
      options              = tl_options
      fields               = tl_dummy
      data                 = tl_data
    EXCEPTIONS
      table_not_available  = 1
      table_without_data   = 2
      option_not_valid     = 3
      field_not_valid      = 4
      not_authorized       = 5
      data_buffer_exceeded = 6
      OTHERS               = 7.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

* This sort its because the TSTCT entries can be read in diferente order
  SORT tl_data.

  vl_lines = lines( tl_data ).

  CLEAR tl_dummy.

  CALL FUNCTION 'RFC_READ_TABLE' " Additional attributes
    DESTINATION server
    EXPORTING
      query_table          = 'TSTCC'
    TABLES
      options              = tl_options
      fields               = tl_dummy
      data                 = tl_data
    EXCEPTIONS
      table_not_available  = 1
      table_without_data   = 2
      option_not_valid     = 3
      field_not_valid      = 4
      not_authorized       = 5
      data_buffer_exceeded = 6
      OTHERS               = 7.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

* Read last line to check if TSTCC entry is empty.
  IF vl_lines NE lines( tl_data ).

    vl_lines = lines( tl_data ).
    READ TABLE tl_data INTO wl_data INDEX vl_lines.
    REPLACE FIRST OCCURRENCE OF me->object-obj_name IN wl_data-wa WITH ''.

    IF wl_data-wa IS INITIAL.
      DELETE tl_data INDEX vl_lines.
    ENDIF.

  ENDIF.

  CLEAR tl_dummy.

  CALL FUNCTION 'RFC_READ_TABLE' " Authorizations
    DESTINATION server
    EXPORTING
      query_table          = 'TSTCA'
    TABLES
      options              = tl_options
      fields               = tl_dummy
      data                 = tl_data_tstca
    EXCEPTIONS
      table_not_available  = 1
      table_without_data   = 2
      option_not_valid     = 3
      field_not_valid      = 4
      not_authorized       = 5
      data_buffer_exceeded = 6
      OTHERS               = 7.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

  me->fix_auth_tx( CHANGING data = tl_data_tstca ).

  IF tl_data_tstca IS NOT INITIAL.
    APPEND LINES OF tl_data_tstca TO tl_data.
  ENDIF.

  me->add_to_string( EXPORTING input = tl_data CHANGING output = object_content ).

ENDMETHOD.


METHOD get_object_list.

    DATA: wl_list TYPE LINE OF devtyrange.

    wl_list-sign   = 'I'.
    wl_list-option = 'EQ'.
    wl_list-low    = 'TRAN'.
    APPEND wl_list TO list.

  ENDMETHOD.
ENDCLASS.
