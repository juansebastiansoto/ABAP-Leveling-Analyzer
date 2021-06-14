class YCL_BC_LEVELING_SUSO definition
  public
  inheriting from YCL_BC_LEVELING
  final
  create public .

public section.

  methods GET_OBJECT_LIST
    redefinition .
protected section.

  methods DETERMINE_TEXT_OBJECT_CODE
    redefinition .
  methods EXISTENCE_CHECK
    redefinition .
  methods GET_OBJECT_CONTENT
    redefinition .
  methods GET_LOCK_ENTRY
    redefinition .
private section.
ENDCLASS.



CLASS YCL_BC_LEVELING_SUSO IMPLEMENTATION.


METHOD determine_text_object_code.

  object_type = 'SUSO'.

ENDMETHOD.


  METHOD existence_check.

    CALL FUNCTION 'RPY_EXISTENCE_CHECK_SUSO'
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
        tl_data    TYPE STANDARD TABLE OF tab512.

  DATA: wl_options TYPE rfc_db_opt.

  CONCATENATE `OBJCT EQ '`
              me->object-obj_name
              `'`
  INTO wl_options-text.

  APPEND wl_options TO tl_options.

  CALL FUNCTION 'RFC_READ_TABLE' " Main
    DESTINATION server
    EXPORTING
      query_table          = 'TOBJ'
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

  CLEAR: tl_dummy, tl_options, wl_options.

  CONCATENATE `OBJECT EQ '`
              me->object-obj_name
              `'`
  INTO wl_options-text.

  APPEND wl_options TO tl_options.

  CALL FUNCTION 'RFC_READ_TABLE' " Description Text
    DESTINATION server
    EXPORTING
      query_table          = 'TOBJT'
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

  me->add_to_string( EXPORTING input = tl_data CHANGING output = object_content ).

ENDMETHOD.


METHOD get_object_list.

    DATA: wl_list TYPE LINE OF devtyrange.

    wl_list-sign   = 'I'.
    wl_list-option = 'EQ'.
    wl_list-low    = 'SUSO'.
    APPEND wl_list TO list.

  ENDMETHOD.
ENDCLASS.
