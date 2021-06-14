class YCL_BC_LEVELING_ACGR definition
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
  methods GET_OBJECT_TYPE_TEXT
    redefinition .
  methods GET_LOCK_ENTRY
    redefinition .
private section.
ENDCLASS.



CLASS YCL_BC_LEVELING_ACGR IMPLEMENTATION.


  METHOD determine_text_object_code.

    object_type = 'ACGR'.

  ENDMETHOD.


METHOD existence_check.

  CALL FUNCTION 'RPY_EXISTENCE_CHECK_ACGR'
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

  TYPES: BEGIN OF tyl_e070,
           trkorr  TYPE e070-trkorr,
           as4date TYPE e070-as4date,
           as4user TYPE e070-as4user,
           strkorr TYPE e070-strkorr,
         END OF tyl_e070.

  DATA: tl_trkorr   TYPE STANDARD TABLE OF tyl_e070.

  DATA: wl_trkorr   TYPE tyl_e070.

  DATA: vl_strkorr  TYPE e070-strkorr.

* Try the global logic (Only for entries in TR with status D)
  CALL METHOD super->get_lock_entry.

  IF me->output-trkorr IS NOT INITIAL.
    RETURN.
  ENDIF.

  SELECT trkorr
  FROM e071
  INTO TABLE tl_trkorr
  WHERE pgmid      EQ 'R3TR'
    AND object     EQ me->output-object_type
    AND obj_name   EQ me->output-object_name.

  SELECT trkorr as4date as4user strkorr
  FROM e070
  INTO TABLE tl_trkorr
  FOR ALL ENTRIES IN tl_trkorr
  WHERE trkorr EQ tl_trkorr-trkorr.

  SORT tl_trkorr BY as4date DESCENDING.

  READ TABLE tl_trkorr INTO wl_trkorr INDEX 1.

* If was a task, get the TR
  IF wl_trkorr-strkorr IS NOT INITIAL.
    me->output-trkorr = wl_trkorr-strkorr.
  ELSE.
    me->output-trkorr = wl_trkorr-trkorr.
  ENDIF.

  me->output-as4date = wl_trkorr-as4date.
  me->output-as4user = wl_trkorr-as4user.

  SELECT SINGLE as4text
  FROM e07t
  INTO me->output-as4text
  WHERE trkorr EQ me->output-trkorr.

ENDMETHOD.


  METHOD get_object_content.

    DATA: tl_profiles       TYPE STANDARD TABLE OF user04,
          tl_authorizations TYPE STANDARD TABLE OF user012a.

    DATA: vl_agr_name TYPE agr_define-agr_name,
          vl_object   TYPE ust12-objct.

    vl_object = vl_agr_name = me->object-obj_name.

    CALL FUNCTION 'PRGN_GET_PROFILES_OF_ROLE_RFC'
      DESTINATION server
      EXPORTING
        agr_name        = vl_agr_name
      TABLES
        profile         = tl_profiles
      EXCEPTIONS
        role_not_exists = 1
        no_authority    = 2
        OTHERS          = 3.

    IF sy-subrc NE 0.
      me->raise_system_exception( ).
    ENDIF.

    CALL FUNCTION 'AUTHORIZATION_DATA_READ_RFC'
      DESTINATION server
      EXPORTING
        object                = vl_object
      TABLES
        profiles              = tl_profiles
        authorizations        = tl_authorizations
      EXCEPTIONS
        client_not_exist      = 1
        system_not_accessible = 2
        system_not_exist      = 3
        wrong_input           = 4
        profile_not_exist     = 5
        no_authority          = 6
        auth_not_exist        = 7
        OTHERS                = 8.

    IF sy-subrc NE 0.
      me->raise_system_exception( ).
    ENDIF.

    me->add_to_string( EXPORTING input = tl_authorizations CHANGING output = object_content ).

  ENDMETHOD.


METHOD get_object_list.

    DATA: wl_list TYPE LINE OF devtyrange.

    wl_list-sign   = 'I'.
    wl_list-option = 'EQ'.
    wl_list-low    = 'ACGR'.
    APPEND wl_list TO list.

  ENDMETHOD.


  METHOD get_object_type_text.

    DATA: tl_in  TYPE STANDARD TABLE OF ko105,
          tl_out TYPE STANDARD TABLE OF ko100.

    DATA: wl_in  TYPE ko105,
          wl_out TYPE ko100.

    wl_in-pgmid  = 'R3TR'.
    wl_in-object = me->object-object.
    APPEND wl_in TO tl_in.

    CALL FUNCTION 'TRINT_OBJECT_TABLE'
      TABLES
        tt_types_in  = tl_in
        tt_types_out = tl_out.

    READ TABLE tl_out INTO wl_out INDEX 1.

    description = wl_out-text.

  ENDMETHOD.
ENDCLASS.
