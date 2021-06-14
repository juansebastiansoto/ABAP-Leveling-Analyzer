class YCL_BC_LEVELING_ENHO definition
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
private section.
ENDCLASS.



CLASS YCL_BC_LEVELING_ENHO IMPLEMENTATION.


METHOD determine_text_object_code.

  object_type = 'ENHO'.

ENDMETHOD.


METHOD existence_check.

    CALL FUNCTION 'RPY_EXISTENCE_CHECK_ENHO'
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


METHOD get_object_content.

  DATA: tl_cinx         TYPE STANDARD TABLE OF enhincinx_vers,
        tl_repos        TYPE STANDARD TABLE OF abaptxt255,
        tl_include_list TYPE STANDARD TABLE OF abaptxt255.

  DATA: wl_cinx         TYPE enhincinx_vers,
        wl_include      TYPE abaptxt255.

  DATA: vl_object_name  TYPE vrsd-objname.

  vl_object_name = me->object-obj_name.

  CALL FUNCTION 'SVRS_GET_VERSION_ENHO_40'
    DESTINATION server
    EXPORTING
      object_name           = vl_object_name
      versno                = versno
    TABLES
      cinx_tab              = tl_cinx
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

  READ TABLE tl_cinx INTO wl_cinx INDEX 1.

  vl_object_name = wl_cinx-enhinclude.

  CALL FUNCTION 'SVRS_GET_VERSION_REPS_40'
    DESTINATION server
    EXPORTING
      object_name           = vl_object_name
      versno                = versno
    TABLES
      repos_tab             = tl_include_list
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

  me->add_to_string( EXPORTING input = tl_cinx         CHANGING output = object_content ).
  me->add_to_string( EXPORTING input = tl_include_list CHANGING output = object_content ).

* Still only the custom includes.
  DELETE tl_include_list WHERE line(9) NE 'INCLUDE Z'.

  LOOP AT tl_include_list INTO wl_include.

    CLEAR tl_repos.

    vl_object_name = wl_include-line+8(35).

    CALL FUNCTION 'SVRS_GET_VERSION_REPS_40'
      DESTINATION server
      EXPORTING
        object_name           = vl_object_name
        versno                = versno
      TABLES
        repos_tab             = tl_repos
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS                = 4.

    IF sy-subrc NE 0.
      me->raise_system_exception( ).
    ENDIF.

    me->add_to_string( EXPORTING input = tl_repos CHANGING output = object_content ).

  ENDLOOP.

ENDMETHOD.


METHOD get_object_list.

    DATA: wl_list TYPE LINE OF devtyrange.

    wl_list-sign   = 'I'.
    wl_list-option = 'EQ'.
    wl_list-low    = 'ENHO'.
    APPEND wl_list TO list.

    wl_list-low    = 'ENHS'.
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
