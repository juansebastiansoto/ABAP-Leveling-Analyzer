class YCL_BC_LEVELING_FUGR definition
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
  methods GET_OBJECT_CONTENT
    redefinition .
  methods GET_LOCK_ENTRY
    redefinition .
private section.
ENDCLASS.



CLASS YCL_BC_LEVELING_FUGR IMPLEMENTATION.


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

  object_type = 'FUGR'.

ENDMETHOD.


METHOD existence_check.

  CALL FUNCTION 'RPY_EXISTENCE_CHECK_FUGR'
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

    DATA: vl_obj_name TYPE e071-obj_name.

    CALL METHOD super->get_lock_entry.

    IF me->output-trkorr IS NOT INITIAL.
      RETURN.
    ENDIF.

    CONCATENATE 'SAPL'
                me->object-obj_name
    INTO vl_obj_name.

    SELECT SINGLE korrnum author
    FROM vrsd
    INTO (me->output-trkorr, me->output-as4user)
    WHERE objtype EQ 'REPS'
      AND objname EQ vl_obj_name
      AND versno  EQ '00000'.

    SELECT SINGLE as4text
    FROM e07t
    INTO me->output-as4text
    WHERE trkorr EQ me->output-trkorr.

    SELECT SINGLE as4date
    FROM e070
    INTO me->output-as4date
    WHERE trkorr EQ me->output-trkorr.

  ENDMETHOD.


METHOD get_object_content.

  DATA: tl_repos_tab   TYPE STANDARD TABLE OF abaptxt255.

  DATA: vl_object_name TYPE vrsd-objname,
        vl_group_name  TYPE rs38l-area,
        vl_pname       TYPE pname.

  FIELD-SYMBOLS: <fsl_repos_tab> TYPE abaptxt255.

  vl_group_name = me->object-obj_name.

  CALL FUNCTION 'GET_FUNCTION_MAIN_PROGRAM'
    EXPORTING
      group_name   = vl_group_name
    IMPORTING
      program_name = vl_pname
    EXCEPTIONS
      OTHERS       = 0.

  vl_object_name = vl_pname.

  CALL FUNCTION 'SVRS_GET_VERSION_REPS_40'
    DESTINATION server
    EXPORTING
      object_name           = vl_object_name
      versno                = versno
    TABLES
      repos_tab             = tl_repos_tab
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3
      OTHERS                = 4.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

* Delete generation header comment
  LOOP AT tl_repos_tab ASSIGNING <fsl_repos_tab>.
    IF <fsl_repos_tab>(1) EQ '*'.
      DELETE tl_repos_tab INDEX 1.
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.

  me->add_to_string( EXPORTING input = tl_repos_tab  CHANGING output = object_content ).

ENDMETHOD.


METHOD get_object_list.

  DATA: wl_list TYPE LINE OF devtyrange.

  wl_list-sign   = 'I'.
  wl_list-option = 'EQ'.
  wl_list-low    = 'FUGR'.
  APPEND wl_list TO list.

ENDMETHOD.
ENDCLASS.
