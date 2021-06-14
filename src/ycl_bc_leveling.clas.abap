class YCL_BC_LEVELING definition
  public
  abstract
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !OBJECT type CTS_OBJECT_KEY
    raising
      CX_T100_MSG .
  class-methods GET_INSTANCE
    importing
      !OBJECT type CTS_OBJECT_KEY
    returning
      value(INSTANCE) type ref to YCL_BC_LEVELING
    raising
      CX_SY_CREATE_OBJECT_ERROR
      CX_T100_MSG .
  methods GET_OBJECT_LIST
  abstract
    returning
      value(LIST) type DEVTYRANGE .
  methods ANALYZE
    raising
      CX_T100_MSG
      CX_CTS_OBJECT_NOT_FOUND .
  methods GET_RESULT
    returning
      value(RESULT) type YBCES0003 .
protected section.

  data OUTPUT type YBCES0003 .
  data DESTINATIONS type YBCES0004 .
  data OBJECT type CTS_OBJECT_KEY .

  methods EXISTENCE_CHECK
  abstract
    importing
      !SERVER type RFCDEST optional
    returning
      value(EXIST) type BOOLEAN .
  methods CALCULATE_HASH
  final
    importing
      !INPUT type STRING
    returning
      value(RESULT) type HASH160
    raising
      CX_T100_MSG .
  methods RAISE_SYSTEM_EXCEPTION
    raising
      CX_T100_MSG .
  methods GET_OBJECT_TYPE_TEXT
    returning
      value(DESCRIPTION) type DDTEXT .
  methods GET_OBJECT_CONTENT
  abstract
    importing
      !SERVER type RFCDEST optional
      !VERSNO type VRSD-VERSNO
    returning
      value(OBJECT_CONTENT) type STRING
    raising
      CX_T100_MSG .
  methods DETERMINE_TEXT_OBJECT_CODE
  abstract
    returning
      value(OBJECT_TYPE) type SEU_OBJ .
  methods ADD_TO_STRING
    importing
      !INPUT type ANY TABLE
    changing
      !OUTPUT type STRING .
  methods GET_LOCK_ENTRY .
private section.

  methods READ_DESTINATIONS
    raising
      CX_T100_MSG .
  methods CONVERT_LIMU_TO_R3TR
    changing
      !CH_OBJECT type TROBJTYPE
      !CH_OBJNAME type TROBJ_NAME .
  methods SAVE_LOG
    importing
      !DATA type STRING
      !SERVER type RFCDEST
      !HASH type HASH160 .
ENDCLASS.



CLASS YCL_BC_LEVELING IMPLEMENTATION.


METHOD add_to_string.

  DATA: tl_stringtab  TYPE stringtab.

  DATA: vl_stringline TYPE string.

  CALL METHOD zcl_ca_file_utilities=>standardtab_to_stringtab
    EXPORTING
      input    = input
      splitter = cl_abap_char_utilities=>horizontal_tab
    IMPORTING
      output   = tl_stringtab.

  LOOP AT tl_stringtab INTO vl_stringline.

    IF output IS INITIAL.

      output = vl_stringline.

    ELSE.

      CONCATENATE output
                  vl_stringline
      INTO output
      SEPARATED BY cl_abap_char_utilities=>newline.

    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD analyze.

  DATA: vl_content  TYPE string,
        vl_hash_dev TYPE hash160,
        vl_hash_qa  TYPE hash160,
        vl_hash_prd TYPE hash160,
        vl_msgv1    TYPE symsgv,
        vl_msgv2    TYPE symsgv,
        vl_descr    TYPE ddtext.

* Development Version
  IF me->existence_check( ) EQ abap_true.

    vl_content  = me->get_object_content( versno = 00000 ).
    vl_hash_dev = me->calculate_hash( vl_content ).

    me->save_log( EXPORTING data = vl_content
                          server = ''
                            hash = vl_hash_dev ).

  ELSE.

    vl_msgv1 = vl_descr = me->get_object_type_text( ).

    vl_msgv2 = me->object-obj_name.

    RAISE EXCEPTION TYPE cx_cts_object_not_found
      EXPORTING
        message_variable_1 = vl_msgv1
        message_variable_2 = vl_msgv2.

  ENDIF.

* Fill output
  me->output-object_type = me->object-object.
  me->output-object_name = me->object-obj_name.

* Quality Version
  IF me->existence_check( me->destinations-qa_rfc ) EQ abap_true.

    vl_content = me->get_object_content( server = me->destinations-qa_rfc
                                         versno = 99998 ).
    vl_hash_qa = me->calculate_hash( vl_content ).

    me->save_log( EXPORTING data = vl_content
                          server = me->destinations-qa_rfc
                            hash = vl_hash_qa ).

    IF vl_hash_dev EQ vl_hash_qa.
      me->output-qa_status = '@01@'.
    ELSE.
      me->output-qa_status = '@02@'.
    ENDIF.

  ELSE.

    me->output-qa_status = '@03@'.

  ENDIF.

* Production Version
  IF me->existence_check( me->destinations-prd_rfc ) EQ abap_true.

    vl_content  = me->get_object_content( server = me->destinations-prd_rfc
                                          versno = 99998 ).
    vl_hash_prd = me->calculate_hash( vl_content ).

    me->save_log( EXPORTING data = vl_content
                          server = me->destinations-prd_rfc
                            hash = vl_hash_prd ).

    IF vl_hash_dev EQ vl_hash_prd.
      me->output-prd_status = '@01@'.
    ELSE.
      me->output-prd_status = '@02@'.
    ENDIF.

  ELSE.

    me->output-prd_status = '@03@'.

  ENDIF.

  me->get_lock_entry( ).

ENDMETHOD.


METHOD calculate_hash.

  DATA: vl_xstring TYPE xstring,
        vl_msgv1   TYPE string,
        vl_msgv2   TYPE string,
        vl_msgv3   TYPE string,
        vl_msgv4   TYPE string.

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = input
    IMPORTING
      buffer = vl_xstring
    EXCEPTIONS
      failed = 1
      OTHERS = 2.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

  CALL FUNCTION 'CALCULATE_HASH_FOR_RAW'
    EXPORTING
      data           = vl_xstring
    IMPORTING
      hash           = result
    EXCEPTIONS
      unknown_alg    = 1
      param_error    = 2
      internal_error = 3
      OTHERS         = 4.

  IF sy-subrc NE 0.
    me->raise_system_exception( ).
  ENDIF.

ENDMETHOD.


METHOD constructor.

  me->object = object.

  me->read_destinations( ).

ENDMETHOD.


METHOD convert_limu_to_r3tr.

    CALL FUNCTION 'GET_R3TR_OBJECT_FROM_LIMU_OBJ'
      EXPORTING
        p_limu_objtype = ch_object
        p_limu_objname = ch_objname
      IMPORTING
        p_r3tr_objtype = ch_object
        p_r3tr_objname = ch_objname
      EXCEPTIONS
        OTHERS         = 0.

  ENDMETHOD.


METHOD get_instance.

  STATICS: tl_classlist TYPE seo_inheritances.

  DATA: rl_plugins   TYPE devtyrange.

  DATA: wl_classline TYPE vseoextend.

  DATA: vl_exist  TYPE boolean.

  IF tl_classlist IS INITIAL.
    SELECT *
    FROM vseoextend
    INTO TABLE tl_classlist
    WHERE refclsname LIKE 'YCL_BC_LEVELING%'
      AND version = '1'.
  ENDIF.

  LOOP AT tl_classlist INTO wl_classline.

    CREATE OBJECT instance
      TYPE
        (wl_classline-clsname)
      EXPORTING
        object                 = object.

    IF  instance IS BOUND
    AND object-object IN instance->get_object_list( ).
      vl_exist = abap_true.
      EXIT.
    ENDIF.

  ENDLOOP.

  IF vl_exist EQ abap_false.

    RAISE EXCEPTION TYPE cx_sy_create_object_error.

  ENDIF.

ENDMETHOD.


METHOD get_lock_entry.

  DATA: tl_locks TYPE STANDARD TABLE OF tlock.

  DATA: rl_object TYPE devtyrange.

  DATA: wl_e071       TYPE e071,
        wl_locks      TYPE tlock.

  wl_e071-pgmid    = me->object-pgmid.
  wl_e071-object   = me->object-object.
  wl_e071-obj_name = me->object-obj_name.

*  Get the lock transport request
  CALL FUNCTION 'TR_SHOW_OBJECT_LOCKS'
    EXPORTING
      iv_dialog           = abap_false
      iv_e071             = wl_e071
    TABLES
      it_locks            = tl_locks
    EXCEPTIONS
      object_not_lockable = 1
      empty_key           = 2
      unknown_object      = 3
      unallowed_locks     = 4
      OTHERS              = 5.

* If the object is not locked, read the versioning.
  IF sy-subrc NE 0
  OR tl_locks IS INITIAL.

    rl_object = me->get_object_list( ).

    SELECT SINGLE korrnum author
    FROM vrsd
    INTO (me->output-trkorr, me->output-as4user)
    WHERE objtype IN rl_object
      AND objname EQ me->object-obj_name
      AND versno  EQ '00000'.

  ELSE.

    READ TABLE tl_locks INTO wl_locks INDEX 1.

    me->output-trkorr  = wl_locks-trkorr.
    me->output-as4user = wl_locks-author.

  ENDIF.

  SELECT SINGLE as4text
  FROM e07t
  INTO me->output-as4text
  WHERE trkorr EQ me->output-trkorr.

  SELECT SINGLE as4date
  FROM e070
  INTO me->output-as4date
  WHERE trkorr EQ me->output-trkorr.

ENDMETHOD.


METHOD get_object_type_text.

  DATA: vl_object_type TYPE seu_obj.

  vl_object_type = me->determine_text_object_code( ).

  SELECT SINGLE singular
  FROM euobjall
  INTO description
  WHERE id    EQ vl_object_type
    AND spras EQ sy-langu.

ENDMETHOD.


METHOD get_result.

  result = me->output.

ENDMETHOD.


METHOD raise_system_exception.

  DATA: vl_msgv1  TYPE string,
        vl_msgv2  TYPE string,
        vl_msgv3  TYPE string,
        vl_msgv4  TYPE string.

  vl_msgv1 = sy-msgv1.
  vl_msgv2 = sy-msgv2.
  vl_msgv3 = sy-msgv3.
  vl_msgv4 = sy-msgv4.

  RAISE EXCEPTION TYPE cx_t100_msg
    EXPORTING
      t100_msgid = sy-msgid
      t100_msgno = sy-msgno
      t100_msgv1 = vl_msgv1
      t100_msgv2 = vl_msgv2
      t100_msgv3 = vl_msgv3
      t100_msgv4 = vl_msgv4.

ENDMETHOD.


METHOD read_destinations.

  DATA: vl_low TYPE tvarvc-low.

* Read the module list
  SELECT SINGLE low
  FROM tvarvc
  INTO vl_low
  WHERE name EQ 'QA_RFC'
    AND type EQ 'P'
    AND numb EQ ''.

  IF sy-subrc NE 0.
    RAISE EXCEPTION TYPE cx_t100_msg
      EXPORTING
        t100_msgid = 'YBC001'
        t100_msgno = 000
        t100_msgv1 = 'QA_RFC'.
  ENDIF.

  me->destinations-qa_rfc = vl_low.

* Read the module list
  SELECT SINGLE low
  FROM tvarvc
  INTO vl_low
  WHERE name EQ 'PRD_RFC'
    AND type EQ 'P'
    AND numb EQ ''.

  IF sy-subrc NE 0.
    RAISE EXCEPTION TYPE cx_t100_msg
      EXPORTING
        t100_msgid = 'YBC001'
        t100_msgno = 000
        t100_msgv1 = 'PRD_RFC'.
  ENDIF.

  me->destinations-prd_rfc = vl_low.

ENDMETHOD.


METHOD save_log.

  LOG-POINT ID ybc_leveling_analyzer SUBKEY server FIELDS server hash data.

  BREAK-POINT ID ybc_leveling_analyzer.

ENDMETHOD.
ENDCLASS.
