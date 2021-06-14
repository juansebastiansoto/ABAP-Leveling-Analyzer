class YCL_BC_LEVELING_CLAS definition
  public
  inheriting from YCL_BC_LEVELING
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
private section.

  methods GET_OBJECT_CONTENT_CLSD
    importing
      !SERVER type RFCDEST optional
      !VERSNO type VRSD-VERSNO
    returning
      value(OBJECT_CONTENT) type STRING
    raising
      CX_T100_MSG .
  methods GET_OBJECT_CONTENT_CPRI
    importing
      !SERVER type RFCDEST optional
      !VERSNO type VRSD-VERSNO
    returning
      value(OBJECT_CONTENT) type STRING
    raising
      CX_T100_MSG .
  methods GET_OBJECT_CONTENT_CPRO
    importing
      !SERVER type RFCDEST optional
      !VERSNO type VRSD-VERSNO
    returning
      value(OBJECT_CONTENT) type STRING
    raising
      CX_T100_MSG .
  methods GET_OBJECT_CONTENT_CPUB
    importing
      !SERVER type RFCDEST optional
      !VERSNO type VRSD-VERSNO
    returning
      value(OBJECT_CONTENT) type STRING
    raising
      CX_T100_MSG .
  methods MOVE_CHANGED_DATA
    importing
      !IM_DATA type ANY .
ENDCLASS.



CLASS YCL_BC_LEVELING_CLAS IMPLEMENTATION.


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

    object_type = 'CLAS'.

  ENDMETHOD.


METHOD existence_check.

    CALL FUNCTION 'RPY_EXISTENCE_CHECK_CLAS'
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

    object_content = me->get_object_content_clsd( server = server
                                                  versno = versno ).

    object_content = me->get_object_content_cpri( server = server
                                                  versno = versno ).

    object_content = me->get_object_content_cpro( server = server
                                                  versno = versno ).

    object_content = me->get_object_content_cpub( server = server
                                                  versno = versno ).


  ENDMETHOD.


METHOD get_object_content_clsd.

    DATA: tl_seoclas    TYPE STANDARD TABLE OF vseoclass,
          tl_seometarel TYPE STANDARD TABLE OF seometarel,
          tl_source     TYPE STANDARD TABLE OF abaptxt255.

    DATA: vl_object_name TYPE vrsd-objname.

    FIELD-SYMBOLS: <fsl_seoclas>    TYPE vseoclass,
                   <fsl_seometarel> TYPE seometarel.

    vl_object_name = me->object-obj_name.

    CALL FUNCTION 'SVRS_GET_VERSION_CLSD_40'
      DESTINATION server
      EXPORTING
        object_name           = vl_object_name
        versno                = versno
      TABLES
        pvseoclass            = tl_seoclas
        pseometarel           = tl_seometarel
        psource               = tl_source
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS                = 4.

    IF sy-subrc NE 0.
      me->raise_system_exception( ).
    ENDIF.

    CALL FUNCTION 'CONVERT_TABLE_TO_STRING'
      EXPORTING
        i_tabline_length = 255
      IMPORTING
        e_string         = object_content
      TABLES
        it_table         = tl_source.

    LOOP AT tl_seometarel ASSIGNING <fsl_seometarel>.
      CLEAR: <fsl_seometarel>-author,    <fsl_seometarel>-createdon,
             <fsl_seometarel>-changedby, <fsl_seometarel>-changedon.
    ENDLOOP.

    me->add_to_string( EXPORTING input = tl_seometarel CHANGING output = object_content ).

  ENDMETHOD.


METHOD get_object_content_cpri.

    DATA: tl_attrib TYPE STANDARD TABLE OF vseoattrib_vrs,
          tl_event  TYPE STANDARD TABLE OF vseoevent,
          tl_excep  TYPE STANDARD TABLE OF vseoexcep,
          tl_method TYPE STANDARD TABLE OF vseomethod,
          tl_param  TYPE STANDARD TABLE OF vseoparam,
          tl_source TYPE STANDARD TABLE OF abaptxt255.

    DATA: vl_object_name        TYPE vrsd-objname.

    FIELD-SYMBOLS: <fsl_attrib> TYPE vseoattrib_vrs,
                   <fsl_event>  TYPE vseoevent,
                   <fsl_excep>  TYPE vseoexcep,
                   <fsl_method> TYPE vseomethod,
                   <fsl_param>  TYPE vseoparam.

    vl_object_name = me->object-obj_name.

    CALL FUNCTION 'SVRS_GET_VERSION_CPRI_40'
      DESTINATION server
      EXPORTING
        object_name           = vl_object_name
        versno                = versno
      TABLES
        pvseoattrib           = tl_attrib
        pvseoevent            = tl_event
        pvseoexcep            = tl_excep
        pvseomethod           = tl_method
        pvseoparam            = tl_param
        psource               = tl_source
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS                = 4.

    IF sy-subrc NE 0.
      me->raise_system_exception( ).
    ENDIF.

    CALL FUNCTION 'CONVERT_TABLE_TO_STRING'
      EXPORTING
        i_tabline_length = 255
      IMPORTING
        e_string         = object_content
      TABLES
        it_table         = tl_source.

    LOOP AT tl_attrib ASSIGNING <fsl_attrib>.
      CLEAR: <fsl_attrib>-author,    <fsl_attrib>-createdon,
             <fsl_attrib>-changedby, <fsl_attrib>-changedon,
             <fsl_attrib>-r3release, <fsl_attrib>-srcrow1,
             <fsl_attrib>-srcrow2,   <fsl_attrib>-srccolumn2.
    ENDLOOP.

    LOOP AT tl_event ASSIGNING <fsl_event>.
      CLEAR: <fsl_event>-author,    <fsl_event>-createdon,
             <fsl_event>-changedby, <fsl_event>-changedon.
    ENDLOOP.

    LOOP AT tl_excep ASSIGNING <fsl_excep>.
      CLEAR: <fsl_excep>-author,    <fsl_excep>-createdon,
             <fsl_excep>-changedby, <fsl_excep>-changedon.
    ENDLOOP.

    LOOP AT tl_method ASSIGNING <fsl_method>.
      CLEAR: <fsl_method>-author,    <fsl_method>-createdon,
             <fsl_method>-changedby, <fsl_method>-changedon,
             <fsl_method>-r3release.
    ENDLOOP.

    LOOP AT tl_param ASSIGNING <fsl_param>.
      CLEAR: <fsl_param>-author,    <fsl_param>-createdon,
             <fsl_param>-changedby, <fsl_param>-changedon.
    ENDLOOP.

    me->add_to_string( EXPORTING input = tl_attrib CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_event CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_excep CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_method CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_param CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_source CHANGING output = object_content ).

  ENDMETHOD.


METHOD get_object_content_cpro.

    DATA: tl_attrib TYPE STANDARD TABLE OF vseoattrib_vrs,
          tl_event  TYPE STANDARD TABLE OF vseoevent,
          tl_excep  TYPE STANDARD TABLE OF vseoexcep,
          tl_method TYPE STANDARD TABLE OF vseomethod,
          tl_param  TYPE STANDARD TABLE OF vseoparam,
          tl_redef  TYPE STANDARD TABLE OF seoredef,
          tl_source TYPE STANDARD TABLE OF abaptxt255.

    DATA: vl_object_name        TYPE vrsd-objname.

    FIELD-SYMBOLS: <fsl_attrib> TYPE vseoattrib_vrs,
                   <fsl_event>  TYPE vseoevent,
                   <fsl_excep>  TYPE vseoexcep,
                   <fsl_method> TYPE vseomethod,
                   <fsl_param>  TYPE vseoparam.

    vl_object_name = me->object-obj_name.

    CALL FUNCTION 'SVRS_GET_VERSION_CPRO_40'
      DESTINATION server
      EXPORTING
        object_name           = vl_object_name
        versno                = versno
      TABLES
        pvseoattrib           = tl_attrib
        pvseoevent            = tl_event
        pvseoexcep            = tl_excep
        pvseomethod           = tl_method
        pvseoparam            = tl_param
        pseoredef             = tl_redef
        psource               = tl_source
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS                = 4.

    IF sy-subrc NE 0.
      me->raise_system_exception( ).
    ENDIF.

    LOOP AT tl_attrib ASSIGNING <fsl_attrib>.
      CLEAR: <fsl_attrib>-author,    <fsl_attrib>-createdon,
             <fsl_attrib>-changedby, <fsl_attrib>-changedon,
             <fsl_attrib>-r3release, <fsl_attrib>-srcrow1,
             <fsl_attrib>-srcrow2,   <fsl_attrib>-srccolumn2.
    ENDLOOP.

    LOOP AT tl_event ASSIGNING <fsl_event>.
      CLEAR: <fsl_event>-author,    <fsl_event>-createdon,
             <fsl_event>-changedby, <fsl_event>-changedon.
    ENDLOOP.

    LOOP AT tl_excep ASSIGNING <fsl_excep>.
      CLEAR: <fsl_excep>-author,    <fsl_excep>-createdon,
             <fsl_excep>-changedby, <fsl_excep>-changedon.
    ENDLOOP.

    LOOP AT tl_method ASSIGNING <fsl_method>.
      CLEAR: <fsl_method>-author,    <fsl_method>-createdon,
             <fsl_method>-changedby, <fsl_method>-changedon,
             <fsl_method>-r3release.
    ENDLOOP.

    LOOP AT tl_param ASSIGNING <fsl_param>.
      CLEAR: <fsl_param>-author,    <fsl_param>-createdon,
             <fsl_param>-changedby, <fsl_param>-changedon.
    ENDLOOP.

    me->add_to_string( EXPORTING input = tl_source CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_attrib CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_event  CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_excep  CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_method CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_param  CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_redef  CHANGING output = object_content ).

  ENDMETHOD.


METHOD get_object_content_cpub.

    DATA: tl_attrib  TYPE STANDARD TABLE OF vseoattrib_vrs,
          tl_event   TYPE STANDARD TABLE OF vseoevent,
          tl_excep   TYPE STANDARD TABLE OF vseoexcep,
          tl_implrel TYPE STANDARD TABLE OF seoimplrel,
          tl_method  TYPE STANDARD TABLE OF vseomethod,
          tl_metarel TYPE STANDARD TABLE OF seometarel,
          tl_param   TYPE STANDARD TABLE OF vseoparam,
          tl_redef   TYPE STANDARD TABLE OF seoredef,
          tl_source  TYPE STANDARD TABLE OF abaptxt255.

    DATA: vl_object_name        TYPE vrsd-objname.

    FIELD-SYMBOLS: <fsl_attrib>  TYPE vseoattrib_vrs,
                   <fsl_event>   TYPE vseoevent,
                   <fsl_excep>   TYPE vseoexcep,
                   <fsl_method>  TYPE vseomethod,
                   <fsl_metarel> TYPE seometarel,
                   <fsl_param>   TYPE vseoparam.

    vl_object_name = me->object-obj_name.

    CALL FUNCTION 'SVRS_GET_VERSION_CPUB_40'
      DESTINATION server
      EXPORTING
        object_name           = vl_object_name
        versno                = versno
      TABLES
        pvseoattrib           = tl_attrib
        pvseoevent            = tl_event
        pvseoexcep            = tl_excep
        pseoimplrel           = tl_implrel
        pvseomethod           = tl_method
        pseometarel           = tl_metarel
        pvseoparam            = tl_param
        pseoredef             = tl_redef
        psource               = tl_source
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS                = 4.

    IF sy-subrc NE 0.
      me->raise_system_exception( ).
    ENDIF.

    LOOP AT tl_attrib ASSIGNING <fsl_attrib>.
      CLEAR: <fsl_attrib>-author,    <fsl_attrib>-createdon,
             <fsl_attrib>-changedby, <fsl_attrib>-changedon,
             <fsl_attrib>-r3release, <fsl_attrib>-srcrow1,
             <fsl_attrib>-srcrow2,   <fsl_attrib>-srccolumn2.
    ENDLOOP.

    LOOP AT tl_event ASSIGNING <fsl_event>.
      CLEAR: <fsl_event>-author,    <fsl_event>-createdon,
             <fsl_event>-changedby, <fsl_event>-changedon.
    ENDLOOP.

    LOOP AT tl_excep ASSIGNING <fsl_excep>.
      CLEAR: <fsl_excep>-author,    <fsl_excep>-createdon,
             <fsl_excep>-changedby, <fsl_excep>-changedon.
    ENDLOOP.

    LOOP AT tl_method ASSIGNING <fsl_method>.
      CLEAR: <fsl_method>-author,    <fsl_method>-createdon,
             <fsl_method>-changedby, <fsl_method>-changedon,
             <fsl_method>-r3release.
    ENDLOOP.

    LOOP AT tl_param ASSIGNING <fsl_param>.
      CLEAR: <fsl_param>-author,    <fsl_param>-createdon,
             <fsl_param>-changedby, <fsl_param>-changedon.
    ENDLOOP.

    LOOP AT tl_metarel ASSIGNING <fsl_metarel>.
      CLEAR: <fsl_metarel>-author,    <fsl_metarel>-createdon,
             <fsl_metarel>-changedby, <fsl_metarel>-changedon.
    ENDLOOP.

    me->add_to_string( EXPORTING input = tl_attrib  CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_event   CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_excep   CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_implrel CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_method  CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_metarel CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_param   CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_redef   CHANGING output = object_content ).
    me->add_to_string( EXPORTING input = tl_source  CHANGING output = object_content ).

  ENDMETHOD.


METHOD get_object_list.

    DATA: wl_list TYPE LINE OF devtyrange.

    wl_list-sign   = 'I'.
    wl_list-option = 'EQ'.
    wl_list-low    = 'CLAS'.
    APPEND wl_list TO list.

    wl_list-low    = 'CLSD'.
    APPEND wl_list TO list.

  ENDMETHOD.


METHOD move_changed_data.

    FIELD-SYMBOLS: <fsl_user> TYPE sy-uname,
                   <fsl_date> TYPE d.

    ASSIGN COMPONENT 'CHANGEDBY' OF STRUCTURE im_data TO <fsl_user>.
    ASSIGN COMPONENT 'CHANGEDON' OF STRUCTURE im_data TO <fsl_date>.

    IF me->output-as4date LT <fsl_date>.

      me->output-as4date = <fsl_date>.

      IF <fsl_user> IS INITIAL.

        UNASSIGN <fsl_user>.
        ASSIGN COMPONENT 'AUTHOR' OF STRUCTURE im_data TO <fsl_user>.

      ENDIF.

      me->output-as4user = <fsl_user>.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
