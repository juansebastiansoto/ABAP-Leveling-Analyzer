SELECTION-SCREEN BEGIN OF BLOCK b10 WITH FRAME TITLE TEXT-b10.

PARAMETERS: p_devcl TYPE tadir-devclass,
            p_trk   TYPE e070-trkorr.

PARAMETERS: p_obtyp TYPE tadir-object.

SELECT-OPTIONS: s_obnam FOR w_screen-obj_name.

SELECTION-SCREEN END OF BLOCK b10.

SELECTION-SCREEN BEGIN OF BLOCK b90 WITH FRAME TITLE TEXT-b90.

PARAMETERS: ck_out AS CHECKBOX DEFAULT 'X',
            ck_hok AS CHECKBOX DEFAULT 'X'.

SELECT-OPTIONS: s_notr  FOR w_screen-trkorr,
                s_mails FOR w_screen-smtp_address NO INTERVALS.

SELECTION-SCREEN END OF BLOCK b90.
