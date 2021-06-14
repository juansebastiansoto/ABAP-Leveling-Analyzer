*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR  'TT_0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  PERFORM send_mail.
  PERFORM pbo_0100.

ENDMODULE.                 " PBO_0100  OUTPUT
