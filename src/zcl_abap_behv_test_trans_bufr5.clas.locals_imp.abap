class lcl_abap_behv_msg definition create public inheriting from cx_no_check.
  public section.

    interfaces if_abap_behv_message .

    aliases msgty
      for if_t100_dyn_msg~msgty .
    aliases msgv1
      for if_t100_dyn_msg~msgv1 .
    aliases msgv2
      for if_t100_dyn_msg~msgv2 .
    aliases msgv3
      for if_t100_dyn_msg~msgv3 .
    aliases msgv4
      for if_t100_dyn_msg~msgv4 .

    methods constructor
      importing
        !textid   like if_t100_message=>t100key optional
        !previous like previous optional
        !msgty    type symsgty optional
        !msgv1    type simple optional
        !msgv2    type simple optional
        !msgv3    type simple optional
        !msgv4    type simple optional .

endclass.


class lcl_abap_behv_msg implementation.

  method constructor.
    call method super->constructor exporting previous = previous.
    me->msgty = msgty .
    me->msgv1 = msgv1 .
    me->msgv2 = msgv2 .
    me->msgv3 = msgv3 .
    me->msgv4 = msgv4 .
    clear me->textid.
    if textid is initial.
      if_t100_message~t100key = if_t100_message=>default_textid.
    else.
      if_t100_message~t100key = textid.
    endif.
  endmethod.

endclass.

*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
class lcl_exception_handler definition create public.

  public section.
    methods new_message
      importing
        !id        type symsgid
        !number    type symsgno
        !severity  type if_abap_behv_message=>t_severity
        !v1        type simple optional
        !v2        type simple optional
        !v3        type simple optional
        !v4        type simple optional
      returning
        value(obj) type ref to if_abap_behv_message .
    constants ms like if_abap_behv_message=>severity value if_abap_behv_message=>severity ##NO_TEXT.
    constants mc like if_abap_behv=>cause value if_abap_behv=>cause ##NO_TEXT.
  protected section.
  private section.

endclass.

class lcl_exception_handler implementation.

  method new_message.

    obj = new lcl_abap_behv_msg(
      textid = value #(
                 msgid = id
                 msgno = number
                 attr1 = cond #( when v1 is not initial then 'IF_T100_DYN_MSG~MSGV1' )
                 attr2 = cond #( when v2 is not initial then 'IF_T100_DYN_MSG~MSGV2' )
                 attr3 = cond #( when v3 is not initial then 'IF_T100_DYN_MSG~MSGV3' )
                 attr4 = cond #( when v4 is not initial then 'IF_T100_DYN_MSG~MSGV4' )
      )
      msgty = switch #( severity
                when ms-error       then 'E'
                when ms-warning     then 'W'
                when ms-information then 'I'
                when ms-success     then 'S' )
      msgv1 = |{ v1 }|
      msgv2 = |{ v2 }|
      msgv3 = |{ v3 }|
      msgv4 = |{ v4 }|
    ).

    obj->m_severity = severity.


  endmethod.

endclass.
