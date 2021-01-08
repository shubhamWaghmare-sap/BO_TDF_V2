class zcx_bo_tdf_failure definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

  public section.
  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  data:
        preconfigured TYPE CHAR10 ,
        new_configuration TYPE CHAR10 ,
        instance TYPE CHAR10,
        message TYPE string .

    constants:
    BEGIN OF entity_not_found,
        msgid TYPE symsgid VALUE 'ZBO_MOCKING_MESSAGE',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF entity_not_found .
    constants:
    BEGIN OF incorrect_response_config ,
        msgid TYPE symsgid VALUE 'ZBO_MOCKING_MESSAGE',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'preconfigured',
        attr2 TYPE scx_attrname VALUE 'new_configuration',
        attr3 TYPE scx_attrname VALUE 'instance',
        attr4 TYPE scx_attrname VALUE '',
      END OF incorrect_response_config .
    constants:
    BEGIN OF entity_not_recorded ,
        msgid TYPE symsgid VALUE 'ZBO_MOCKING_MESSAGE',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'new_configuration',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF entity_not_recorded .
    constants:
    BEGIN OF unauthorized_action ,
        msgid TYPE symsgid VALUE 'ZBO_MOCKING_MESSAGE',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'message',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF unauthorized_action .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !preconfigured TYPE CHAR10 OPTIONAL
        !new_configuration TYPE CHAR10 OPTIONAL
        !instance TYPE CHAR10 OPTIONAL
        !message TYPE string OPTIONAL .

  protected section.
  private section.
endclass.



class zcx_bo_tdf_failure implementation.
  method constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).
    me->preconfigured = preconfigured.
    me->instance = instance.
    me->new_configuration = new_configuration.
    me->message = message.

    if textid is initial.
      if_t100_message~t100key = zcx_bo_tdf_failure=>entity_not_found.
    else.
      if_t100_message~t100key = textid.
    endif.

  endmethod.

endclass.
