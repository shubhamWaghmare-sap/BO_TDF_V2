class zcl_abap_behv_conf_modify_res definition
  public
  final
  create public .

  public section.
    class-methods : configure_mapped_for_modify
      importing
        entity    type cl_abap_behv_load=>t_entity
        operation type if_abap_behv=>t_char01
        mapped    type data
      changing
        dbl_transactional_buffer_table type zif_abap_behv_test_trans_bufr6=>tt_double_transactional_buffer
      raising
        zcx_bo_tdf_failure.


    class-methods : configure_reported_for_modify
      importing
        entity    type cl_abap_behv_load=>t_entity
        operation type if_abap_behv=>t_char01
        reported  type data
      changing
        dbl_transactional_buffer_table type zif_abap_behv_test_trans_bufr6=>tt_double_transactional_buffer
      raising
        zcx_bo_tdf_failure.

    class-methods : configure_failed_for_modify
      importing
        entity    type cl_abap_behv_load=>t_entity
        operation type if_abap_behv=>t_char01
        failed  type data
      changing
        dbl_transactional_buffer_table type zif_abap_behv_test_trans_bufr6=>tt_double_transactional_buffer
      raising
        zcx_bo_tdf_failure.

    class-methods : find_failed_config_for_modify
        importing
          entity            type cl_abap_behv_load=>t_entity
          instance_to_check type any
          operation         type if_abap_behv=>t_char01
        changing
          buffer_4_entity   type zif_abap_behv_test_trans_bufr6=>ty_double_transactional_buffer
          c_failed          type data optional
        returning
          value(result)     type abap_bool.

     class-methods : find_mapped_config_for_modify
        importing
          entity            type cl_abap_behv_load=>t_entity
          instance_to_check type any
          operation         type if_abap_behv=>t_char01
        changing
          buffer_4_entity   type zif_abap_behv_test_trans_bufr6=>ty_double_transactional_buffer
          c_mapped          type data optional
        returning
          value(result)     type abap_bool.

     class-methods : find_reported_config_fr_modify
        importing
          entity            type cl_abap_behv_load=>t_entity
          instance_to_check type any
          operation         type if_abap_behv=>t_char01
        changing
          buffer_4_entity   type zif_abap_behv_test_trans_bufr6=>ty_double_transactional_buffer
          c_reported        type data optional
        returning
          value(result)     type abap_bool.

  protected section.
  private section.

endclass.



class zcl_abap_behv_conf_modify_res implementation.

  method configure_mapped_for_modify.
    field-symbols: <buffer_4_entity>  type zif_abap_behv_test_trans_bufr6=>ty_double_transactional_buffer.
    field-symbols: <entity_mapped>  type standard table.
    field-symbols: <entity_reported_instances> type standard table.
    field-symbols: <mapped_entity_to_be_filled> type standard table.
    field-symbols: <entity_buffer_instances> type standard table.
    field-symbols: <entity_buffer_instance>  type any.


    data(alias) = zcl_abap_behv_buf_util=>get_entity_alias( entity = entity ).
    assign component alias of structure mapped to <entity_mapped>.

    if <entity_mapped> is assigned and <entity_mapped> is not initial.
      loop at <entity_mapped> assigning field-symbol(<entity_mapped_instance>).
        if line_exists( dbl_transactional_buffer_table[ entity_name = entity-name ] ).
          assign dbl_transactional_buffer_table[ entity_name = entity-name  ] to <buffer_4_entity>.
          assign <buffer_4_entity>-entity_instances->* to <entity_buffer_instances>.
          data(entity_fields) = zcl_abap_behv_buf_util=>get_entity_fields( entity-name ).
          loop at <entity_buffer_instances> assigning <entity_buffer_instance>.
            data(equal) = zcl_abap_behv_buf_util=>check_instance_equal(
                                 exporting
                                   instance_1    = <entity_buffer_instance>
                                   instance_2    = <entity_mapped_instance>
                                   entity_fields = entity_fields
                             ).
            if equal = abap_true.
              read table <buffer_4_entity>-modify_response_config with key operation = operation assigning field-symbol(<response_config>).
              if  <response_config> is not assigned .
                "create an entry for the modify response config
                data mapped_config_for_op type zif_abap_behv_test_trans_bufr6=>ty_modify_response_config.
                create data mapped_config_for_op-mapped like mapped.
                mapped_config_for_op-operation = operation.

                assign mapped_config_for_op-mapped->* to field-symbol(<mapped_to_be_filled>).
                assign component alias of structure <mapped_to_be_filled> to <mapped_entity_to_be_filled>.
                append <entity_mapped_instance> to <mapped_entity_to_be_filled>.

                append mapped_config_for_op to <buffer_4_entity>-modify_response_config.
                exit.
              else.

                "check if failed is configured for the operation and instance
                if <response_config>-failed is not initial.
                  data(failed_already_configured) = find_failed_config_for_modify(
                                                 exporting
                                                   entity            = entity
                                                   instance_to_check = <entity_mapped_instance>
                                                   operation         = operation
                                                 changing
                                                   buffer_4_entity   = <buffer_4_entity>
                                               ).
                  if failed_already_configured eq abap_true.
                      "TODO: Exception reported is already configured. Configuring mapped for the same instance is undefined.
                      raise exception type zcx_bo_tdf_failure
                        exporting
                          textid = zcx_bo_tdf_failure=>incorrect_response_config
                          preconfigured = 'failed'
                          new_configuration = 'mapped'.
                    EXIT.
                  endif.
                endif.

                "check if reported is configured for the operation and instance
                if <response_config>-reported is not initial.
                  assign <response_config>-reported->* to field-symbol(<entity_reported>).
                  assign component alias of structure <entity_reported> to <entity_reported_instances>.
                  loop at <entity_reported_instances> assigning field-symbol(<entity_reported_instance>).
                    data(reported_already_configured) = zcl_abap_behv_buf_util=>check_instance_equal(
                                 exporting
                                   instance_1    = <entity_reported_instance>
                                   instance_2    = <entity_mapped_instance>
                                   entity_fields = entity_fields
                             ).
                    if reported_already_configured eq abap_true.
                      "TODO: Exception reported is already configured. Configuring mapped for the same instance is undefined.
                      raise exception type zcx_bo_tdf_failure
                        exporting
                          textid = zcx_bo_tdf_failure=>incorrect_response_config
                          preconfigured = 'reported'
                          new_configuration = 'mapped'.
                      EXIT.
                    endif.
                  endloop.
                  if reported_already_configured eq abap_true.
                    "TODO: Exception reported is already configured. Configuring mapped for the same instance is undefined.
                      raise exception type zcx_bo_tdf_failure
                        exporting
                          textid = zcx_bo_tdf_failure=>incorrect_response_config
                          preconfigured = 'reported'
                          new_configuration = 'mapped'.
                      EXIT.
                  endif.

                  "At this point it is clear that reported is not configured for the instance.
                  if <response_config>-mapped is initial.
                    create data <response_config>-mapped like mapped.
                    assign <response_config>-mapped->* to <mapped_to_be_filled>.
                    assign component alias of structure <mapped_to_be_filled> to <mapped_entity_to_be_filled>.
                    append <entity_mapped_instance> to <mapped_entity_to_be_filled>.
                    exit.
                  else.
                    "TODO: Handle duplicate entries
                    assign <response_config>-mapped->* to <mapped_to_be_filled>.
                    assign component alias of structure <mapped_to_be_filled> to <mapped_entity_to_be_filled>.
                    append <entity_mapped_instance> to <mapped_entity_to_be_filled>.
                    exit.
                  endif.

                else.
                  "At this point it is clear that reported is not configured for the instance.
                  if <response_config>-mapped is initial.
                    create data <response_config>-mapped like mapped.
                    assign <response_config>-mapped->* to <mapped_to_be_filled>.
                    assign component alias of structure <mapped_to_be_filled> to <mapped_entity_to_be_filled>.
                    append <entity_mapped_instance> to <mapped_entity_to_be_filled>.
                    exit.
                  else.
                    "TODO: Handle duplicate entries
                    assign <response_config>-mapped->* to <mapped_to_be_filled>.
                    assign component alias of structure <mapped_to_be_filled> to <mapped_entity_to_be_filled>.
                    append <entity_mapped_instance> to <mapped_entity_to_be_filled>.
                    exit.
                  endif.

                endif.
              endif.
            else.
              "TODO: Throw exception with a reason that  "no buffer entry exists for the entity instance"
              " Record the instance for which the response is to be configured.
              raise exception type zcx_bo_tdf_failure
                exporting
                  textid = zcx_bo_tdf_failure=>entity_not_recorded
                  new_configuration = 'mapped'.
              EXIT.

            endif.
          endloop.
        else.
          "TODO: Throw exception with a reason that  "no buffer exists for the entity"
          " Record the instance for which the response is to be configured.
          raise exception type zcx_bo_tdf_failure
            exporting
              textid = zcx_bo_tdf_failure=>entity_not_recorded
              new_configuration = 'mapped'.
          EXIT.

        endif.
      endloop.
    endif.

  endmethod.

  method configure_reported_for_modify.
    field-symbols: <buffer_4_entity>  type zif_abap_behv_test_trans_bufr6=>ty_double_transactional_buffer.
    field-symbols: <entity_mapped_instances>  type standard table.
    field-symbols: <entity_reported> type standard table.
    field-symbols: <reported_entity_to_be_filled> type standard table.
    field-symbols: <entity_buffer_instances> type standard table.
    field-symbols: <entity_buffer_instance>  type any.


    data(alias) = zcl_abap_behv_buf_util=>get_entity_alias( entity = entity ).
    assign component alias of structure reported to <entity_reported>.

    if <entity_reported> is assigned and <entity_reported> is not initial.
      loop at <entity_reported> assigning field-symbol(<entity_reported_instance>).
        if line_exists( dbl_transactional_buffer_table[ entity_name = entity-name ] ).
          assign dbl_transactional_buffer_table[ entity_name = entity-name  ] to <buffer_4_entity>.
          assign <buffer_4_entity>-entity_instances->* to <entity_buffer_instances>.
          data(entity_fields) = zcl_abap_behv_buf_util=>get_entity_fields( entity-name ).
          loop at <entity_buffer_instances> assigning <entity_buffer_instance>.
            data(equal) = zcl_abap_behv_buf_util=>check_instance_equal(
                                 exporting
                                   instance_1    = <entity_buffer_instance>
                                   instance_2    = <entity_reported_instance>
                                   entity_fields = entity_fields
                             ).
            if equal = abap_true.
              read table <buffer_4_entity>-modify_response_config with key operation = operation assigning field-symbol(<response_config>).
              if  <response_config> is not assigned .
                "create an entry for the modify response config
                data reported_config_for_op type zif_abap_behv_test_trans_bufr6=>ty_modify_response_config.
                create data reported_config_for_op-reported like reported.
                reported_config_for_op-operation = operation.

                assign reported_config_for_op-reported->* to field-symbol(<reported_to_be_filled>).
                assign component alias of structure <reported_to_be_filled> to <reported_entity_to_be_filled>.
                append <entity_reported_instance> to <reported_entity_to_be_filled>.

                append reported_config_for_op to <buffer_4_entity>-modify_response_config.
                exit.
              else.
                "check if mapped is configured for the operation and instance
                if <response_config>-mapped is not initial.
                  assign <response_config>-mapped->* to field-symbol(<entity_mapped>).
                  assign component alias of structure <entity_mapped> to <entity_mapped_instances>.
                  loop at <entity_mapped_instances> assigning field-symbol(<entity_mapped_instance>).
                    data(mapped_already_configured) = zcl_abap_behv_buf_util=>check_instance_equal(
                                 exporting
                                   instance_1    = <entity_mapped_instance>
                                   instance_2    = <entity_reported_instance>
                                   entity_fields = entity_fields
                             ).
                    if mapped_already_configured eq abap_true.
                      "TODO: Exception mapped is already configured. Configuring reported for the same instance is undefined.
                      raise exception type zcx_bo_tdf_failure
                        exporting
                          textid = zcx_bo_tdf_failure=>incorrect_response_config
                          preconfigured = 'mapped'
                          new_configuration = 'reported'.
                      EXIT.
                    endif.
                  endloop.
                  if mapped_already_configured eq abap_true.
                      "TODO: Exception mapped is already configured. Configuring reported for the same instance is undefined.
                      raise exception type zcx_bo_tdf_failure
                        exporting
                          textid = zcx_bo_tdf_failure=>incorrect_response_config
                          preconfigured = 'mapped'
                          new_configuration = 'reported'.
                      EXIT.
                  endif.

                  "At this point it is clear that mapped is not configured for the instance.
                  if <response_config>-reported is initial.
                    create data <response_config>-reported like reported.
                    assign <response_config>-reported->* to <reported_to_be_filled>.
                    assign component alias of structure <reported_to_be_filled> to <reported_entity_to_be_filled>.
                    append <entity_reported_instance> to <reported_entity_to_be_filled>.
                    exit.
                  else.
                    "TODO: Handle duplicate entries
                    assign <response_config>-reported->* to <reported_to_be_filled>.
                    assign component alias of structure <reported_to_be_filled> to <reported_entity_to_be_filled>.
                    append <entity_reported_instance> to <reported_entity_to_be_filled>.
                    exit.
                  endif.

                else.
                  "At this point it is clear that mapped is not configured for the instance.
                  if <response_config>-reported is initial.
                    create data <response_config>-reported like reported.
                    assign <response_config>-reported->* to <reported_to_be_filled>.
                    assign component alias of structure <reported_to_be_filled> to <reported_entity_to_be_filled>.
                    append <entity_reported_instance> to <reported_entity_to_be_filled>.
                    exit.
                  else.
                    "TODO: Handle duplicate entries
                    assign <response_config>-reported->* to <reported_to_be_filled>.
                    assign component alias of structure <reported_to_be_filled> to <reported_entity_to_be_filled>.
                    append <entity_reported_instance> to <reported_entity_to_be_filled>.
                    exit.
                  endif.

                endif.
              endif.
            else.
              "TODO: Throw exception with a reason that  "no buffer entry exists for the entity instance"
              " Record the instance for which the response is to be configured.
*                raise exception type zcx_bo_tdf_failure
*                  exporting
*                    textid = zcx_bo_tdf_failure=>entity_not_recorded
*                    new_configuration = 'reported'.
*                EXIT.
" A little issue as this should go back into loop until the instance is not found

            endif.
          endloop.
            if equal eq abap_false.
                raise exception type zcx_bo_tdf_failure
                  exporting
                    textid = zcx_bo_tdf_failure=>entity_not_recorded
                    new_configuration = 'reported'.
                EXIT.
            endif.
        else.
          "TODO: Throw exception with a reason that  "no buffer exists for the entity"
          " Record the instance for which the response is to be configured.
          raise exception type zcx_bo_tdf_failure
            exporting
              textid = zcx_bo_tdf_failure=>entity_not_recorded
              new_configuration = 'reported'.
          EXIT.
        endif.
      endloop.
    endif.

  endmethod.

  method configure_failed_for_modify.
    field-symbols: <buffer_4_entity>  type zif_abap_behv_test_trans_bufr6=>ty_double_transactional_buffer.
    field-symbols: <entity_mapped_instances>  type standard table.
    field-symbols: <entity_failed> type standard table.
    field-symbols: <failed_entity_to_be_filled> type standard table.
    field-symbols: <entity_buffer_instances> type standard table.
    field-symbols: <entity_buffer_instance>  type any.


    data(alias) = zcl_abap_behv_buf_util=>get_entity_alias( entity = entity ).
    assign component alias of structure failed to <entity_failed>.

    if <entity_failed> is assigned and <entity_failed> is not initial.
      loop at <entity_failed> assigning field-symbol(<entity_failed_instance>).
        if line_exists( dbl_transactional_buffer_table[ entity_name = entity-name ] ).
          assign dbl_transactional_buffer_table[ entity_name = entity-name  ] to <buffer_4_entity>.
          assign <buffer_4_entity>-entity_instances->* to <entity_buffer_instances>.
          data(entity_fields) = zcl_abap_behv_buf_util=>get_entity_fields( entity-name ).
          loop at <entity_buffer_instances> assigning <entity_buffer_instance>.
            data(equal) = zcl_abap_behv_buf_util=>check_instance_equal(
                                 exporting
                                   instance_1    = <entity_buffer_instance>
                                   instance_2    = <entity_failed_instance>
                                   entity_fields = entity_fields
                             ).
            if equal = abap_true.
              read table <buffer_4_entity>-modify_response_config with key operation = operation assigning field-symbol(<response_config>).
              if <response_config> is not assigned.
                "create an entry for the modify response config
                data failed_config_for_op type zif_abap_behv_test_trans_bufr6=>ty_modify_response_config.
                create data failed_config_for_op-failed like failed.
                failed_config_for_op-operation = operation.

                assign failed_config_for_op-failed->* to field-symbol(<failed_to_be_filled>).
                assign component alias of structure <failed_to_be_filled> to <failed_entity_to_be_filled>.
                append <entity_failed_instance> to <failed_entity_to_be_filled>.

                append failed_config_for_op to <buffer_4_entity>-modify_response_config.
                exit.
              else.
                "check if mapped is configured for the operation and instance
                if <response_config>-mapped is not initial.
                  data(mapped_already_configured) = find_mapped_config_for_modify(
                                            exporting
                                              entity            = entity
                                              instance_to_check = <entity_failed_instance>
                                              operation         = operation
                                            changing
                                              buffer_4_entity   = <buffer_4_entity>
                                          ).

                    if mapped_already_configured eq abap_true.
                      "TODO: Exception mapped is already configured. Configuring failed for the same instance is undefined.
                      raise exception type zcx_bo_tdf_failure
                        exporting
                          textid = zcx_bo_tdf_failure=>incorrect_response_config
                          preconfigured = 'mapped'
                          new_configuration = 'failed'.
                      EXIT.
                    endif.
                 endif.

                "At this point it is clear that mapped is not configured for the instance.
                if <response_config>-failed is initial.
                  create data <response_config>-failed like failed.
                  assign <response_config>-failed->* to <failed_to_be_filled>.
                  assign component alias of structure <failed_to_be_filled> to <failed_entity_to_be_filled>.
                  append <entity_failed_instance> to <failed_entity_to_be_filled>.
                  exit.
                else.
                  "TODO: Handle duplicate entries
                  assign <response_config>-failed->* to <failed_to_be_filled>.
                  assign component alias of structure <failed_to_be_filled> to <failed_entity_to_be_filled>.
                  append <entity_failed_instance> to <failed_entity_to_be_filled>.
                  exit.
                endif.

              endif.
            else.
              "TODO: Throw exception with a reason that  "no buffer entry exists for the entity instance"
              " Record the instance for which the response is to be configured.
*              raise exception type zcx_bo_tdf_failure
*                exporting
*                  textid = zcx_bo_tdf_failure=>entity_not_recorded
*                  new_configuration = 'failed'.
*
*              EXIT.

            endif.
          endloop.
            if equal = abap_false.
              raise exception type zcx_bo_tdf_failure
                exporting
                  textid = zcx_bo_tdf_failure=>entity_not_recorded
                  new_configuration = 'failed'.
              EXIT.
            endif.
        else.
          "TODO: Throw exception with a reason that  "no buffer exists for the entity"
          " Record the instance for which the response is to be configured.
            raise exception type zcx_bo_tdf_failure
              exporting
                textid = zcx_bo_tdf_failure=>entity_not_recorded
                new_configuration = 'failed'.

            EXIT.
        endif.
      endloop.
    endif.

  endmethod.

  method find_failed_config_for_modify.

    field-symbols: <configured_entity_failed> type standard table.
    field-symbols: <existing_buff_failed> type any.

    data(alias) = zcl_abap_behv_buf_util=>get_entity_alias( entity = entity ).
    alias = to_upper( alias ).
    data(entity_fields) = zcl_abap_behv_buf_util=>get_entity_fields( entity-name ).
    result = abap_false.

    read table buffer_4_entity-modify_response_config with key operation = operation into data(response_config).

    if response_config is not initial and response_config-failed is not initial.
      assign response_config-failed->* to field-symbol(<configured_failed>).
      assign component alias of structure <configured_failed> to <configured_entity_failed>.
    endif.

    if <configured_entity_failed> is assigned and <configured_entity_failed> is not initial.
      loop at <configured_entity_failed> assigning field-symbol(<configured_failed_instance>).
        data(equal) = zcl_abap_behv_buf_util=>check_instance_equal(
                       exporting
                         instance_1    = instance_to_check
                         instance_2    = <configured_failed_instance>
                         entity_fields = entity_fields
                   ).

        if equal = abap_true.
          if c_failed is supplied.
            field-symbols <fs_entry_failed_table> type standard table.
            assign component alias of structure c_failed to <fs_entry_failed_table>.
            append <configured_failed_instance> to <fs_entry_failed_table>.
          endif.
          result = abap_true.
          exit.
        endif.
      endloop.
    endif.

  endmethod.

  method find_mapped_config_for_modify.
    field-symbols: <configured_entity_mapped> type standard table.

    data(alias) = zcl_abap_behv_buf_util=>get_entity_alias( entity = entity ).
    alias = to_upper( alias ).
    data(entity_fields) = zcl_abap_behv_buf_util=>get_entity_fields( entity-name ).
    result = abap_false.

    read table buffer_4_entity-modify_response_config with key operation = operation into data(response_config).

    if response_config is not initial and response_config-mapped is not initial.
      assign response_config-mapped->* to field-symbol(<configured_mapped>).
      assign component alias of structure <configured_mapped> to <configured_entity_mapped>.
    endif.

    if <configured_entity_mapped> is assigned and  <configured_entity_mapped> is not initial.
      loop at <configured_entity_mapped> assigning field-symbol(<configured_mapped_instance>).
        data(equal) = zcl_abap_behv_buf_util=>check_instance_equal(
                       exporting
                         instance_1    = instance_to_check
                         instance_2    = <configured_mapped_instance>
                         entity_fields = entity_fields
                   ).

        if equal = abap_true.
          if c_mapped is supplied.
            field-symbols <fs_entry_mapped_table> type standard table.
            assign component alias of structure c_mapped to <fs_entry_mapped_table>.
            append <configured_mapped_instance> to <fs_entry_mapped_table>.
          endif.
          result = abap_true.
          exit.
        endif.
      endloop.
    endif.
  endmethod.


  method find_reported_config_fr_modify.

    field-symbols: <configured_entity_reported> type standard table.
    field-symbols: <existing_buff_reported> type any.

    data(alias) = zcl_abap_behv_buf_util=>get_entity_alias( entity = entity ).
    alias = to_upper( alias ).
    data(entity_fields) = zcl_abap_behv_buf_util=>get_entity_fields( entity-name ).
    result = abap_false.

    read table buffer_4_entity-modify_response_config with key operation = operation into data(response_config).

    if response_config is not initial and response_config-reported is not initial.
      assign response_config-reported->* to field-symbol(<configured_reported>).
      assign component alias of structure <configured_reported> to <configured_entity_reported>.
    endif.

    if <configured_entity_reported> is assigned and <configured_entity_reported> is not initial.
      loop at <configured_entity_reported> assigning field-symbol(<configured_reported_instance>).
        data(equal) = zcl_abap_behv_buf_util=>check_instance_equal(
                       exporting
                         instance_1    = instance_to_check
                         instance_2    = <configured_reported_instance>
                         entity_fields = entity_fields
                   ).

        if equal = abap_true.
          if c_reported is supplied.
            field-symbols <fs_entry_reported_table> type standard table.
            assign component alias of structure c_reported to <fs_entry_reported_table>.
            append <configured_reported_instance> to <fs_entry_reported_table>.
          endif.
          result = abap_true.
          exit.
        endif.
      endloop.
    endif.

  endmethod.



endclass.
