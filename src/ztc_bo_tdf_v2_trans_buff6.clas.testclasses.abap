*"* use this source file for your ABAP unit test classes
class ltcl_modify_eml definition final for testing
  duration short
  risk level harmless.

  private section.

    methods create_valid_inst_no_record for testing raising cx_static_check.
    methods create_invalid_entity_instance for testing raising cx_static_check.
    methods create_inst_incomplete_key for testing raising cx_static_check.

    methods create_valid_inst_by_assoc for testing raising cx_static_check.
    methods create_invalid_inst_by_assoc_1 for testing raising cx_static_check.
    methods create_invalid_inst_by_assoc_2 for testing raising cx_static_check.

    methods update_invalid_instance for testing raising cx_static_check.
    methods update_valid_instance for testing raising cx_static_check.
    methods update_inst_imcomplete_key for testing raising cx_static_check.

    methods delete_inst_incomplete_key for testing raising cx_static_check.
    methods delete_valid_instance for testing raising cx_static_check.
    methods delete_valid_instance_2 for testing raising cx_static_check.
    methods delete_invalid_instance for testing raising cx_static_check.
    methods cascading_delete_ok for testing raising cx_static_check.

    methods complex_modify for testing raising cx_static_check.
    methods modify_with_multiple_ops for testing raising cx_static_check.
    methods mock_multiple_instances for testing raising cx_static_check.

    methods create_valid_inst_with_record for testing raising cx_static_check.
    methods create_passes_update_fails for testing raising cx_static_check.
    methods deep_create for testing raising cx_static_check.

    methods teardown.
    class-methods class_setup.
    class-data double type ref to zcl_abap_behv_generic_tst_dbl6.
endclass.


class ltcl_modify_eml implementation.

  method class_setup.
    double = new zcl_abap_behv_generic_tst_dbl6( '/dmo/i_travel_tum' ).
    cl_abap_behv_test_environment=>set_test_double( double ).

  endmethod.

  method teardown.
    double->clear_buffer( ).
  endmethod.

  method create_invalid_entity_instance.

    "Record
    modify entities of /dmo/i_travel_tum
    entity travel
      create from value #( ( travelid = '987'
                            begindate = '20180101' EndDate = '20180101'
                            totalprice = 100
                            %control-begindate = if_abap_behv=>mk-on
                            %control-totalprice = if_abap_behv=>mk-on )
                         )
    reported data(reported)
    failed data(failed)
    mapped data(mapped).

    "Try t create the same instance
    modify entities of /dmo/i_travel_tum
    entity travel
      create from value #( ( travelid = '987'
                             begindate = '20180101' EndDate = '20180101'
                            totalprice = 100
                            %control-begindate = if_abap_behv=>mk-on
                            %control-totalprice = if_abap_behv=>mk-on )
                         )
    reported data(reported1)
    failed data(failed1)
    mapped data(mapped1).

    cl_abap_unit_assert=>assert_initial( mapped1 ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).

  endmethod.

  method mock_multiple_instances.
    double = new zcl_abap_behv_generic_tst_dbl6( '/dmo/i_travel_tmn' ).
    cl_abap_behv_test_environment=>set_test_double( double ).

    data:   cut  type ref to zcl_abap_behv_test_demo .
    cut = new zcl_abap_behv_test_demo( ).

    modify entities of /dmo/i_travel_tmn
     entity travel
       create from value #( ( travel_id = '987'  ) )
     reported data(reported2)
     failed   data(failed2)
     mapped   data(mapped2).

    "Inserting duplicate test data
    modify entities of /dmo/i_travel_tum
      entity travel
        create from value #(
                             ( travelid = '987' begindate = '20180101' EndDate = '20180101' totalprice = 100  %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
                             ( travelid = '987' currencycode = 'EUR'  %control-currencycode = if_abap_behv=>mk-on )
                           )
      reported data(reported3)
      failed data(failed3)
      mapped data(mapped3).


    "Read
    read entity /dmo/i_travel_tum
    from value #(
               ( travelid = '987' %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
             )
    result   data(result)
    reported data(reported)
    failed   data(failed).

    read entity /dmo/i_travel_tmn
     from value #(
                   ( travel_id = '987' %control-begin_date = if_abap_behv=>mk-on )
                 )
     result   data(result4)
     reported data(reported4)
     failed   data(failed4).

    cl_abap_unit_assert=>assert_not_initial( result ).
    cl_abap_unit_assert=>assert_not_initial( result4 ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).

  endmethod.

  method complex_modify.
  endmethod.

  method create_inst_incomplete_key.
    "if key is incomplete 00000 is the value that is assigned by default to a key.
*     modify entities of /dmo/i_travel_tum
*      entity travel
*        create from value #(
*                              ( begindate = '20180101' EndDate = '20180101'
*                              totalprice = 100
*                              %control-begindate = if_abap_behv=>mk-on
*                              %control-totalprice = if_abap_behv=>mk-on )
*                           )
*      reported data(reported3)
*      failed data(failed3)
*      mapped data(mapped3).
*
*   cl_abap_unit_assert=>assert_initial( mapped3 ).  " assertion fails because if key is incomplete 00000 is the value that is
*                                                    " assigned by default to a key.


  endmethod.

  method create_valid_inst_no_record.

  "No record step. the create in CUT will perform the create op on the buffer directly.


  " CUT
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    cl_abap_unit_assert=>assert_not_initial( mapped ).
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).


  endmethod.

  method create_valid_inst_with_record.
  "CUT has create EML.
  "Step 1: Record the instance for which create EML has to be evaluated.
  "Step 2: Configure the Create Operation in CUT to succeed.

  "Record.
  "Step 1.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

   "Step 2.
    clear reported.
    clear failed.
    "change mapped if required.
    mapped-travel = value #( ( TravelID = '987' ) ).
    double->config_response_4_modify( operation = if_abap_behv=>op-m-create mapped = mapped failed = failed reported = failed ).


  " CUT
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported_cut)
     failed data(failed_cut)
     mapped data(mapped_cut).

    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).
    cl_abap_unit_assert=>assert_equals( act = mapped_cut-travel[ 1 ]-TravelID  exp = '987' ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).


  endmethod.

  method delete_inst_incomplete_key.

  endmethod.

  method delete_invalid_instance.
    modify entities of /dmo/i_travel_tum
       entity travel
         delete from value #( ( %key-TravelID = '987' )
                            )
       reported data(reported)
       failed data(failed)
       mapped data(mapped).

    cl_abap_unit_assert=>assert_initial( mapped ).
    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_not_initial( reported ).

  endmethod.

  method delete_valid_instance.

    " Record test data
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    " cuts
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' )
                          )
     reported data(reported2)
     failed data(failed2)
     mapped data(mapped2).

    cl_abap_unit_assert=>assert_initial( mapped2 ).
    cl_abap_unit_assert=>assert_initial( failed2 ).
    cl_abap_unit_assert=>assert_initial( reported2 ).
  endmethod.

  method delete_valid_instance_2.

   "This method tries to check the use-case :
   "if the root-instance is deleted then associated entities are also deleted."

    " Record test data for root entity
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    " Record test data for associated entity
     ""Create booking by association
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( (
                                              BookingID = '333'
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).

    " cuts
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' )
                          )
     reported data(reported2)
     failed data(failed2)
     mapped data(mapped2).

   "Check that associated entity should is also deleted - i.e. cascading delete
   "Read the booking entity
   read entity /dmo/i_travel_tum
    by \_Booking
     from value #(
                ( travelid = '987'  %control-FlightPrice = if_abap_behv=>mk-on %control-BookingDate = if_abap_behv=>mk-on
                  %control-BookingID = if_abap_behv=>mk-on )
              )
     result   data(result3)
     reported data(reported3)
     failed   data(failed3).


    cl_abap_unit_assert=>assert_initial( mapped2 ).
    cl_abap_unit_assert=>assert_initial( failed2 ).
    cl_abap_unit_assert=>assert_initial( reported2 ).
  endmethod.

  method cascading_delete_ok.
    "Insert test travel entity
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                            ( travelid = '988'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 200
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
       reported data(reported)
       failed data(failed)
       mapped data(mapped).

   "Insert test booking instance
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '988'
                         %target = value #( (
                                              BookingDate = '20200902'
                                              FlightPrice = '100'

                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).

   "Insert test booking supplement instance
    modify entities of /dmo/i_travel_tum
     entity booking
       create by \_BookSuppl
         from value #( ( %key-TravelID = '988'
                         %key-BookingID = '000'
                         %target = value #( (
                                              Price = '100'
                                              %control-Price = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped ls_mapped
             failed ls_failed
             reported ls_reported.


    " Confirm if test data was inserted sucessfully
    READ ENTITIES OF /dmo/i_travel_tum
      ENTITY TRAVEL
        all fields with value #( ( %key-TravelID = '987' ) ( %key-TravelID = '988' ) )
        result data(lt_result_travel)
        by \_Booking
          all fields with value #( ( %key-TravelID ='987' ) ( %key-TravelID ='988' ) )
          result data(lt_result_booking)
      entity booking
        by \_BookSuppl
        all fields with value #( ( %key-travelid = '987'  %key-bookingid = '000' )
                                  ( %key-travelid = '988'  %key-bookingid = '000' ) )
          result data(lt_result_bookSupp)
          failed failed
          reported data(lt_reported).

    " Delete the travel instance "Cascading delete
    MODIFY ENTITIES OF /dmo/i_travel_tum
      ENTITY Travel
        DELETE FROM VALUE #( ( %key-travelid = '988'
                           ) )
        FAILED DATA(failed_delete)
        REPORTED DATA(reported_delete)
        MAPPED DATA(mapped_delete).


    READ ENTITIES OF /dmo/i_travel_tum
      ENTITY TRAVEL
        all fields with value #( ( %key-TravelID = '987' ) ( %key-TravelID = '988' ) )
        result lt_result_travel
        by \_Booking
          all fields with value #( ( %key-TravelID ='987' ) ( %key-TravelID ='988' ) )
          result lt_result_booking
      entity booking
        by \_BookSuppl
        all fields with value #( ( %key-travelid = '987'  %key-bookingid = '000' )
                                 ( %key-travelid = '988'  %key-bookingid = '000' ) )
          result lt_result_bookSupp
          failed failed
          reported lt_reported.
  ENDMETHOD.


  method modify_with_multiple_ops.
    " Create and update in the same modify statement.
    " Create will pass but update will fail as we try to update an instance that does not exist.

    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
       update from value #( ( %key-TravelID = '988'
                             EndDate = '20180102'
                             totalprice = 200
                             %control-enddate = if_abap_behv=>mk-on
                             %control-totalprice = if_abap_behv=>mk-on )
                          )

     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    cl_abap_unit_assert=>assert_not_initial( reported ).
    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_not_initial( mapped ).

    cl_abap_unit_assert=>assert_equals( exp = '987' act = mapped-travel[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( exp = '988' act = reported-travel[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( exp = '988' act = failed-travel[ 1 ]-TravelID ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).



  endmethod.

  method update_inst_imcomplete_key.

  endmethod.

  method update_invalid_instance.

    modify entities of /dmo/i_travel_tum
     entity travel
       update from value #( ( %key-TravelID = '987'
                             begindate = '20180101' EndDate = '20180101'
                             totalprice = 100
                             %control-begindate = if_abap_behv=>mk-on
                             %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported3)
     failed data(failed3)
     mapped data(mapped3).

    cl_abap_unit_assert=>assert_initial( mapped3 ).
    cl_abap_unit_assert=>assert_not_initial( failed3 ).
    cl_abap_unit_assert=>assert_not_initial( reported3 ).

  endmethod.

  method update_valid_instance.

    " Record test data
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    " cut
    modify entities of /dmo/i_travel_tum
     entity travel
       update from value #( ( %key-TravelID = '987'
                             EndDate = '20180102'
                             totalprice = 200
                             %control-enddate = if_abap_behv=>mk-on
                             %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported2)
     failed data(failed2)
     mapped data(mapped2).

    cl_abap_unit_assert=>assert_not_initial( mapped2 ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).

  endmethod.

  method create_valid_inst_by_assoc.

    " Record step to insert a travel instance for which booking is to be created.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).


    ""Create booking by association
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( (
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).

    cl_abap_unit_assert=>assert_initial( ls_failed ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = '0000' act = ls_mapped-booking[ 1 ]-BookingID ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).

    modify entities of /dmo/i_travel_tum
     entity booking
       delete from value #( ( %key-TravelID = '987' %key-BookingID = '000' ) ).


  endmethod.


  method create_invalid_inst_by_assoc_1.

    " Record step to insert a travel instance for which booking is to be created.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).


    ""Create booking by association- Record
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( (
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).

    cl_abap_unit_assert=>assert_initial( ls_failed ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = '0000' act = ls_mapped-booking[ 1 ]-BookingID ).


    ""Create booking by association- Create the same booking again
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( (
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped ls_mapped
             failed ls_failed
             reported ls_reported.


    cl_abap_unit_assert=>assert_initial( ls_mapped ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking should have failed' exp = '0000' act = ls_failed-booking[ 1 ]-BookingID ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking should have failed' exp = '01' act = ls_failed-booking[ 1 ]-%create ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking should have failed' exp = '0000' act = ls_reported-booking[ 1 ]-BookingID ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).

    modify entities of /dmo/i_travel_tum
     entity booking
       delete from value #( ( %key-TravelID = '987' %key-BookingID = '000' ) ).

  endmethod.

  method create_invalid_inst_by_assoc_2.

    ""Create booking by association
    "" Travel instance with TravelID 987 does not exist.
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( (
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).


    cl_abap_unit_assert=>assert_initial( ls_mapped ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking should have failed' exp = '0000' act = ls_failed-booking[ 1 ]-BookingID ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking should have failed' exp = '01' act = ls_failed-booking[ 1 ]-%create ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking should have failed' exp = '0000' act = ls_reported-booking[ 1 ]-BookingID ).

  endmethod.


  method create_passes_update_fails.
  "CUT has CREATE EML which should pass.
  "And a UPDATE EML which should fail.

  "Step 1: Record the instance for which create and update EML has to be evaluated.
  "Step 2: Configure the Create Operation in CUT to succeed.
  "Step 3: Configure the Update Operation in CUT to fail.

  "Record.
  "Step 1.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

   "Step 2.
    clear reported.
    clear failed.
    "change mapped if required.
    mapped-travel = value #( ( TravelID = '987' ) ).
    double->config_response_4_modify( operation = if_abap_behv=>op-m-create mapped = mapped failed = failed reported = failed ).

   "Step 3.
    clear mapped.
    "change reported if required.
    "Configure the exception
    data: lo_bo_exception type ref to z_bo_mock_exception.
          create object lo_bo_exception.
    reported-travel = value #( ( TravelID = '987' %msg = lo_bo_exception ) ).
    failed-travel = value #( ( TravelID = '987'  ) ).

    double->config_response_4_modify( operation = if_abap_behv=>op-m-update mapped = mapped failed = failed reported = reported ).


  " CUT
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported_cut)
     failed data(failed_cut)
     mapped data(mapped_cut).

    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

    if mapped_cut is not initial.
      modify entities of /dmo/i_travel_tum
      entity travel
       update from value #( ( travelid = '987'
                              totalprice = 200
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
      reported reported_cut
      failed failed_cut
      mapped mapped_cut.

      cl_abap_unit_assert=>assert_initial( mapped_cut ).
      cl_abap_unit_assert=>assert_not_initial( failed_cut ).
      cl_abap_unit_assert=>assert_not_initial( reported_cut ).

    endif.

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).



  endmethod.

  method deep_create.
  "CUT has a deep create statement
  "Check if correct responses are returned by the EML statement

  "No Record required.

    " Create a BO instance using Deep Create
    modify entities of /dmo/i_travel_tum
       " Create a booking supplement by association
       entity booking
             create by \_BookSuppl fields ( supplementid price  ) with
                       value #( ( %cid_ref = 'CID_200'
                                  %target  = value #( (
                                                 %cid          = 'CID_300'
                                                 supplementid  = '007'
                                                 price         = 100
                                                  ) ) ) )
     entity travel
             create fields ( travelid customerid begindate enddate status ) with
                         value #( ( %cid        = 'CID_100'    " Preliminary ID for new travel instance
                                    travelid    = '987'
                                    customerid  = '9876'
                                    begindate   = '20180101'
                                    enddate     = '20180101'
                                    status      = 'O' ) ) "Travel status open
             " Create a new booking by association
             create by \_booking fields (  customerid BookingDate FlightPrice ) with
                         value #( ( %cid_ref  = 'CID_100'      "refers to the root (travel instance)
                                    %target   = value #( (
                                                  %cid           = 'CID_200' " Preliminary ID for new booking instance
                                                  customerid     = '9876'
                                                  flightprice     = 100
                                                  BookingDate   = '20180101' ) ) ) )
             execute set_status_booked from
                         value #( ( %cid_ref = 'CID_100') )
       MAPPED   DATA(mapped)
       FAILED   DATA(failed)
       REPORTED DATA(reported).

  "Asserts
    cl_abap_unit_assert=>assert_initial( act = failed ).  " Creating a travel BO instance is successful.
    cl_abap_unit_assert=>assert_initial( act = reported ).  " Creating a travel BO instance is successful.

    cl_abap_unit_assert=>assert_equals( exp = 'CID_100' act = mapped-travel[ 1 ]-%cid ).
    cl_abap_unit_assert=>assert_equals( exp = 'CID_200' act = mapped-booking[ 1 ]-%cid ).
    cl_abap_unit_assert=>assert_equals( exp = 'CID_300' act = mapped-booksuppl[ 1 ]-%cid ).

  endmethod.

endclass.


class ltcl_read_eml definition final for testing
  duration short
  risk level harmless.

  private section.

    methods read_valid_entity_instance for testing raising cx_static_check.
    methods read_valid_entity_instances for testing raising cx_static_check.
    methods read_invalid_entity_instance for testing raising cx_static_check.
    methods read_not_existing_bo_node for testing raising cx_static_check.
    methods read_entity_with_multiple_keys for testing raising cx_static_check.
    methods read_entity_with_ctrl_struct for testing raising cx_static_check.
    methods read_mixed_entity_instances for testing raising cx_static_check.

    methods read_valid_instance_by_assoc for testing raising cx_static_check.
    methods read_invalid_inst_by_assoc_1 for testing raising cx_static_check.
    methods read_invalid_inst_by_assoc_2 for testing raising cx_static_check.
    methods read_invalid_inst_by_assoc_3 for testing raising cx_static_check.
    methods report_an_instance_for_modify for testing raising cx_static_check.
    methods report_an_instance_for_read for testing raising cx_static_check.

    class-methods class_setup.
    methods teardown.
    class-data double type ref to zcl_abap_behv_generic_tst_dbl6.
endclass.


class ltcl_read_eml implementation.

  method class_setup.
    double = new zcl_abap_behv_generic_tst_dbl6( '/dmo/i_travel_tum' ).
    cl_abap_behv_test_environment=>set_test_double( double ).
  endmethod.

  method read_entity_with_ctrl_struct.
    " TODO: Check if required.
  endmethod.

  method read_entity_with_multiple_keys.
    " A booking with TravelID and BookingID as keys

    " Record test data
    " create travel instance
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                            ( travelid = '000'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    " create booking instance
    modify entities of /dmo/i_travel_tum
     entity booking
       create from value #( ( travelid = '987'
                              BookingID = '001'
                                             BookingDate = '20200902'
                                             FlightPrice = '100'
                                             %control-BookingDate = if_abap_behv=>mk-on
                                             %control-FlightPrice = if_abap_behv=>mk-on )
                            ( travelid = '000'
                              BookingID = '001'
                                             BookingDate = '20200902'
                                             FlightPrice = '100'
                                             %control-BookingDate = if_abap_behv=>mk-on
                                             %control-FlightPrice = if_abap_behv=>mk-on )
                          )
     reported reported
     failed failed
     mapped mapped.

    " CUT
    " Read booking

    read entities of /dmo/i_travel_tum
    entity booking
     from value #(
                ( travelid = '987' BookingID = '001' %control-FlightPrice = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported reported
     failed   failed.

    cl_abap_unit_assert=>assert_not_initial( result ).
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).

    cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = '100' act = result[ 1 ]-FlightPrice ).


    read entities of /dmo/i_travel_tum
     entity booking
      from value #(
                 ( travelid = '988' BookingID = '001' %control-FlightPrice = if_abap_behv=>mk-on )
               )
      result   result
      reported reported
      failed   failed.

    cl_abap_unit_assert=>assert_equals( msg = 'read passed' exp = '988' act = failed-booking[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( msg = 'read passed' exp = '001' act = failed-booking[ 1 ]-BookingID ).

    read entities of /dmo/i_travel_tum
     entity booking
      from value #(
                 ( travelid = '987' BookingID = '002' %control-FlightPrice = if_abap_behv=>mk-on )
               )
      result   result
      reported reported
      failed   failed.

    cl_abap_unit_assert=>assert_equals( msg = 'read passed' exp = '987' act = failed-booking[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( msg = 'read passed' exp = '002' act = failed-booking[ 1 ]-BookingID ).

    read entities of /dmo/i_travel_tum
     entity booking
      from value #(
                 (  BookingID = '001' %control-FlightPrice = if_abap_behv=>mk-on )
               )
      result   result
      reported reported
      failed   failed.

    cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = '100' act = result[ 1 ]-FlightPrice ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ( %key-TravelID = '000' ) ).

    modify entities of /dmo/i_travel_tum
     entity booking
       delete from value #( ( %key-TravelID = '987' %key-BookingID = '001' ) ( %key-TravelID = '000' %key-BookingID = '001' ) ).


  endmethod.

  method read_invalid_entity_instance.
    " read an entity that does not exist

    " Record test data
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    " Read travel entity which does not exist
    read entity /dmo/i_travel_tum
     from value #(
                ( travelid = '988' %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported reported
     failed   failed.

    cl_abap_unit_assert=>assert_initial( result ).
    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_not_initial( reported ).

    cl_abap_unit_assert=>assert_equals( msg = 'read passed' exp = '988' act = failed-travel[ 1 ]-TravelID ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).


  endmethod.


  method read_mixed_entity_instances.
    " read an entity that does not exist and read an entity that exists

    " Record test data
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    " Read travel entity which does not exist
    read entity /dmo/i_travel_tum
     from value #(
                ( travelid = '987' %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
                ( travelid = '988' %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported reported
     failed   failed.

    cl_abap_unit_assert=>assert_equals( msg = 'read failed for travel id 987' exp = '987' act = result[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( msg = 'read passed for travel id 988' exp = '988' act = failed-travel[ 1 ]-TravelID ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).

  endmethod.



  method read_not_existing_bo_node.
    " Results in a compile time error to read a non existing bo node.
  endmethod.

  method read_valid_entity_instance.

    " Record test data
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              memo = 'travel 1'
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on
                              %control-Memo = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    " CUT
    read entity /dmo/i_travel_tum
     from value #(
                ( travelid = '987' %control-Memo = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported data(reported2)
     failed   data(failed2).

    cl_abap_unit_assert=>assert_not_initial( result ).
    cl_abap_unit_assert=>assert_initial( reported2 ).
    cl_abap_unit_assert=>assert_initial( failed2 ).

    cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = '987' act = result[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = 'travel 1' act = result[ 1 ]-Memo ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' )
                          ).

  endmethod.

  method read_valid_entity_instances.

    " Record test data
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              Memo = 'travel 1'
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on
                              %control-Memo = if_abap_behv=>mk-on )
                            ( travelid = '988'
                              begindate = '20190101' EndDate = '20190101'
                              totalprice = 300
                              Memo = 'travel 2'
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on
                              %control-Memo = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    " CUT
    read entity /dmo/i_travel_tum
     from value #(
                ( travelid = '987' %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
                ( travelid = '988' %control-memo = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported data(reported2)
     failed   data(failed2).

    cl_abap_unit_assert=>assert_not_initial( result ).
    cl_abap_unit_assert=>assert_initial( reported2 ).
    cl_abap_unit_assert=>assert_initial( failed2 ).

    read table result with key TravelID = '988' into data(travel).
    cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = 0 act = sy-subrc ).

    cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = 'travel 2' act = travel-Memo ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ( %key-TravelID = '988' ) ).

  endmethod.


  method read_valid_instance_by_assoc.

    " Record step to insert a travel instance .
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).


    ""Record: Create booking by association
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( ( "Since no booking Id is passed booking id 0000 will be assigned to the booking
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).

    cl_abap_unit_assert=>assert_initial( ls_failed ).
    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = '0000' act = ls_mapped-booking[ 1 ]-BookingID ).

    " CUT
    read entity /dmo/i_travel_tum
    by \_Booking
     from value #(
                ( travelid = '987'  %control-FlightPrice = if_abap_behv=>mk-on %control-BookingDate = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported data(reported2)
     failed   data(failed2).

    cl_abap_unit_assert=>assert_not_initial( result ).
    cl_abap_unit_assert=>assert_initial( reported2 ).
    cl_abap_unit_assert=>assert_initial( failed2 ).

    read table result with key bookingid = '000' into data(booking).
    cl_abap_unit_assert=>assert_equals( msg = 'read failed' exp = 0 act = sy-subrc ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ( %key-TravelID = '000' ) ).

    modify entities of /dmo/i_travel_tum
     entity booking
       delete from value #( ( %key-TravelID = '987' %key-BookingID = '001' ) ( %key-TravelID = '000' %key-BookingID = '001' ) ).


  endmethod.

  method read_invalid_inst_by_assoc_1.
    " No booking is present for the queried travel

    " Record step to insert a travel instance .
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).


    ""NO booking
*    modify entities of /dmo/i_travel_tum
*     entity travel
*       create by \_Booking
*         from value #( ( %key-TravelID = '987'
*                         %target = value #( (
*                                              BookingDate = '20200902'
*                                              FlightPrice = '100'
*                                              %control-BookingDate = if_abap_behv=>mk-on
*                                              %control-FlightPrice = if_abap_behv=>mk-on
*                                               ) )
*                         ) )
*             MAPPED DATA(ls_mapped)
*             FAILED DATA(ls_failed)
*             REPORTED DATA(ls_reported).
*
*    cl_abap_unit_assert=>assert_initial( ls_failed ).
*    cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = '0000' act = ls_mapped-booking[ 1 ]-BookingID ).

    " CUT
    read entity /dmo/i_travel_tum
    by \_Booking
     from value #(
                ( travelid = '987'  %control-FlightPrice = if_abap_behv=>mk-on %control-BookingDate = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported data(reported2)
     failed   data(failed2).

    cl_abap_unit_assert=>assert_initial( result ).
    cl_abap_unit_assert=>assert_initial( reported2 ).
    cl_abap_unit_assert=>assert_initial( failed2 ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).

  endmethod.

  method read_invalid_inst_by_assoc_2.
    "  queried travel for booking does not exist

    " Record step to insert a travel instance .
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).


    " CUT
    read entity /dmo/i_travel_tum
    by \_Booking
     from value #(
                ( travelid = '988'  %control-FlightPrice = if_abap_behv=>mk-on %control-BookingDate = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported data(reported2)
     failed   data(failed2).

    cl_abap_unit_assert=>assert_initial( result ).
    cl_abap_unit_assert=>assert_not_initial( failed2 ).

    cl_abap_unit_assert=>assert_equals( msg = 'read should failed' exp = '988' act = failed2-travel[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( msg = 'read should failed' exp = '01' act = failed2-travel[ 1 ]-%assoc-_Booking ).

    " Clear the buffer
    " TODO: API to clear the double transactional buffer.
    modify entities of /dmo/i_travel_tum
     entity travel
       delete from value #( ( %key-TravelID = '987' ) ).

  endmethod.

  method read_invalid_inst_by_assoc_3.
    " No travel entity instance exists in buffer
    " Querying a travel for booking will fail

    " Record step to insert a travel instance .
*     modify entities of /dmo/i_travel_tum
*      entity travel
*        create from value #( ( travelid = '987'
*                               begindate = '20180101' EndDate = '20180101'
*                               totalprice = 100
*                               %control-begindate = if_abap_behv=>mk-on
*                               %control-totalprice = if_abap_behv=>mk-on )
*                           )
*      reported data(reported)
*      failed data(failed)
*      mapped data(mapped).


    " CUT
    read entity /dmo/i_travel_tum
    by \_Booking
     from value #(
                ( travelid = '988'  %control-FlightPrice = if_abap_behv=>mk-on %control-BookingDate = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported data(reported2)
     failed   data(failed2).

    cl_abap_unit_assert=>assert_initial( result ).
    cl_abap_unit_assert=>assert_not_initial( failed2 ).

    cl_abap_unit_assert=>assert_equals( msg = 'read should failed' exp = '988' act = failed2-travel[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( msg = 'read should failed' exp = '01' act = failed2-travel[ 1 ]-%assoc-_Booking ).

  endmethod.

  method report_an_instance_for_modify.

    "Record
    modify entities of /dmo/i_travel_tum
    entity travel
      create from value #( ( travelid = '987'
                            begindate = '20180101' EndDate = '20180101'
                            totalprice = 100
                            %control-begindate = if_abap_behv=>mk-on
                            %control-totalprice = if_abap_behv=>mk-on )
                         )
    reported data(reported)
    failed data(failed)
    mapped data(mapped).

    Clear mapped.
    "Configure the exception
    data: lo_bo_exception type ref to z_bo_mock_exception.
          create object lo_bo_exception.
    reported-travel = value #( ( TravelID = '987' %msg = lo_bo_exception ) ).
    double->config_response_4_modify( operation = if_abap_behv=>op-m-create mapped = mapped reported = reported  failed = failed ).
    "double->config_success_response( mapped = mapped ).
    "Replay - code under test
    modify entities of /dmo/i_travel_tum
    entity travel
      create from value #( ( travelid = '987'
                             begindate = '20180101' EndDate = '20180101'
                            totalprice = 100
                            %control-begindate = if_abap_behv=>mk-on
                            %control-totalprice = if_abap_behv=>mk-on )
                         )
    reported data(reported1)
    failed data(failed1)
    mapped data(mapped1).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines(  reported1-travel ) ).

  endmethod.

  method report_an_instance_for_read.

  "Record
    modify entities of /dmo/i_travel_tum
    entity travel
      create from value #( ( travelid = '987'
                            begindate = '20180101' EndDate = '20180101'
                            totalprice = 100
                            %control-begindate = if_abap_behv=>mk-on
                            %control-totalprice = if_abap_behv=>mk-on )
                         )
    reported data(reported)
    failed data(failed)
    mapped data(mapped).

    data: lo_bo_exception type ref to z_bo_mock_exception.
          create object lo_bo_exception.
    reported-travel = value #( ( TravelID = '987' %msg = lo_bo_exception ) ).

    data read_result type table for read result /DMO/I_Travel_TUM.

    double->config_response_4_read( operation = if_abap_behv=>op-r-read reported = reported failed = failed result = read_result ).

  read entity /dmo/i_travel_tum
    from value #(
               ( travelid = '987' %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
             )
    result   data(result)
    reported data(reported2)
    failed   data(failed2).

   cl_abap_unit_assert=>assert_equals( exp =  reported act = reported2 ).


  endmethod.

  method teardown.
    double->clear_buffer( ).
  endmethod.

endclass.

class ltcl_eml_w_response_config definition final for testing
  duration short
  risk level harmless.

  private section.

    methods create_valid_inst_with_record for testing raising cx_static_check.
    methods create_passes_update_fails for testing raising cx_static_check.
    methods update_inst_mult_mapped_config for testing raising cx_static_check.
    methods create_fails_read_fails for testing raising cx_static_check.
    methods create_fails_read_passes for testing raising cx_static_check.
    methods configure_result_for_read for testing raising cx_static_check.
    methods configure_with_empty_structs for testing raising cx_static_check.
    methods configure_with_invalid_op     for testing raising cx_static_check.
    methods config_res_multi_entities     for testing raising cx_static_check.
    methods mapped_n_failed_configured     for testing raising cx_static_check.

    methods cba_config_mapped for testing raising cx_static_check.
    methods cba_config_failed for testing raising cx_static_check.

    methods rba_config_result for testing raising cx_static_check.
    methods rba_config_failed for testing raising cx_static_check.
    methods config_res_multi_inst for testing raising cx_static_check.
    methods rba_fails_read_pass for testing raising cx_static_check.
    methods config_failed_for_invalid_inst for testing raising cx_static_check.

    methods teardown.
    class-methods class_setup.
    class-data double type ref to zcl_abap_behv_generic_tst_dbl6.
endclass.

class ltcl_eml_w_response_config implementation.

  method class_setup.
    double = new zcl_abap_behv_generic_tst_dbl6( '/dmo/i_travel_tum' ).
    cl_abap_behv_test_environment=>set_test_double( double ).
  endmethod.

  method teardown.
    double->clear_buffer( ).
  endmethod.

  method update_inst_mult_mapped_config.
  " multiple mapped configurations for the same instance and operation(here update).
  " Unsure Behavior

  endmethod.

  method configure_result_for_read.
  "CUT has READ EML which should pass.
  "But returns different values for other field values.
  "Hence requires the use of configure response api for read.

  "Step 1: Record the instance for which READ EML has to be evaluated.
  "Step 2: Configure the read Operation in CUT to succeed.
  "        Use configure_response_for_read api with result as a parameter.

  "Record.
  "Step 1.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

   "Create the result structure.
   "Can we also provide the structure via some api? Or via Using the Read EML

   data result_config type table for read result /dmo/i_travel_tum.
   result_config = value #( ( TravelID = '987'
                       BeginDate = '20180101' EndDate = '20180101'
                       totalprice = 200   " The instance is recorded with the total price 100,
                                          " but the read should return total price 200
                         ) ).

   "Step 2
   double->config_response_4_read( result = result_config operation = if_abap_behv=>op-r-read ).

  "CUT
    read entity /dmo/i_travel_tum
     from value #(
                ( travelid = '987' %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported reported
     failed   failed.

    cl_abap_unit_assert=>assert_equals( msg = 'read failed for travel id 987' exp = '987' act = result[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( msg = 'configuration failed for result' exp = 200 act = result[ 1 ]-TotalPrice ).

  endmethod.

  method create_fails_read_fails.
  "CUT has: CREATE EML which should fail due to a specific reason.
  "         This requires configuring specific response
  "         Consequently READ EML in CUT fails.

  "Step 1: Record the instance for which CREATE and READ EML has to be evaluated.
  "Step 2: Configure the CREATE Operation in CUT to Fail with a specific response.
  "Step 3: Configure the READ Operation in CUT to fail, otherwise it will succeed and read the recorded instance in Step 1.

  "Record.
  "Step 1.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

   "Step 2.
    clear mapped.
    "Configure the exception
    data: lo_bo_exception type ref to z_bo_mock_exception.
          create object lo_bo_exception.
    reported-travel = value #( ( TravelID = '987' %msg = lo_bo_exception ) ).
    failed-travel = value #( ( TravelID = '987' %fail-cause = if_abap_behv=>cause-unauthorized ) ).

    double->config_response_4_modify( operation = if_abap_behv=>op-m-create mapped = mapped failed = failed reported = reported ).

   "Step 3.
    clear mapped.
    "change reported if required.
    "Configure the exception
    reported-travel = value #( ( TravelID = '987' %msg = lo_bo_exception ) ).
    failed-travel = value #( ( TravelID = '987' %fail-cause = if_abap_behv=>cause-not_found ) ).

    double->config_response_4_read( operation = if_abap_behv=>op-r-read failed = failed reported = reported ).


  " CUT
  "CREATE
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported_create)
     failed data(failed_create)
     mapped data(mapped_create).

    cl_abap_unit_assert=>assert_initial( mapped_create ).
    cl_abap_unit_assert=>assert_not_initial( failed_create ).
    cl_abap_unit_assert=>assert_not_initial( reported_create ).

    cl_abap_unit_assert=>assert_equals( msg = 'configuration for create failed'
                                        exp = lo_bo_exception->get_text(  )
                                        act = reported_create-travel[ 1 ]-%msg->if_message~get_text( ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'configuration for create failed'
                                        exp = if_abap_behv=>cause-unauthorized
                                        act = failed_create-travel[ 1 ]-%fail-cause ).

    "READ
    read entities of /dmo/i_travel_tum
    entity travel
     from value #( ( travelid = '987'
                     %control-totalprice = if_abap_behv=>mk-on )
                        )
     result data(read_result)
     reported data(read_reported)
     failed data(read_failed).

    cl_abap_unit_assert=>assert_initial( read_result ).
    cl_abap_unit_assert=>assert_not_initial( read_reported ).
    cl_abap_unit_assert=>assert_not_initial( read_failed ).

    cl_abap_unit_assert=>assert_equals( msg = 'configuration for read failed'
                                        exp = if_abap_behv=>cause-not_found
                                        act = read_failed-travel[ 1 ]-%fail-cause ).

  endmethod.

  method create_fails_read_passes.

  "CUT has: CREATE EML which should fail due to already existing instance.
  "         But READ EML in CUT passes.

  "Step 1: Record the instance for which CREATE has to be evaluated.
  "No further configuration required

  "Record.
  "Step 1.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).


  " CUT
  "CREATE
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported_create)
     failed data(failed_create)
     mapped data(mapped_create).

    cl_abap_unit_assert=>assert_initial( mapped_create ).
    cl_abap_unit_assert=>assert_not_initial( failed_create ).
    cl_abap_unit_assert=>assert_not_initial( reported_create ).

    cl_abap_unit_assert=>assert_equals( msg = 'configuration for create failed'
                                        act = reported_create-travel[ 1 ]-TravelID
                                        exp = '987' ).

    "READ
    read entities of /dmo/i_travel_tum
    entity travel
     from value #( ( travelid = '987'
                     %control-totalprice = if_abap_behv=>mk-on )
                        )
     result data(read_result)
     reported data(read_reported)
     failed data(read_failed).

    cl_abap_unit_assert=>assert_not_initial( read_result ).
    cl_abap_unit_assert=>assert_initial( read_reported ).
    cl_abap_unit_assert=>assert_initial( read_failed ).

    cl_abap_unit_assert=>assert_equals( msg = 'read failed'
                                        exp = read_result[ 1 ]-TotalPrice
                                        act = 100 ).

    cl_abap_unit_assert=>assert_initial( read_result[ 1 ]-BeginDate ). "Created but not requested in read

  endmethod.

  method create_passes_update_fails.
  "CUT has CREATE EML which should pass.
  "And a UPDATE EML which should fail.

  "Step 1: Record the instance for which create and update EML has to be evaluated.
  "Step 2: Configure the Create Operation in CUT to succeed.
  "Step 3: Configure the Update Operation in CUT to fail.

  "Record.
  "Step 1.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

   "Step 2.
    clear reported.
    clear failed.
    "change mapped if required.
    mapped-travel = value #( ( TravelID = '987' ) ).
    double->config_response_4_modify( operation = if_abap_behv=>op-m-create mapped = mapped failed = failed reported = failed ).

   "Step 3.
    clear mapped.
    "change reported if required.
    "Configure the exception
    data: lo_bo_exception type ref to z_bo_mock_exception.
          create object lo_bo_exception.
    reported-travel = value #( ( TravelID = '987' %msg = lo_bo_exception ) ).
    failed-travel = value #( ( TravelID = '987'  ) ).

    double->config_response_4_modify( operation = if_abap_behv=>op-m-update mapped = mapped failed = failed reported = reported ).


  " CUT
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported_cut)
     failed data(failed_cut)
     mapped data(mapped_cut).

    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

    if mapped_cut is not initial.
      modify entities of /dmo/i_travel_tum
      entity travel
       update from value #( ( travelid = '987'
                              totalprice = 200
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
      reported reported_cut
      failed failed_cut
      mapped mapped_cut.

      cl_abap_unit_assert=>assert_initial( mapped_cut ).
      cl_abap_unit_assert=>assert_not_initial( failed_cut ).
      cl_abap_unit_assert=>assert_not_initial( reported_cut ).

    endif.

  endmethod.

  method create_valid_inst_with_record.
  "CUT has create EML.
  "Step 1: Record the instance for which create EML has to be evaluated.
  "Step 2: Configure the Create Operation in CUT to succeed.

  "Record.
  "Step 1.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

   "Step 2.
    clear reported.
    clear failed.
    "change mapped if required.
    mapped-travel = value #( ( TravelID = '987' ) ).
    double->config_response_4_modify( operation = if_abap_behv=>op-m-create mapped = mapped failed = failed reported = reported ).


  " CUT
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported_cut)
     failed data(failed_cut)
     mapped data(mapped_cut).

    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

  endmethod.


  method configure_with_empty_structs.
  " Using configure response api, but mapped/reported/failed are empty
  " What should be the behavior?
  "CUT has create EML.
  "Step 1: Record the instance for which create EML has to be evaluated.
  "Step 2: Use configure API with empty response structures

  "Record.
  "Step 1.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

   "Step 2.
    clear reported.
    clear failed.
    clear mapped.
    "change mapped if required.
"    mapped-travel = value #( ( TravelID = '987' ) ).
    double->config_response_4_modify( operation = if_abap_behv=>op-m-create mapped = mapped failed = failed reported = reported ).


  " CUT
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported_cut)
     failed data(failed_cut)
     mapped data(mapped_cut).

    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).

  endmethod.

  method configure_with_invalid_op.
  " Using configure response api, but with invalid operation.
  " TODO:Current implementation doesn't throw an exception

  "Record.
  "Step 1.
*    modify entities of /dmo/i_travel_tum
*     entity travel
*       create from value #( ( travelid = '987'
*                              begindate = '20180101' EndDate = '20180101'
*                              totalprice = 100
*                              %control-begindate = if_abap_behv=>mk-on
*                              %control-totalprice = if_abap_behv=>mk-on )
*                          )
*     reported data(reported)
*     failed data(failed)
*     mapped data(mapped).
*
*   "Step 2.
*    clear reported.
*    clear failed.
*    "change mapped if required.
*    mapped-travel = value #( ( TravelID = '987' ) ).
*    double->config_response_4_modify( operation = 'K' mapped = mapped failed = failed reported = reported ).

  endmethod.

  method config_res_multi_entities.
  " CUT has Update EML for Travel and Booking instance, both should fail
  " Configure response for Update for travel and booking

  " Use configure_response_for_modify API to configure reported/failed for Update

  "Step 1: Record the instance for which Update EML has to be evaluated.
  "Step 2: Configure the Update Operation in CUT to fail.

  "Step 1
    " Record step to insert a travel instance for which booking is to be created.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    ""Create booking by association
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( ( BookingID = '001'
                                              BookingDate = '20200902'
                                              FlightPrice = 100
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).

    "Step 3:
    "Configure the response for update
    clear ls_mapped.
    clear ls_failed.
    clear ls_reported.

    data: lo_bo_exception type ref to z_bo_mock_exception.
          create object lo_bo_exception.
    ls_reported-booking = value #( ( TravelID = '987' BookingID = '001' %msg = lo_bo_exception ) ).
    ls_reported-travel = value #( ( TravelID = '987' %msg = lo_bo_exception ) ).

    ls_failed-travel = value #( ( TravelID = '987' %fail-cause = if_abap_behv=>cause-unauthorized ) ).
    ls_failed-booking = value #( ( TravelID = '987' BookingID = '001' %fail-cause = if_abap_behv=>cause-unauthorized ) ).

    double->config_response_4_modify( reported = ls_reported failed = ls_failed operation = if_abap_behv=>op-m-update ).

  " CUT
    modify entities of /dmo/i_travel_tum
    entity travel
     update from value #( ( travelid = '987'
                            totalprice = 200
                            %control-totalprice = if_abap_behv=>mk-on )
                        )
    entity booking
      update from value #( ( travelid = '987'
                             BookingID = '001'
                             FlightPrice = 200
                        %control-FlightPrice = if_abap_behv=>mk-on )
                        )
    reported data(reported_cut)
    failed data(failed_cut)
    mapped data(mapped_cut).

    cl_abap_unit_assert=>assert_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_not_initial( reported_cut ).

    cl_abap_unit_assert=>assert_equals( exp = '987'
                                        act = reported_cut-travel[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = reported_cut-booking[ 1 ]-BookingID ).

    cl_abap_unit_assert=>assert_equals( exp = lo_bo_exception->get_text(  )
                                        act = reported_cut-travel[ 1 ]-%msg->if_message~get_text( ) ).
    cl_abap_unit_assert=>assert_equals( exp = lo_bo_exception->get_text(  )
                                        act = reported_cut-booking[ 1 ]-%msg->if_message~get_text( ) ).

    cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-unauthorized
                                        act = failed_cut-travel[ 1 ]-%fail-cause ).
   cl_abap_unit_assert=>assert_equals( exp = if_abap_behv=>cause-unauthorized
                                        act = failed_cut-booking[ 1 ]-%fail-cause ).


  endmethod.

  method config_res_multi_inst.
  "CUT has Create EML for two travel instances.
  " 1 Passes other fails due to a specific reason.
  " Use config_response_4_modify API for both the instances.
  "Step 1: Record the instances for which create EML has to be evaluated.
  "Step 2: Configure the CREATE Operation in CUT to fail
  "        for one inst and fail for other.

  "Step 1
    " Record step to insert two travel instances for which response is to be configured.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          ( travelid = '988'
                              begindate = '20200101' EndDate = '20201201'
                              totalprice = 200
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on ) )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

   "Step 2
    "Configure mapped for successful Create of one instance
    mapped-travel = value #( ( TravelID = '987' ) ).

    "Configure failed/reported for failed Create of another instance
    data: lo_bo_exception type ref to z_bo_mock_exception.
          create object lo_bo_exception.
    reported-travel = value #( ( TravelID = '988' %msg = lo_bo_exception ) ).

    failed-travel = value #( ( TravelID = '988' %fail-cause = if_abap_behv=>cause-unauthorized ) ).

    double->config_response_4_modify( operation = if_abap_behv=>op-m-create mapped = mapped failed = failed reported = reported ).


  " CUT
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          ( travelid = '988'
                              begindate = '20200101' EndDate = '20201201'
                              totalprice = 200
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on ) )
     reported data(reported_cut)
     failed data(failed_cut)
     mapped data(mapped_cut).

    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_not_initial( failed_cut ).
    cl_abap_unit_assert=>assert_not_initial( reported_cut ).

    cl_abap_unit_assert=>assert_equals( msg = 'create failed for Travel ID 987'
                                        exp = '987'
                                        act = mapped_cut-travel[ 1 ]-TravelID ).

    cl_abap_unit_assert=>assert_equals( msg = 'configuration for create failed'
                                        exp = lo_bo_exception->get_text(  )
                                        act = reported_cut-travel[ 1 ]-%msg->if_message~get_text( ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'configuration for create failed'
                                        exp = if_abap_behv=>cause-unauthorized
                                        act = failed_cut-travel[ 1 ]-%fail-cause ).


  endmethod.

  method cba_config_failed.
  "CUT has a Create By Assoc EML which should fail due to authorization issue.
  "    Use configure_response api to configure failed for this operation
  "Step 1: Record the instance for which create by assoc EML has to be evaluated.
  "Step 2: Configure the Create By Assoc Operation in CUT to fail.

  "Step 1
    " Record step to insert a travel instance for which booking is to be created.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    ""Create booking by association
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( ( BookingID = '001'
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).

    "Step 2:
    clear ls_mapped.
    clear ls_failed.
    clear ls_reported.

    "Configure the exception
    data: lo_bo_exception type ref to z_bo_mock_exception.
          create object lo_bo_exception.
    ls_reported-booking = value #( ( TravelID = '987' BookingID = '001' %msg = lo_bo_exception ) ).

    ls_failed-booking = value #( ( TravelID = '987' BookingID = '001' %fail-cause = if_abap_behv=>cause-unauthorized ) ).

    double->config_response_4_modify( operation = if_abap_behv=>op-m-create_ba reported = ls_reported failed = ls_failed  ).

    "CUT
    ""Create booking by association
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( ( BookingID = '001'
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
     reported data(reported_cba)
     failed data(failed_cba)
     mapped data(mapped_cba).

     cl_abap_unit_assert=>assert_initial( mapped_cba ).
     cl_abap_unit_assert=>assert_equals( msg = 'create booking passed' exp = '001' act = reported_cba-booking[ 1 ]-BookingID ).
     cl_abap_unit_assert=>assert_equals( msg = 'create booking passed' exp = '987' act = reported_cba-booking[ 1 ]-TravelID ).
     cl_abap_unit_assert=>assert_equals( msg = 'create booking passed' exp = if_abap_behv=>cause-unauthorized act = failed_cba-booking[ 1 ]-%fail-cause ).

  endmethod.

  method cba_config_mapped.
  "CUT has a Create By Assoc EML which should pass.
  "    Use configure_response api to configure mapped for this operation
  "Step 1: Record the instance for which create by asspc EML has to be evaluated.
  "Step 2: Configure the Create By Assoc Operation in CUT to succeed.

  "Step 1
    " Record step to insert a travel instance for which booking is to be created.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    ""Create booking by association
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( ( BookingID = '001'
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).

    "Step 2:
    clear ls_mapped.
    clear ls_failed.
    clear ls_reported.

    ls_mapped-booking = value #( ( TravelID = '987' BookingID = '001' ) ).

    double->config_response_4_modify( operation = if_abap_behv=>op-m-create_ba mapped = ls_mapped ).

    "CUT
    ""Create booking by association
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( ( BookingID = '001'
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
     reported data(reported_cba)
     failed data(failed_cba)
     mapped data(mapped_cba).

     cl_abap_unit_assert=>assert_initial( reported_cba ).
     cl_abap_unit_assert=>assert_initial( failed_cba ).
     cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = '001' act = mapped_cba-booking[ 1 ]-BookingID ).
     cl_abap_unit_assert=>assert_equals( msg = 'create booking failed' exp = '987' act = mapped_cba-booking[ 1 ]-TravelID ).

  endmethod.

  method rba_config_failed.
  "CUT has a Read By Assoc EML which should fail.
  "    Use configure_response api to configure reported/failed for this operation
  "Step 1: Record the instance for which read by assoc EML has to be evaluated.
  "Step 2: Configure the Read By Assoc Operation in CUT to fail.

  "Step 1
    " Record step to insert a travel instance for which booking is to be created.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    ""Create booking by association
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( ( BookingID = '001'
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).

    "Step 2:
    "Configure the exception
    data: lo_bo_exception type ref to z_bo_mock_exception.
          create object lo_bo_exception.
    ls_reported-booking = value #( ( TravelID = '987' BookingID = '001' %msg = lo_bo_exception ) ).

    ls_failed-booking = value #( ( TravelID = '987' BookingID = '001' %fail-cause = if_abap_behv=>cause-unauthorized ) ).
    double->config_response_4_read( reported = ls_reported failed = ls_failed operation = if_abap_behv=>op-r-read_ba ).

    "CUT
    ""Read booking by association
    read entities of /dmo/i_travel_tum
     entity travel
       by \_Booking
         from value #( ( TravelID = '987'
                         %control-BookingDate = if_abap_behv=>mk-on
                         %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
     result data(result_rba)
     reported data(reported_rba)
     failed data(failed_rba).

     cl_abap_unit_assert=>assert_initial( result_rba ).
     cl_abap_unit_assert=>assert_not_initial( reported_rba ).
     cl_abap_unit_assert=>assert_not_initial( failed_rba ).

     cl_abap_unit_assert=>assert_equals( msg = 'read booking passed' exp = '001' act = reported_rba-booking[ 1 ]-BookingID ).
     cl_abap_unit_assert=>assert_equals( msg = 'read booking passed' exp = '987' act = reported_rba-booking[ 1 ]-TravelID ).
     cl_abap_unit_assert=>assert_equals( msg = 'read booking passed' exp = if_abap_behv=>cause-unauthorized act = failed_rba-booking[ 1 ]-%fail-cause ).


  endmethod.

  method rba_config_result.
  "CUT has a Read By Assoc EML which should pass.
  "    Use configure_response api to configure result for this operation
  "Step 1: Record the instance for which read by assoc EML has to be evaluated.
  "Step 2: Configure the Read By Assoc Operation in CUT to succeed.

  "Step 1
    " Record step to insert a travel instance for which booking is to be created.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    ""Create booking by association
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( ( BookingID = '001'
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).

    "Step 2:
   "Create the result structure.
   "Can we also provide the structure via some api? Or via Using the Read EML

   data result_config type table for read result /dmo/i_booking_tum.
   result_config = value #( ( TravelID = '987'
                              BookingID = '001'
                              BookingDate = '20200902'
                              FlightPrice = '200'   " The instance is recorded with the flight price 100,
                                          " but the read should return flight price 200
                         ) ).

   "Step 2
   double->config_response_4_read( result = result_config operation = if_abap_behv=>op-r-read_ba ).

    "CUT
    ""Read booking by association
    read entities of /dmo/i_travel_tum
     entity travel
       by \_Booking
         from value #( ( TravelID = '987'
                         %control-BookingDate = if_abap_behv=>mk-on
                         %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
     result data(result_rba)
     reported data(reported_rba)
     failed data(failed_rba).

     cl_abap_unit_assert=>assert_initial( reported_rba ).
     cl_abap_unit_assert=>assert_initial( failed_rba ).
     cl_abap_unit_assert=>assert_equals( msg = 'read booking failed' exp = '001' act = result_rba[ 1 ]-BookingID ).
     cl_abap_unit_assert=>assert_equals( msg = 'read booking failed' exp = '987' act = result_rba[ 1 ]-TravelID ).
     cl_abap_unit_assert=>assert_equals( msg = 'read booking failed' exp = 200 act = result_rba[ 1 ]-FlightPrice ).


  endmethod.

  method rba_fails_read_pass.
  "CUT has a READ EML on Parent which passes but returns different value for a field
  "and Read By Assoc EML which should fail.

  "    Use configure_response_for_read API to configure result for read
  "    Use configure_response_for_read API to configure reported/failed for rba

  "Step 1: Record the instance for which read and read by assoc EML has to be evaluated.
  "Step 2: Configure the Read Operation in CUT to pass.
  "Step 3: Configure the Read By Assoc Operation in CUT to fail.

  "Step 1
    " Record step to insert a travel instance for which booking is to be created.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

    ""Create booking by association
    modify entities of /dmo/i_travel_tum
     entity travel
       create by \_Booking
         from value #( ( %key-TravelID = '987'
                         %target = value #( ( BookingID = '001'
                                              BookingDate = '20200902'
                                              FlightPrice = '100'
                                              %control-BookingDate = if_abap_behv=>mk-on
                                              %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
                         ) )
             mapped data(ls_mapped)
             failed data(ls_failed)
             reported data(ls_reported).

    "Step 2
       "Create the result structure.
   "Can we also provide the structure via some api? Or via Using the Read EML

   data result_config type table for read result /dmo/i_travel_tum.
   result_config = value #( ( TravelID = '987'
                       BeginDate = '20180101' EndDate = '20180101'
                       totalprice = 200   " The instance is recorded with the total price 100,
                                          " but the read should return total price 200
                         ) ).

   double->config_response_4_read( result = result_config operation = if_abap_behv=>op-r-read ).

    "Step 3:
    "Configure the exception
    data: lo_bo_exception type ref to z_bo_mock_exception.
          create object lo_bo_exception.
    ls_reported-booking = value #( ( TravelID = '987' BookingID = '001' %msg = lo_bo_exception ) ).

    ls_failed-booking = value #( ( TravelID = '987' BookingID = '001' %fail-cause = if_abap_behv=>cause-unauthorized ) ).
    double->config_response_4_read( reported = ls_reported failed = ls_failed operation = if_abap_behv=>op-r-read_ba ).

    "CUT
    "Read Parent i.e. Travel instance.
    read entity /dmo/i_travel_tum
     from value #(
                ( travelid = '987' %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
              )
     result   data(result)
     reported reported
     failed   failed.

    cl_abap_unit_assert=>assert_equals( msg = 'read failed for travel id 987' exp = '987' act = result[ 1 ]-TravelID ).
    cl_abap_unit_assert=>assert_equals( msg = 'configuration failed for result' exp = 200 act = result[ 1 ]-TotalPrice ).


    ""Read booking by association
    read entities of /dmo/i_travel_tum
     entity travel
       by \_Booking
         from value #( ( TravelID = '987'
                         %control-BookingDate = if_abap_behv=>mk-on
                         %control-FlightPrice = if_abap_behv=>mk-on
                                               ) )
     result data(result_rba)
     reported data(reported_rba)
     failed data(failed_rba).

     cl_abap_unit_assert=>assert_initial( result_rba ).
     cl_abap_unit_assert=>assert_not_initial( reported_rba ).
     cl_abap_unit_assert=>assert_not_initial( failed_rba ).

     cl_abap_unit_assert=>assert_equals( msg = 'read booking passed' exp = '001' act = reported_rba-booking[ 1 ]-BookingID ).
     cl_abap_unit_assert=>assert_equals( msg = 'read booking passed' exp = '987' act = reported_rba-booking[ 1 ]-TravelID ).
     cl_abap_unit_assert=>assert_equals( msg = 'read booking passed' exp = if_abap_behv=>cause-unauthorized act = failed_rba-booking[ 1 ]-%fail-cause ).

  endmethod.


  method mapped_n_failed_configured.
  " If both mapped and failed are configured for an instance+operation
  " an execption must be thrown. (TODO)

  "CUT has create EML.
  "Step 1: Record the instance for which create EML has to be evaluated.
  "Step 2: Configure the Create Operation in CUT to succeed.
  "Step 3: Configure the Create Operation in CUT to fail.

  "Record.
  "Step 1.
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

   "Step 2.
    clear reported.
    clear failed.
    "change mapped if required.
    mapped-travel = value #( ( TravelID = '987' ) ).
    failed-travel = value #( ( TravelID = '987' %fail-cause = if_abap_behv=>cause-unspecific ) ).

    double->config_response_4_modify( operation = if_abap_behv=>op-m-create mapped = mapped failed = failed reported = reported ).


  " CUT
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported_cut)
     failed data(failed_cut)
     mapped data(mapped_cut).

    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).


  endmethod.

  method config_failed_for_invalid_inst.
  " Configure failed for an instance that does not exist in the buffer

  "CUT has create EML.
  "Step 1: Do not record the instance for which create EML has to be evaluated.
  "Step 2: Configure the Create Operation in CUT to fail.

  "Record.
  "Step 1.
  "Record a instance not in CUT
  "This step is to get the failed structure
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported)
     failed data(failed)
     mapped data(mapped).

   "Step 2.
    clear reported.
    clear failed.
    clear mapped.
    "Configure failed for travelID 988 which is not recorded.
    failed-travel = value #( ( TravelID = '988' %fail-cause = if_abap_behv=>cause-unspecific ) ).

    double->config_response_4_modify( operation = if_abap_behv=>op-m-create mapped = mapped failed = failed reported = reported ).

  " CUT
    modify entities of /dmo/i_travel_tum
     entity travel
       create from value #( ( travelid = '987'
                              begindate = '20180101' EndDate = '20180101'
                              totalprice = 100
                              %control-begindate = if_abap_behv=>mk-on
                              %control-totalprice = if_abap_behv=>mk-on )
                          )
     reported data(reported_cut)
     failed data(failed_cut)
     mapped data(mapped_cut).

    cl_abap_unit_assert=>assert_not_initial( mapped_cut ).
    cl_abap_unit_assert=>assert_initial( failed_cut ).
    cl_abap_unit_assert=>assert_initial( reported_cut ).


  endmethod.


endclass.
