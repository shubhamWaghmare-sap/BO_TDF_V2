*"* use this source file for your ABAP unit test classes
class ltcl_modify_eml definition final for testing
  duration short
  risk level harmless.

  private section.


    methods report_an_instance_for_modify for testing raising cx_static_check.
    methods report_an_instance_for_read for testing raising cx_static_check.



















    class-methods class_setup.
    class-data double type ref to zcl_abap_behv_generic_tst_dbl4.
endclass.


class ltcl_modify_eml implementation.

  method class_setup.
    double = new zcl_abap_behv_generic_tst_dbl4( '/dmo/i_travel_tum' ).
    cl_abap_behv_test_environment=>set_test_double( double ).
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

    "Configure the exception
    data: lo_bo_exception type ref to z_bo_mock_exception.
          create object lo_bo_exception.
    reported-travel = value #( ( TravelID = '987' %msg = lo_bo_exception ) ).
    double->config_error_response_4_modify( reported = reported  failed = failed ).

    "Replay
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

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines(  reported1-travel ) ).

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
    double->config_error_response_4_read( reported = reported failed = failed ).

  read entity /dmo/i_travel_tum
    from value #(
               ( travelid = '987' %control-begindate = if_abap_behv=>mk-on %control-totalprice = if_abap_behv=>mk-on )
             )
    result   data(result)
    reported data(reported2)
    failed   data(failed2).

   cl_abap_unit_assert=>assert_equals( exp =  reported act = reported2 ).

  endmethod.


endclass.
