:- dynamic meta_user_pred/3.
meta_user_pred(safe_time_out(_8087,_8089,_8091),tools_meta,'.'(meta_arg(1,0),[])).
meta_user_pred(safe_time_out_or_virtual_time_out(_8087,_8089,_8091),tools_meta,'.'(meta_arg(1,0),[])).
meta_user_pred(call_residue(_8087,_8089),tools_meta,'.'(meta_arg(1,0),[])).
meta_user_pred(safe_on_exception(_8087,_8089,_8091),tools_meta,'.'(meta_arg(2,0),'.'(meta_arg(3,0),[]))).
meta_user_pred(safe_on_exception_silent(_8087,_8089,_8091),tools_meta,'.'(meta_arg(2,0),'.'(meta_arg(3,0),[]))).
meta_user_pred(det_call_cleanup(_8087,_8089),tools_meta,'.'(meta_arg(1,0),'.'(meta_arg(2,0),[]))).
meta_user_pred(catch_matching(_8087,_8089,_8091),tools_meta,'.'(meta_arg(1,0),'.'(meta_arg(3,0),[]))).
meta_user_pred(catch_call(_8087),tools,'.'(meta_arg(1,0),[])).
meta_user_pred(nonvar_call(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(assert_pre(_8087,_8089),self_check,'.'(meta_arg(1,0),'.'(meta_arg(2,0),[]))).
meta_user_pred(assert_post(_8087,_8089),self_check,'.'(meta_arg(1,0),'.'(meta_arg(2,0),[]))).
meta_user_pred(assert_must_succeed_any(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(assert_must_succeed(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(assert_must_succeed_multiple(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(assert_must_fail(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(~~(_8087),debug,'.'(meta_arg(1,0),[])).
meta_user_pred(timer_call(_8087),debug,'.'(meta_arg(1,0),[])).
meta_user_pred(timer_call(_8087,_8089),debug,'.'(meta_arg(2,0),[])).
meta_user_pred(time_if_debug(_8087),debug,'.'(meta_arg(1,0),[])).
meta_user_pred(time(_8087),debug,'.'(meta_arg(1,0),[])).
meta_user_pred(watch_det(_8087,_8089),debug,'.'(meta_arg(2,0),[])).
meta_user_pred(watch(_8087),debug,'.'(meta_arg(1,0),[])).
meta_user_pred(watch(_8087,_8089),debug,'.'(meta_arg(2,0),[])).
meta_user_pred(det_check(_8087),debug,'.'(meta_arg(1,0),[])).
meta_user_pred(det_check(_8087,_8089),debug,'.'(meta_arg(1,0),'.'(meta_arg(2,0),[]))).
meta_user_pred(if_det_check(_8087,_8089,_8091),debug,'.'(meta_arg(1,0),'.'(meta_arg(2,0),'.'(meta_arg(3,0),[])))).
meta_user_pred(if_det_check_pp(_8087,_8089,_8091,_8093),debug,'.'(meta_arg(1,0),'.'(meta_arg(2,0),'.'(meta_arg(3,0),[])))).
meta_user_pred(add_failed_call_error(_8087),error_manager,'.'(meta_arg(1,0),[])).
%meta_user_pred(add_internal_error(_8087,_8089),error_manager,'.'(meta_arg(2,0),[])).
meta_user_pred(call_in_fresh_error_scope_for_one_solution(_8087),error_manager,'.'(meta_arg(1,0),[])).
meta_user_pred(on_enumeration_warning(_8087,_8089),error_manager,'.'(meta_arg(1,0),'.'(meta_arg(2,0),[]))).
meta_user_pred(on_enumeration_warning_with_continue(_8087,_8089,_8091),error_manager,'.'(meta_arg(1,0),'.'(meta_arg(2,0),'.'(meta_arg(3,0),[])))).
meta_user_pred(catch_enumeration_warning_exceptions(_8087,_8089),error_manager,'.'(meta_arg(1,0),'.'(meta_arg(2,0),[]))).
meta_user_pred(catch_enumeration_warning_exceptions(_8087,_8089,_8091),error_manager,'.'(meta_arg(1,0),'.'(meta_arg(2,0),[]))).
meta_user_pred(observe_enumeration_warnings(_8087,_8089),error_manager,'.'(meta_arg(1,0),'.'(meta_arg(2,0),[]))).
meta_user_pred(call_with_enumeration_warning(_8087),error_manager,'.'(meta_arg(1,0),[])).
meta_user_pred(time_out_with_enum_warning_one_solution(_8087,_8089,_8091),error_manager,'.'(meta_arg(1,0),[])).
meta_user_pred(time_out_with_enum_warning_one_solution_no_new_error_scope(_8087,_8089,_8091),error_manager,'.'(meta_arg(1,0),[])).
meta_user_pred(time_out_with_enum_warning_for_findall(_8087,_8089,_8091),error_manager,'.'(meta_arg(1,0),[])).
meta_user_pred(time_out_with_enum_warning_for_findall2(_8087,_8089,_8091,_8093),error_manager,'.'(meta_arg(2,0),[])).
meta_user_pred(time_out_with_enum_warning_for_findall_in_current_error_scope(_8087,_8089,_8091,_8093),error_manager,'.'(meta_arg(2,0),[])).
meta_user_pred(assert_true(_8087),error_manager,'.'(meta_arg(1,0),[])).
meta_user_pred(pp_mnf(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(pp_cll(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(mnf(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(mnf(_8087,_8089),self_check,'.'(meta_arg(2,0),[])).
meta_user_pred(det_call(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(mnf_call(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(mnf_call_with_pp(_8087,_8089),self_check,'.'(meta_arg(2,0),[])).
meta_user_pred(prepost_mnf_call(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(prepost_call(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(check_exception_call(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(rt_timeout_call(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(mnf_det(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(force_det_call(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(residue_check_call(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(must_fail(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(must_succeed(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(must_succeed_without_residue(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(must_succeed_multiple_without_residue(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(safe_call(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(register_event_listener(_8087,_8089,_8091),eventhandling,'.'(meta_arg(2,0),[])).
meta_user_pred(call_for_event(_8087,_8089),eventhandling,'.'(meta_arg(2,0),[])).
meta_user_pred(check_deterministic(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(check_det(_8087),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(check_det2(_8087,_8089),self_check,'.'(meta_arg(1,0),[])).
meta_user_pred(filter(_8087,_8089,_8091,_8093),tools,'.'(meta_arg(1,1),[])).
meta_user_pred(get_options(_8087,_8089,_8091,_8093),tools,'.'(meta_arg(2,4),[])).
meta_user_pred(get_options(_8087,_8089,_8091,_8093,_8095),tools,'.'(meta_arg(2,4),'.'(meta_arg(5,0),[]))).
meta_user_pred(retract_all_count(_8087,_8089,_8091),tools,'.'(meta_arg(1,0),[])).
meta_user_pred(space_call(_8087),tools,'.'(meta_arg(1,0),[])).
meta_user_pred(split_list(_8087,_8089,_8091,_8093),tools,'.'(meta_arg(1,1),[])).
meta_user_pred(split_list2(_8087,_8089,_8091,_8093),tools,'.'(meta_arg(2,1),[])).
meta_user_pred(split_list_idx(_8087,_8089,_8091,_8093,_8095),tools,'.'(meta_arg(1,1),[])).
meta_user_pred(split_list_idx2(_8087,_8089,_8091,_8093,_8095),tools,'.'(meta_arg(2,1),[])).
meta_user_pred(foldl(_8087,_8089,_8091,_8093),tools,'.'(meta_arg(1,3),[])).
meta_user_pred(foldl2(_8087,_8089,_8091,_8093),tools,'.'(meta_arg(2,3),[])).
meta_user_pred(foldl(_8087,_8089,_8091,_8093,_8095),tools,'.'(meta_arg(1,4),[])).
meta_user_pred(foldl2(_8087,_8089,_8091,_8093,_8095),tools,'.'(meta_arg(2,4),[])).
meta_user_pred(foldl(_8087,_8089,_8091,_8093,_8095,_8097),tools,'.'(meta_arg(1,5),[])).
meta_user_pred(foldl2(_8087,_8089,_8091,_8093,_8095,_8097),tools,'.'(meta_arg(2,5),[])).
meta_user_pred(assert_once(_8087),tools,'.'(meta_arg(1,0),[])).
meta_user_pred(call_with_preference(_8087,_8089,_8091),preferences,'.'(meta_arg(1,0),[])).
meta_user_pred(assert_must_abort_wf(_8087,_8089),kernel_waitflags,'.'(meta_arg(1,0),[])).
meta_user_pred(post_constraint(_8087),clpfd_interface,'.'(meta_arg(1,0),[])).
meta_user_pred(post_constraint(_8087,_8089),clpfd_interface,'.'(meta_arg(1,0),'.'(meta_arg(2,0),[]))).
meta_user_pred(post_constraint2(_8087,_8089),clpfd_interface,'.'(meta_arg(1,0),[])).
meta_user_pred(time_out_constraint(_8087,_8089),clpfd_interface,'.'(meta_arg(1,0),'.'(meta_arg(2,0),[]))).
meta_user_pred(catch_clpfd_overflow_call1(_8087),clpfd_interface,'.'(meta_arg(1,0),[])).
meta_user_pred(catch_clpfd_overflow_call2(_8087,_8089),clpfd_interface,'.'(meta_arg(1,0),'.'(meta_arg(2,0),[]))).
meta_user_pred(perfmessagecall(_8087,_8089),performance_messages,'.'(meta_arg(2,0),[])).
meta_user_pred(perfmessagecall(_8087,_8089,_8091),performance_messages,'.'(meta_arg(2,0),[])).
meta_user_pred(my_findall(_8087,_8089,_8091,_8093),delay,'.'(meta_arg(2,0),[])).
meta_user_pred(my_findall_catch(_8087,_8089,_8091,_8093),delay,'.'(meta_arg(2,0),[])).
meta_user_pred(my_findall_catch(_8087,_8089,_8091,_8093,_8095),delay,'.'(meta_arg(2,0),[])).
meta_user_pred(my_findall_check(_8087,_8089,_8091,_8093,_8095,_8097),delay,'.'(meta_arg(2,0),'.'(meta_arg(5,0),'.'(meta_arg(6,0),[])))).
meta_user_pred(apply_rewrite_rule_with_rename(_8087,_8089,_8091,_8093,_8095,_8097,_8099,_8101),b_ast_cleanup_rewrite_rules,'.'(meta_arg(1,8),[])).
meta_user_pred(mycall(_8087,_8089,_8091,_8093,_8095,_8097,_8099,_8101,_8103),b_ast_cleanup_rewrite_rules,'.'(meta_arg(1,8),[])).
meta_user_pred(wd_delay(_8087,_8089,_8091,_8093),b_interpreter_check,'.'(meta_arg(1,0),[])).
meta_user_pred(wd_delay_block(_8087,_8089,_8091,_8093,_8095,_8097),b_interpreter_check,'.'(meta_arg(1,0),[])).
meta_user_pred(safe_call(_8087,_8089),external_functions,'.'(meta_arg(1,0),[])).
meta_user_pred(succeed_max_call(_8087,_8089),succeed_max,'.'(meta_arg(1,0),[])).
meta_user_pred(succeed_max_call_id(_8087,_8089,_8091),succeed_max,'.'(meta_arg(2,0),[])).
meta_user_pred(open_cache_file(_8087,_8089,_8091,_8093),value_persistance,'.'(meta_arg(4,0),[])).
meta_user_pred(show_cache_file_contents_for_machine(_8087,_8089,_8091,_8093),value_persistance,'.'(meta_arg(2,0),[])).
meta_user_pred(bv_time_out_call(_8087,_8089,_8091,_8093),bvisual2,'.'(meta_arg(1,0),[])).
meta_user_pred(findall_keepvars(_8087,_8089,_8091),haskell_csp_analyzer,'.'(meta_arg(2,0),[])).
meta_user_pred(read_compiled_prolog_file(_8087,_8089,_8091),haskell_csp,'.'(meta_arg(3,1),[])).
meta_user_pred(user_interruptable_call_det(_8087,_8089),user_signal,'.'(meta_arg(1,0),[])).
meta_user_pred(protect_from_user_interrupt_det(_8087),user_signal,'.'(meta_arg(1,0),[])).
meta_user_pred(ignore_user_interrupt_det(_8087),user_signal,'.'(meta_arg(1,0),[])).
meta_user_pred(catch_interrupt_exception(_8087,_8089),user_signal,'.'(meta_arg(1,0),[])).
meta_user_pred(catch_interrupt_assertion_call(_8087),user_interrupts,'.'(meta_arg(1,0),[])).
meta_user_pred(interruptable_call(_8087),user_interrupts,'.'(meta_arg(1,0),[])).
meta_user_pred(interruptable_call(_8087,_8089),user_interrupts,'.'(meta_arg(1,0),[])).
meta_user_pred(maplist5(_8087,_8089,_8091,_8093,_8095),predicate_debugger,'.'(meta_arg(1,4),[])).
meta_user_pred(call_with_temp_preference(_8087,_8089,_8091),bvisual2,'.'(meta_arg(3,0),[])).
meta_user_pred(kernel_call_or(_8087,_8089,_8091,_8093,_8095),kernel_mappings,'.'(meta_arg(1,0),[])).
meta_user_pred(must_succ(_8087,_8089),kernel_mappings,'.'(meta_arg(1,0),[])).
meta_user_pred(time_out_with_factor_call(_8087,_8089,_8091),tools_timeout,'.'(meta_arg(1,0),'.'(meta_arg(3,0),[]))).
meta_user_pred(time_out_call(_8087,_8089),tools_timeout,'.'(meta_arg(1,0),'.'(meta_arg(2,0),[]))).
meta_user_pred(time_out_call(_8087),tools_timeout,'.'(meta_arg(1,0),[])).
meta_user_pred(cvc4_interface_call(_8087),cvc4interface,[]).
meta_user_pred(z3_interface_call(_8087),z3interface,[]).
meta_user_pred(smt_solver_interface_call(_8087,_8089),solver_dispatcher,[]).
meta_user_pred(call_with_smt_mode_enabled(_8087),solver_interface,'.'(meta_arg(1,0),[])).
meta_user_pred(try_call(_8087,_8089,_8091),kodkod,'.'(meta_arg(3,0),[])).
meta_user_pred(profile_single_call(_8087,_8089,_8091),runtime_profiler,'.'(meta_arg(3,0),[])).
meta_user_pred(profile_single_call(_8087,_8089),runtime_profiler,'.'(meta_arg(2,0),[])).
meta_user_pred(apply_transformation_step(_8087,_8089,_8091,_8093),bmachine,'.'(meta_arg(2,2),[])).
meta_user_pred(delay_setof_with_explicit_waitvars(_8087,_8089,_8091,_8093),delay,'.'(meta_arg(2,0),[])).
meta_user_pred(delay_setof(_8087,_8089,_8091,_8093,_8095),delay,'.'(meta_arg(2,0),[])).
meta_user_pred(block_my_findall_catch(_8087,_8089,_8091,_8093,_8095),delay,'.'(meta_arg(3,0),[])).
meta_user_pred(delay_setof_check(_8087,_8089,_8091,_8093,_8095,_8097,_8099),delay,'.'(meta_arg(2,0),'.'(meta_arg(6,0),'.'(meta_arg(7,0),[])))).
meta_user_pred(block_findall_check(_8087,_8089,_8091,_8093,_8095,_8097,_8099,_8101),delay,'.'(meta_arg(3,0),'.'(meta_arg(5,0),'.'(meta_arg(6,0),[])))).
meta_user_pred(delay_setof_list(_8087,_8089,_8091,_8093),delay,'.'(meta_arg(2,0),[])).
meta_user_pred(block_my_findall_sort(_8087,_8089,_8091,_8093),delay,'.'(meta_arg(3,0),[])).
meta_user_pred(delay_call(_8087,_8089,_8091),delay,'.'(meta_arg(1,0),[])).
meta_user_pred(delay_call(_8087,_8089),delay,'.'(meta_arg(1,0),[])).
meta_user_pred(delay_not(_8087,_8089),delay,'.'(meta_arg(1,0),[])).
meta_user_pred(not_with_enum_warning(_8087),delay,'.'(meta_arg(1,0),[])).
meta_user_pred(not_with_enum_warning2(_8087,_8089),delay,'.'(meta_arg(1,0),[])).
meta_user_pred(map_over_full_bexpr_no_fail(_8087,_8089),bsyntaxtree,'.'(meta_arg(1,1),[])).
meta_user_pred(map_over_bexpr(_8087,_8089),bsyntaxtree,'.'(meta_arg(1,1),[])).
meta_user_pred(map_over_typed_bexpr(_8087,_8089),bsyntaxtree,'.'(meta_arg(1,1),[])).
meta_user_pred(map_over_typed_bexpr(_8087,_8089,_8091),bsyntaxtree,'.'(meta_arg(1,2),[])).
meta_user_pred(map_over_bexpr_top_down_acc(_8087,_8089,_8091),bsyntaxtree,'.'(meta_arg(1,3),[])).
meta_user_pred(map_over_type_bexpr_top_down_acc(_8087,_8089,_8091),bsyntaxtree,'.'(meta_arg(1,3),[])).
meta_user_pred(reduce_over_bexpr(_8087,_8089,_8091,_8093),bsyntaxtree,'.'(meta_arg(1,3),[])).
meta_user_pred(transform_bexpr(_8087,_8089,_8091),bsyntaxtree,'.'(meta_arg(1,2),[])).
meta_user_pred(l_transform_bexpr(_8087,_8089,_8091),bsyntaxtree,'.'(meta_arg(2,2),[])).
meta_user_pred(transform_bexpr_with_bup_accs(_8087,_8089,_8091,_8093,_8095),bsyntaxtree,'.'(meta_arg(1,4),[])).
meta_user_pred(l_transform_bexpr_with_bup_accs(_8087,_8089,_8091,_8093,_8095),bsyntaxtree,'.'(meta_arg(2,4),[])).
meta_user_pred(transform_bexpr_with_acc(_8087,_8089,_8091,_8093,_8095),bsyntaxtree,'.'(meta_arg(1,4),[])).
meta_user_pred(l_transform_bexpr_with_acc(_8087,_8089,_8091,_8093,_8095),bsyntaxtree,'.'(meta_arg(2,4),[])).
meta_user_pred(map_over(_8087,_8089,_8091),translate_keywords,'.'(meta_arg(1,2),[])).
meta_user_pred(map_over_raw_expr(_8087,_8089,_8091),translate_keywords,'.'(meta_arg(2,2),[])).
meta_user_pred(catch_enumeration_warning(_8087,_8089),enabling_analysis,'.'(meta_arg(1,0),'.'(meta_arg(2,0),[]))).
meta_user_pred(c_ltl_modelcheck(_8087,_8089,_8091,_8093),ltlc,'.'(meta_arg(4,5),[])).
meta_user_pred(evaluate_ltl_formula(_8087,_8089,_8091,_8093,_8095,_8097),ltl_verification,'.'(meta_arg(4,2),'.'(meta_arg(5,3),[]))).
meta_user_pred(evaluate_ltl_fairness(_8087,_8089,_8091,_8093,_8095,_8097),ltl_verification,'.'(meta_arg(4,3),'.'(meta_arg(5,2),[]))).
meta_user_pred(find_enabled_fairids_for_state(_8087,_8089,_8091),ltl_verification,'.'(meta_arg(1,3),[])).
meta_user_pred(find_enabled_fairids(_8087,_8089,_8091,_8093),ltl_verification,'.'(meta_arg(2,3),[])).
meta_user_pred(eval_fairness(_8087,_8089,_8091,_8093,_8095),ltl_verification,'.'(meta_arg(3,3),'.'(meta_arg(4,2),[]))).
meta_user_pred(is_executed(_8087,_8089,_8091),ltl_verification,'.'(meta_arg(1,2),[])).
meta_user_pred(is_executed2(_8087,_8089,_8091),ltl_verification,'.'(meta_arg(1,2),[])).
meta_user_pred(cltl2ba(_8087,_8089,_8091,_8093,_8095,_8097),ltl2ba,[]).
meta_user_pred(reduce_state_space(_8087,_8089),state_space_reduction,'.'(meta_arg(1,3),'.'(meta_arg(2,5),[]))).
meta_user_pred(reduce_states(_8087,_8089),state_space_reduction,'.'(meta_arg(1,3),[])).
meta_user_pred(sm_node_pred(_8087,_8089,_8091,_8093,_8095,_8097,_8099),state_space_reduction,'.'(meta_arg(1,2),[])).
meta_user_pred(defspec_pred(_8087,_8089),plspec,'.'(meta_arg(2,1),[])).
meta_user_pred(defspec_pred_recursive(_8087,_8089,_8091,_8093),plspec,'.'(meta_arg(2,3),'.'(meta_arg(3,3),'.'(meta_arg(4,4),[])))).
meta_user_pred(defspec_connective(_8087,_8089,_8091,_8093),plspec,'.'(meta_arg(2,3),'.'(meta_arg(3,3),'.'(meta_arg(4,4),[])))).
meta_user_pred(set_error_handler(_8087),plspec,'.'(meta_arg(1,1),[])).
meta_user_pred(plspec_some(_8087,_8089),plspec,'.'(meta_arg(1,1),[])).
meta_user_pred(probcli_clpfd_overflow_mnf_call1(_8087),eval_strings,'.'(meta_arg(1,0),[])).
meta_user_pred(probcli_clpfd_overflow_call1(_8087),eval_strings,'.'(meta_arg(1,0),[])).
meta_user_pred(maplist(_8087,_8089,_8091,_8093,_8095),predicate_handling,'.'(meta_arg(1,4),[])).
meta_user_pred(catch_enumeration_warning(_8087,_8089),static_analysis,'.'(meta_arg(1,0),'.'(meta_arg(2,0),[]))).
meta_user_pred(catch_and_ignore_well_definedness_error(_8087,_8089),static_analysis,'.'(meta_arg(1,0),[])).
meta_user_pred(maxsolver(_8087,_8089,_8091),maxsolver,'.'(meta_arg(1,1),[])).
meta_user_pred(maxsolver(_8087,_8089,_8091,_8093),maxsolver,'.'(meta_arg(2,1),[])).
meta_user_pred(maxsolver_by_longest_prefix(_8087,_8089,_8091),maxsolver,'.'(meta_arg(1,1),[])).
meta_user_pred(x_append_and_eval(_8087,_8089,_8091),maxsolver,'.'(meta_arg(1,1),[])).
meta_user_pred(x_prepend_and_eval(_8087,_8089,_8091),maxsolver,'.'(meta_arg(1,1),[])).
meta_user_pred(longest_satisfiable_prefix(_8087,_8089,_8091,_8093),maxsolver,'.'(meta_arg(1,1),[])).
meta_user_pred(x_longest_satisfiable_prefix(_8087,_8089,_8091,_8093),maxsolver,'.'(meta_arg(1,1),[])).
meta_user_pred(satisfiable_segment(_8087,_8089,_8091,_8093,_8095),maxsolver,'.'(meta_arg(1,1),[])).
meta_user_pred(longest_satisfiable_segment(_8087,_8089,_8091,_8093,_8095),maxsolver,'.'(meta_arg(1,1),[])).
meta_user_pred(maxsolver_by_longest_segment(_8087,_8089,_8091),maxsolver,'.'(meta_arg(1,1),[])).
meta_user_pred(x_maxsolver_by_longest_segment(_8087,_8089,_8091,_8093,_8095),maxsolver,'.'(meta_arg(1,1),[])).
meta_user_pred(maxsolver_exact_with_marker(_8087,_8089,_8091),maxsolver,'.'(meta_arg(1,1),[])).
meta_user_pred(x_precalc(_8087,_8089,_8091,_8093,_8095),maxsolver,'.'(meta_arg(1,1),[])).
meta_user_pred(x_false_subset_til_true(_8087,_8089,_8091,_8093,_8095),maxsolver,'.'(meta_arg(1,1),[])).
meta_user_pred(x_existis_satisfiable_subset_of_length(_8087,_8089,_8091,_8093,_8095),maxsolver,'.'(meta_arg(1,1),[])).
meta_user_pred(x_satisfiable_subset_of_length(_8087,_8089,_8091,_8093,_8095),maxsolver,'.'(meta_arg(1,1),[])).
meta_user_pred(x_maxsolver_exact_with_marker(_8087,_8089,_8091,_8093,_8095,_8097),maxsolver,'.'(meta_arg(1,1),[])).
meta_user_pred(x_false_subset(_8087,_8089,_8091,_8093),maxsolver,'.'(meta_arg(1,1),[])).
meta_user_pred(visit_tree(_8087,_8089,_8091,_8093,_8095),bvisual,'.'(meta_arg(1,3),'.'(meta_arg(2,2),[]))).
meta_user_pred(mnf1(_8087),ltsmin,'.'(meta_arg(1,0),[])).
meta_user_pred(print_single_enable_graph(_8087,_8089),flow,'.'(meta_arg(2,3),[])).
meta_user_pred(print_single_enable_graph(_8087,_8089,_8091),flow,'.'(meta_arg(2,3),[])).
meta_user_pred(printtime(_8087),ctl,'.'(meta_arg(1,0),[])).
meta_user_pred(take_while(_8087,_8089,_8091),ltsmin_trace,'.'(meta_arg(1,1),[])).
meta_user_pred(take_while1(_8087,_8089,_8091),ltsmin_trace,'.'(meta_arg(2,1),[])).
meta_user_pred(call_pred_on_expanded_state(_8087,_8089,_8091,_8093),user,'.'(meta_arg(1,3),[])).
meta_user_pred(map_over_history(_8087,_8089),user,'.'(meta_arg(1,3),[])).
meta_user_pred(catch_clpfd_overflow_call_for_state(_8087,_8089,_8091),user,'.'(meta_arg(2,0),'.'(meta_arg(3,0),[]))).
meta_user_pred(register_conjunct_error_hook(_8087),predicate_evaluator,'.'(meta_arg(1,5),[])).
meta_user_pred(analyse_quick_time_out(_8087),predicate_evaluator,'.'(meta_arg(1,0),[])).
meta_user_pred(time_out_and_catch_errors(_8087,_8089,_8091),predicate_evaluator,'.'(meta_arg(1,0),[])).
meta_user_pred(tcltk_time_call(_8087),user,'.'(meta_arg(1,0),[])).
meta_user_pred(add_csp_process_id1(_8087,_8089,_8091),user,'.'(meta_arg(3,1),[])).
meta_user_pred(start_worker(_8087,_8089,_8091,_8093,_8095,_8097),worker,'.'(meta_arg(6,1),[])).
meta_user_pred(measured_call(_8087,_8089),kodkod_test,'.'(meta_arg(1,0),[])).
meta_user_pred(check(_8087,_8089),kodkod_test,'.'(meta_arg(1,0),[])).
meta_user_pred(multiple_times(_8087,_8089,_8091,_8093),smtlib2_parser,'.'(meta_arg(1,3),[])).
meta_user_pred(at_least_once(_8087,_8089,_8091,_8093),smtlib2_parser,'.'(meta_arg(1,3),[])).
meta_user_pred(multiple_times_no_whitespace(_8087,_8089,_8091,_8093),smtlib2_parser,'.'(meta_arg(1,3),[])).
meta_user_pred(at_least_once_no_whitespace(_8087,_8089,_8091,_8093),smtlib2_parser,'.'(meta_arg(1,3),[])).
meta_user_pred(wall(_8087),disprover_test_runner,'.'(meta_arg(1,0),[])).
meta_user_pred(if_option_set(_8087,_8089),user,'.'(meta_arg(2,0),[])).
meta_user_pred(if_option_set(_8087,_8089,_8091),user,'.'(meta_arg(2,0),'.'(meta_arg(3,0),[]))).
meta_user_pred(if_options_set(_8087,_8089),user,'.'(meta_arg(2,0),[])).
meta_user_pred(if_option_set_loaded(_8087,_8089,_8091),user,'.'(meta_arg(3,0),[])).
meta_user_pred(ifm_option_set(_8087,_8089),user,'.'(meta_arg(2,0),[])).
meta_user_pred(ifm_option_set_loaded(_8087,_8089,_8091),user,'.'(meta_arg(3,0),[])).
meta_user_pred(timeout_call(_8087,_8089),user,'.'(meta_arg(1,0),[])).
meta_user_pred(call_probcli_option(_8087),user,'.'(meta_arg(1,0),[])).
meta_user_pred(map_translate(_8087,_8089,_8091,_8093),alloy2b,'.'(meta_arg(1,3),[])).
meta_user_pred(maplist5(_8087,_8089,_8091,_8093,_8095),b_read_write_info,'.'(meta_arg(1,4),[])).
meta_user_pred(delay_setof_wf(_8087,_8089,_8091,_8093,_8095,_8097),delay,'.'(meta_arg(2,0),[])).
meta_user_pred(delay_setof_check_wf(_8087,_8089,_8091,_8093,_8095,_8097,_8099,_8101),delay,'.'(meta_arg(2,0),'.'(meta_arg(6,0),'.'(meta_arg(7,0),[])))).
meta_user_pred(block_findall_check(_8087,_8089,_8091,_8093,_8095,_8097,_8099,_8101,_8103),delay,'.'(meta_arg(3,0),'.'(meta_arg(5,0),'.'(meta_arg(6,0),[])))).
meta_user_pred(my_findall_check(_8087,_8089,_8091,_8093,_8095,_8097,_8099),delay,'.'(meta_arg(2,0),'.'(meta_arg(5,0),'.'(meta_arg(6,0),[])))).
meta_user_pred(delay_setof_wf(_8087,_8089,_8091,_8093,_8095,_8097,_8099),delay,'.'(meta_arg(2,0),[])).
meta_user_pred(delay_setof_check_wf(_8087,_8089,_8091,_8093,_8095,_8097,_8099,_8101,_8103),delay,'.'(meta_arg(2,0),'.'(meta_arg(6,0),'.'(meta_arg(7,0),[])))).
meta_user_pred(block_findall_check(_8087,_8089,_8091,_8093,_8095,_8097,_8099,_8101,_8103,_8105),delay,'.'(meta_arg(3,0),'.'(meta_arg(5,0),'.'(meta_arg(6,0),[])))).
meta_user_pred(not_with_enum_warning_delay(_8087,_8089),b_interpreter,'.'(meta_arg(1,0),[])).
meta_user_pred(not_with_enum_warning_and_possible_delay(_8087,_8089),b_interpreter,'.'(meta_arg(1,0),[])).
meta_user_pred(try_post_constraint(_8087),clpfd_interface,'.'(meta_arg(1,0),[])).
meta_user_pred(my_findall_catch(_8087,_8089,_8091,_8093,_8095,_8097),delay,'.'(meta_arg(2,0),[])).
meta_user_pred(myprofile(_8087,_8089,_8091),prob2_interface,'.'(meta_arg(3,0),[])).
%meta_user_pred(call_residue_check(_8087),predicate_evaluator,[]).
meta_user_pred(start_worker(_8087,_8089,_8091,_8093,_8095),worker,'.'(meta_arg(5,1),[])).
meta_user_pred(add_new_event_in_error_scope(_8087,_8089),error_manager,'.'(meta_arg(2,2),[])).
meta_user_pred(delay_not(_8087,_8089,_8091),delay,'.'(meta_arg(1,0),[])).
meta_user_pred(not_with_enum_warning(_8087,_8089),delay,'.'(meta_arg(1,0),[])).
meta_user_pred(not_with_enum_warning2(_8087,_8089,_8091),delay,'.'(meta_arg(1,0),[])).
meta_user_pred(wd_delay_until_needed(_8087,_8089),b_interpreter_check,'.'(meta_arg(2,0),[])).
meta_user_pred(wd_delay_until_needed_block(_8087,_8089,_8091,_8093),b_interpreter_check,'.'(meta_arg(3,0),'.'(meta_arg(4,0),[]))).
meta_user_pred(module_info(_8087,_8089),module_information,[]).
meta_user_pred(add_internal_error(_8087,_8089),error_manager,[]).
meta_user_pred(call_residue_check(_8087),predicate_evaluator,'.'(meta_arg(1,0),[])).
