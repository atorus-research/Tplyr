# Implementation Plan: Refactor evalq() Pattern

## Overview

This implementation plan breaks down the refactoring of Tplyr's `evalq()` usage into discrete, manageable tasks. Each task builds incrementally on previous work, with checkpoints to ensure tests pass throughout the process.

## Task List

- [x] 1. Preparation and Setup
  - Document all current `evalq()` usage locations
  - Establish performance baseline for key functions
  - Verify test suite is comprehensive and passing
  - Create refactoring branch
  - _Requirements: All requirements (foundation)_

- [x] 2. Refactor treatment_group_build()
  - Extract bindings at function start (target, treat_var, pop_data, etc.)
  - Move all processing logic to function environment
  - Explicitly bind results (built_target, built_pop_data) at function end
  - Remove evalq() wrapper
  - Verify no temporary variables remain in table environment
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6, 3.1-3.8_

- [x] 2.1 Write tests for treatment_group_build()
  - Test that built_target and built_pop_data are created correctly
  - Test that no temporary variables (fct_levels, grp_i, i) remain in table environment
  - Test filter error handling
  - Test treatment group expansion
  - Test factor handling
  - _Requirements: 12.1-12.5_

- [x] 3. Refactor build_header_n()
  - Extract bindings from table environment
  - Calculate header N values in function environment
  - Explicitly bind header_n back to table environment
  - Remove evalq() wrapper
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6, 10.1-10.8_

- [x] 3.1 Write tests for build_header_n()
  - Test header N calculation with population data
  - Test header N with column grouping variables
  - Test that no temporary variables remain in table environment
  - _Requirements: 12.1-12.5_

- [x] 4. Checkpoint - Verify table-level functions
  - Run full test suite
  - Verify R CMD check passes
  - Benchmark performance of table-level functions
  - Ensure all tests pass, ask the user if questions arise

- [x] 5. Refactor process_summaries.count_layer()
  - Extract bindings from layer environment (built_target, target_var, by, where, etc.)
  - Perform count calculations in function environment
  - Explicitly bind numeric_data back to layer environment
  - Remove evalq() wrapper
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6, 4.1-4.9, 6.1-6.10_

- [x] 5.1 Write tests for process_summaries.count_layer()
  - Test count calculations are correct
  - Test distinct counting
  - Test nested counting
  - Test that no temporary variables remain in layer environment
  - _Requirements: 12.1-12.5_

- [x] 6. Refactor count layer helper functions
  - Refactor process_single_count_target()
  - Refactor process_count_n()
  - Refactor process_count_total_row()
  - Refactor process_missing_subjects_row()
  - Refactor process_count_denoms()
  - Refactor factor_treat_var()
  - Refactor rename_missing_values()
  - Each function should follow Extract-Process-Bind pattern
  - Remove all evalq() wrappers
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6, 6.1-6.10, 10.1-10.8_

- [x] 6.1 Write tests for count layer helpers
  - Test each helper function independently
  - Test that no temporary variables remain in layer environment
  - Test edge cases (empty data, all NA, etc.)
  - _Requirements: 12.1-12.5_

- [x] 7. Refactor process_formatting.count_layer()
  - Extract bindings from layer environment (numeric_data, format_strings, etc.)
  - Perform formatting in function environment
  - Explicitly bind formatted_data back to layer environment
  - Remove evalq() wrapper
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6, 4.1-4.9_

- [x] 7.1 Write tests for process_formatting.count_layer()
  - Test formatting output matches expected format
  - Test that no temporary variables remain in layer environment
  - _Requirements: 12.1-12.5_

- [x] 8. Refactor process_metadata.count_layer()
  - Extract bindings from layer environment
  - Generate metadata in function environment
  - Explicitly bind metadata back to layer environment
  - Remove evalq() wrapper
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6, 4.1-4.9, 9.1-9.7_

- [x] 8.1 Write tests for process_metadata.count_layer()
  - Test metadata structure is correct
  - Test traceability information is complete
  - Test that no temporary variables remain in layer environment
  - _Requirements: 12.1-12.5_

- [x] 9. Checkpoint - Verify count layer functions
  - Run full test suite
  - Verify R CMD check passes
  - Benchmark performance of count layer functions
  - Ensure all tests pass, ask the user if questions arise

- [ ] 10. Refactor process_summaries.desc_layer()
  - Extract bindings from layer environment
  - Calculate descriptive statistics in function environment
  - Explicitly bind trans_sums back to layer environment
  - Remove evalq() wrapper
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6, 4.1-4.9, 7.1-7.8_

- [ ] 10.1 Write tests for process_summaries.desc_layer()
  - Test all built-in statistics
  - Test custom summaries
  - Test multi-variable summaries
  - Test that no temporary variables remain in layer environment
  - _Requirements: 12.1-12.5_

- [ ] 11. Refactor process_formatting.desc_layer()
  - Extract bindings from layer environment
  - Perform formatting in function environment
  - Explicitly bind form_sums back to layer environment
  - Remove evalq() wrapper
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6, 4.1-4.9, 7.1-7.8_

- [ ] 11.1 Write tests for process_formatting.desc_layer()
  - Test formatting output matches expected format
  - Test that no temporary variables remain in layer environment
  - _Requirements: 12.1-12.5_

- [ ] 12. Refactor process_metadata.desc_layer()
  - Extract bindings from layer environment
  - Generate metadata in function environment
  - Explicitly bind metadata back to layer environment
  - Remove evalq() wrapper
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6, 4.1-4.9, 9.1-9.7_

- [ ] 12.1 Write tests for process_metadata.desc_layer()
  - Test metadata structure is correct
  - Test that no temporary variables remain in layer environment
  - _Requirements: 12.1-12.5_

- [ ] 13. Checkpoint - Verify desc layer functions
  - Run full test suite
  - Verify R CMD check passes
  - Benchmark performance of desc layer functions
  - Ensure all tests pass, ask the user if questions arise

- [ ] 14. Refactor process_summaries.shift_layer()
  - Extract bindings from layer environment
  - Calculate shift counts in function environment
  - Explicitly bind numeric_data back to layer environment
  - Remove evalq() wrapper
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6, 4.1-4.9, 8.1-8.9_

- [ ] 15. Refactor shift layer helper functions
  - Refactor process_shift_n()
  - Refactor process_shift_total()
  - Refactor process_shift_denoms()
  - Each function should follow Extract-Process-Bind pattern
  - Remove all evalq() wrappers
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6, 8.1-8.9_

- [ ] 15.1 Write tests for shift layer functions
  - Test shift count calculations
  - Test row/column matrix structure
  - Test that no temporary variables remain in layer environment
  - _Requirements: 12.1-12.5_

- [ ] 16. Refactor process_formatting.shift_layer()
  - Extract bindings from layer environment
  - Perform formatting in function environment
  - Explicitly bind formatted_data back to layer environment
  - Remove evalq() wrapper
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6, 4.1-4.9, 8.1-8.9_

- [ ] 17. Refactor process_metadata.shift_layer()
  - Extract bindings from layer environment
  - Generate metadata in function environment
  - Explicitly bind metadata back to layer environment
  - Remove evalq() wrapper
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6, 4.1-4.9, 9.1-9.7_

- [ ] 18. Refactor prepare_format_metadata methods
  - Refactor prepare_format_metadata.count_layer()
  - Refactor prepare_format_metadata.shift_layer()
  - Each function should follow Extract-Process-Bind pattern
  - Remove all evalq() wrappers
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6_

- [ ] 19. Checkpoint - Verify shift layer functions
  - Run full test suite
  - Verify R CMD check passes
  - Benchmark performance of shift layer functions
  - Ensure all tests pass, ask the user if questions arise

- [ ] 20. Refactor sorting functions
  - Refactor add_order_columns.count_layer()
  - Refactor add_order_columns.desc_layer()
  - Refactor add_order_columns.shift_layer()
  - Refactor get_data_order()
  - Each function should follow Extract-Process-Bind pattern
  - Remove all evalq() wrappers
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6, 5.1-5.8_

- [ ] 20.1 Write tests for sorting functions
  - Test all sorting methods (bycount, byfactor, byvarn)
  - Test that no temporary variables remain in layer environment
  - _Requirements: 12.1-12.5_

- [ ] 21. Refactor nested count functions
  - Refactor process_nested_count_target()
  - Follow Extract-Process-Bind pattern
  - Remove evalq() wrapper
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6_

- [ ] 21.1 Write tests for nested count functions
  - Test nested count structure
  - Test indentation
  - Test that no temporary variables remain in layer environment
  - _Requirements: 12.1-12.5_

- [ ] 22. Refactor risk difference functions
  - Refactor process_statistic_data.tplyr_riskdiff()
  - Refactor process_statistic_formatting.tplyr_riskdiff()
  - Refactor process_metadata.tplyr_riskdiff()
  - Each function should follow Extract-Process-Bind pattern
  - Remove all evalq() wrappers
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6_

- [ ] 22.1 Write tests for risk difference functions
  - Test risk difference calculations
  - Test that no temporary variables remain in environment
  - _Requirements: 12.1-12.5_

- [ ] 23. Refactor remaining helper functions
  - Review all remaining evalq() usage
  - Refactor any remaining functions in gather_defaults.R (if needed)
  - Refactor any remaining functions in assertions.R (if needed)
  - Refactor any remaining functions in print.R (if needed)
  - Each function should follow Extract-Process-Bind pattern
  - Remove all evalq() wrappers
  - _Requirements: 1.1, 1.3, 1.5, 2.1-2.6_

- [ ] 24. Checkpoint - Verify all functions refactored
  - Search codebase for remaining evalq() usage
  - Verify zero uses of evalq() for multi-line code blocks
  - Run full test suite
  - Verify R CMD check passes
  - Ensure all tests pass, ask the user if questions arise

- [ ] 25. Performance validation
  - Benchmark all refactored functions
  - Compare to baseline performance
  - Verify performance is within 10% of baseline
  - Profile and optimize if needed
  - _Requirements: 15.1-15.5_

- [ ] 25.1 Document performance results
  - Create performance comparison report
  - Document any optimizations made
  - _Requirements: 15.1-15.5_

- [ ] 26. Code quality review
  - Verify all functions follow Extract-Process-Bind pattern
  - Verify all functions have clear Extract/Process/Bind sections
  - Verify no temporary variables remain in environments
  - Verify all error handling is preserved
  - Run R CMD check with no errors, warnings, or notes
  - _Requirements: 13.1-13.5, 14.1-14.5_

- [ ] 27. Documentation updates
  - Add roxygen2 comments to refactored functions explaining pattern
  - Update internal documentation with refactoring notes
  - Update NEWS.md with internal changes note
  - Create developer guide section on Extract-Process-Bind pattern
  - _Requirements: 13.1-13.5, 16.1-16.5_

- [ ] 28. Test coverage verification
  - Run test coverage analysis
  - Verify coverage is maintained or improved
  - Add tests for any gaps identified
  - _Requirements: 12.1-12.5_

- [ ] 29. Backward compatibility verification
  - Run all vignette examples
  - Verify output is identical to pre-refactoring
  - Test with real-world use cases if available
  - Verify all user-facing APIs unchanged
  - _Requirements: 11.1-11.5_

- [ ] 30. Final checkpoint - Complete validation
  - Run full test suite (all tests must pass)
  - Run R CMD check (must pass with no errors/warnings/notes)
  - Run UAT test suite (must pass)
  - Verify performance within acceptable range
  - Code review by maintainer
  - Ensure all tests pass, ask the user if questions arise

- [ ] 31. Merge and release preparation
  - Merge refactoring branch to main development branch
  - Update version number if appropriate
  - Finalize NEWS.md entry
  - Prepare release notes if needed
  - _Requirements: 16.1-16.5_

## Notes

### Testing Philosophy
- All test tasks are required for comprehensive coverage
- Each checkpoint ensures system remains in working state
- Tests should verify both functionality and absence of side effects
- Tests verify no environment pollution from refactored functions

### Incremental Approach
- Each task is independently testable
- Checkpoints allow for validation before proceeding
- Easy rollback to any previous checkpoint
- Can pause and resume at any checkpoint

### Success Criteria
- Zero evalq() uses for multi-line code blocks
- All tests pass
- Performance within 10% of baseline
- R CMD check passes
- Code review approved
- Documentation complete

### Risk Mitigation
- Frequent checkpoints reduce risk
- Incremental changes allow easy debugging
- Comprehensive testing catches regressions
- Performance monitoring prevents degradation
- Code review ensures quality
