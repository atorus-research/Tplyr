# Tplyr Refactoring Steering Rules

## Overview

These steering rules guide any refactoring work on the Tplyr package. Tplyr is a mature, production-ready R package used in regulated pharmaceutical environments. Any changes must preserve backward compatibility and maintain the package's qualification status.

## Critical Principles

### 1. Backward Compatibility is Paramount

**Rule**: Never break existing user code without explicit deprecation cycle

**Rationale**: Tplyr is used in validated pharmaceutical workflows. Breaking changes require revalidation, which is extremely costly for users.

**Implementation**:
- All existing functions must continue to work with same inputs/outputs
- If changing behavior, use deprecation warnings for at least one release cycle
- Provide migration path for deprecated features
- Document all changes in NEWS.md

### 2. Preserve Traceability Features

**Rule**: Metadata and traceability functionality must be maintained or enhanced, never reduced

**Rationale**: Traceability from summary to source data is a core value proposition for regulatory compliance.

**Implementation**:
- All refactoring must preserve `build(metadata=TRUE)` functionality
- Metadata structure can be extended but not reduced
- Test metadata generation for all layer types
- Ensure `get_metadata()` and related functions continue to work

### 3. Maintain Test Coverage

**Rule**: Refactored code must maintain or improve test coverage

**Rationale**: High test coverage is essential for confidence in a package used in regulated environments.

**Implementation**:
- Run full test suite before and after changes
- Add tests for any new code paths
- Update snapshot tests if output format changes (with justification)
- Maintain UAT test suite

### 4. Respect the Environment-Based Architecture

**Rule**: Understand and preserve the environment-based object model

**Rationale**: The environment-based design enables mutable state, parent-child relationships, and lazy evaluation - all core to Tplyr's design.

**Implementation**:
- Don't convert environments to lists or other structures without careful consideration
- Preserve parent-child relationships between tables and layers
- Maintain lazy evaluation pattern (construction ≠ execution)
- Understand quosure handling for NSE

## Command Execution Best Practices

### 1. Git Command Safety

**Rule**: Prevent git commands from entering interactive paging mode while preserving important information

**Rationale**: Git commands that produce large output can trigger interactive pagers (like `less`) which hang the terminal and require manual intervention. However, we must balance this with the need to see all relevant information.

**When to Limit Output**:
- Exploratory commands (checking history, browsing commits)
- Commands where you only need recent/summary information
- Commands that might produce hundreds of lines

**When NOT to Limit Output**:
- Viewing specific diffs needed for debugging
- Checking for specific errors or patterns
- When the full output is needed for decision-making
- When output is expected to be small (e.g., `git status`)

**Implementation**:
```bash
# Exploratory/browsing - LIMIT OUTPUT
git log --all --oneline | head -20        # Recent commits
git log --all --oneline -20               # Using git's built-in limit
git branch -a | head -30                  # List branches

# Specific information needed - USE GREP/FILTER
git log --all --oneline | grep "refactor" # Find specific commits
git diff HEAD~1 | grep -A 5 "function"    # Find specific changes

# Full output needed for analysis - DON'T LIMIT
git status                                # Usually short
git diff path/to/specific/file.R          # Specific file diff
git show commit_hash:path/to/file         # Specific file content

# Test output - SHOW SUMMARY
Rscript -e "devtools::test()" 2>&1 | tail -100  # Show end with summary
Rscript -e "devtools::test()" 2>&1 | grep -E "(FAIL|WARN|PASS|Duration)" # Just stats

# Test output - SHOW FAILURES
Rscript -e "devtools::test()" 2>&1 | grep -A 20 "FAIL"  # Show failure details

# Use --no-pager for git when piping
git --no-pager log --oneline -20
git --no-pager diff HEAD~1 | head -100
```

**Best Practice**: Start with limited output for exploration, then run specific commands without limits when you need full details.

### 2. Test Output Management

**Rule**: Always limit test output to prevent terminal overflow

**Implementation**:
- Use `tail -N` to show only the last N lines of test output
- Use `grep` to filter for specific patterns
- Examples:
  ```bash
  # Show only summary
  Rscript -e "devtools::test()" 2>&1 | tail -50
  
  # Show only failures
  Rscript -e "devtools::test()" 2>&1 | grep -A 10 "FAIL"
  ```

## Code Quality Standards

### 1. R Package Best Practices

**Rule**: Follow standard R package development practices

**Implementation**:
- Use roxygen2 for documentation
- Follow tidyverse style guide
- Pass `R CMD check` with no errors, warnings, or notes
- Maintain CRAN compliance

### 2. Documentation Requirements

**Rule**: All changes must be documented

**Implementation**:
- Update function documentation for any API changes
- Update relevant vignettes
- Add examples for new features
- Update NEWS.md with user-facing changes

### 3. Code Organization

**Rule**: Maintain clear separation of concerns

**Implementation**:
- Keep related functionality in same file
- Use consistent naming conventions
- Separate user-facing API from internal implementation
- Use `@noRd` for internal functions

## Testing Requirements

### 1. Test Before Refactoring

**Rule**: Establish baseline test results before making changes

**Implementation**:
```r
# Run full test suite
devtools::test()

# Run R CMD check
devtools::check()

# Document baseline results
```

### 2. Test During Refactoring

**Rule**: Run tests frequently during refactoring

**Implementation**:
- Run relevant tests after each logical change
- Don't accumulate untested changes
- Fix test failures immediately

### 3. Test After Refactoring

**Rule**: Comprehensive testing before considering refactoring complete

**Implementation**:
- Full test suite must pass
- R CMD check must pass
- Manual testing of examples in vignettes
- Performance testing if relevant

## Refactoring Strategies

### 1. Incremental Changes

**Rule**: Make small, testable changes rather than large rewrites

**Rationale**: Small changes are easier to test, review, and debug. They also reduce risk.

**Implementation**:
- Refactor one module at a time
- Commit working code frequently
- Each commit should leave code in working state

### 2. Extract Before Modify

**Rule**: When refactoring complex functions, extract helper functions first

**Implementation**:
1. Extract logical chunks into helper functions
2. Test that extraction didn't change behavior
3. Refactor helper functions individually
4. Test again

### 3. Parallel Implementation

**Rule**: For major changes, implement new version alongside old

**Implementation**:
- Create new functions with different names
- Migrate functionality gradually
- Deprecate old functions once new ones are stable
- Remove deprecated functions after deprecation cycle

## Specific Tplyr Considerations

### 1. Layer Processing

**Rule**: Understand the three-phase layer processing model

**Phases**:
1. `process_summaries()`: Calculate numeric results
2. `process_formatting()`: Apply string formatting and pivot
3. `process_metadata()`: Generate traceability information

**Implementation**:
- Changes to one phase should not break others
- Test all three phases for each layer type
- Maintain S3 dispatch pattern

### 2. Format Strings

**Rule**: Preserve `f_str()` functionality and syntax

**Rationale**: Format strings are a core DSL that users rely on

**Implementation**:
- Don't change format string parsing without strong justification
- Maintain support for: x-based width, auto-precision (a), parenthesis hugging (X/A)
- Test all format string features

### 3. Quosures and NSE

**Rule**: Maintain tidy evaluation patterns

**Implementation**:
- Use `enquo()`, `enquos()` for capturing user inputs
- Use `!!` for unquoting
- Use `as_name()` for converting quosures to strings
- Test with both quoted and unquoted inputs

### 4. Factor Handling

**Rule**: Preserve factor-based ordering and dummy value generation

**Implementation**:
- Test with factor and non-factor inputs
- Verify factor levels are respected in output
- Ensure dummy rows are generated for all factor levels

### 5. Population Data

**Rule**: Maintain separation between target and population datasets

**Implementation**:
- Test with and without separate population data
- Verify denominators calculated correctly
- Ensure header N values use population data when specified

## Performance Considerations

### 1. Don't Optimize Prematurely

**Rule**: Maintain correctness over performance unless performance is a documented issue

**Implementation**:
- Profile before optimizing
- Document performance improvements with benchmarks
- Don't sacrifice readability for minor performance gains

### 2. Benchmark Major Changes

**Rule**: If refactoring could affect performance, measure it

**Implementation**:
```r
# Use microbenchmark or bench
library(bench)
mark(
  old_version = old_function(...),
  new_version = new_function(...),
  iterations = 100
)
```

## Deprecation Process

### 1. Deprecation Warnings

**Rule**: Use lifecycle package for deprecation warnings

**Implementation**:
```r
#' @export
old_function <- function(...) {
  lifecycle::deprecate_warn(
    when = "1.3.0",
    what = "old_function()",
    with = "new_function()"
  )
  # ... existing implementation
}
```

### 2. Deprecation Timeline

**Rule**: Minimum one release cycle for deprecation

**Timeline**:
1. Release N: Add deprecation warning, document alternative
2. Release N+1: Mark as deprecated in documentation
3. Release N+2: Consider removal (or keep indefinitely if low cost)

## Code Review Checklist

Before considering refactoring complete, verify:

- [ ] All existing tests pass
- [ ] R CMD check passes with no errors/warnings/notes
- [ ] New tests added for any new code paths
- [ ] Documentation updated (functions, vignettes, NEWS.md)
- [ ] Backward compatibility maintained or deprecation warnings added
- [ ] Metadata generation still works
- [ ] Examples in vignettes still run
- [ ] Performance is acceptable (benchmark if relevant)
- [ ] Code follows tidyverse style guide
- [ ] Commit messages are clear and descriptive

## Common Pitfalls to Avoid

### 1. Breaking Quosure Handling

**Pitfall**: Treating quosures as regular values

**Solution**: Use `as_name()`, `eval_tidy()`, etc. appropriately

### 2. Changing Output Structure

**Pitfall**: Modifying column names or structure of output data frame

**Solution**: Output structure is part of the API - changes require deprecation

### 3. Ignoring Edge Cases

**Pitfall**: Testing only happy path

**Solution**: Test with empty data, single group, all NA, etc.

### 4. Forgetting About Sublayers

**Pitfall**: Testing only top-level layers

**Solution**: Test nested count layers and other sublayer scenarios

### 5. Breaking S3 Dispatch

**Pitfall**: Changing method signatures without updating all implementations

**Solution**: Verify all S3 methods for a generic have consistent signatures

## When to Seek Help

Consult with Tplyr maintainers or experienced R developers when:

- Considering changes to core architecture (environment model, S3 dispatch)
- Unsure about backward compatibility implications
- Encountering complex quosure/NSE issues
- Planning to change output structure
- Considering performance optimizations that affect readability
- Dealing with CRAN submission issues

## Resources

### Internal Documentation
- Codebase mapping: `.kiro/specs/tplyr-refactor/codebase-mapping.md`
- Functional requirements: `.kiro/specs/tplyr-refactor/functional-requirements.md`

### External Resources
- [Advanced R - Environments](https://adv-r.hadley.nz/environments.html)
- [Advanced R - Metaprogramming](https://adv-r.hadley.nz/metaprogramming.html)
- [R Packages Book](https://r-pkgs.org/)
- [Tidyverse Style Guide](https://style.tidyverse.org/)
- [Tplyr Vignettes](vignettes/)

## Conclusion

Refactoring Tplyr requires careful attention to backward compatibility, testing, and documentation. The package's use in regulated environments means that reliability and traceability are more important than code elegance. When in doubt, prefer conservative changes that maintain existing behavior over aggressive refactoring that could introduce risk.

Remember: **Working code that users depend on is more valuable than perfect code that breaks their workflows.**
