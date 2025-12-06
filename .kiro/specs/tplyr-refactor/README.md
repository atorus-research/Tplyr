# Tplyr Refactoring Documentation

## Overview

This directory contains comprehensive documentation for understanding and refactoring the Tplyr R package. These documents were created to support safe, effective refactoring while maintaining backward compatibility and the package's qualification status for use in regulated pharmaceutical environments.

## Document Index

### 1. [Codebase Mapping](./codebase-mapping.md)
**Purpose**: Comprehensive map of the Tplyr codebase

**Contents**:
- Package overview and statistics
- Core architecture (object model, data flow)
- Module-by-module breakdown (45 R source files)
- Key design patterns
- Data structures
- Testing strategy overview
- Dependencies
- Extension points
- File organization

**Use When**: 
- Getting oriented in the codebase
- Understanding how modules relate to each other
- Planning refactoring scope
- Identifying critical functions

### 2. [Functional Requirements](./functional-requirements.md)
**Purpose**: Catalog of all functional requirements that must be preserved

**Contents**:
- 16 major functional requirement categories
- Detailed API specifications
- Expected behaviors
- Non-functional requirements (performance, documentation, testing)
- Critical behaviors to preserve
- Edge cases and special behaviors

**Use When**:
- Validating that refactoring preserves functionality
- Understanding what each feature should do
- Writing tests for refactored code
- Documenting changes

### 3. [Design Patterns](./design-patterns.md)
**Purpose**: Explanation of key design patterns used in Tplyr

**Contents**:
- 8 core design patterns:
  - Environment-based OOP
  - Lazy evaluation
  - S3 method dispatch
  - Quosure-based NSE
  - Builder pattern with fluent interface
  - Format string DSL
  - Strategy pattern for denominators
  - Template method for layer processing
- Architectural patterns
- Anti-patterns to avoid
- Testing patterns
- Performance patterns

**Use When**:
- Understanding why code is structured a certain way
- Deciding whether to preserve or change a pattern
- Implementing new features consistently
- Reviewing code changes

### 4. [Testing Strategy](./testing-strategy.md)
**Purpose**: Guide for testing during and after refactoring

**Contents**:
- Testing philosophy and principles
- Test suite structure
- 5 types of tests (unit, integration, snapshot, property-based, regression)
- Test data strategy
- Critical test scenarios
- Testing workflow (before, during, after refactoring)
- Test maintenance guidelines
- Common pitfalls
- CI/CD integration

**Use When**:
- Planning testing approach for refactoring
- Writing new tests
- Debugging test failures
- Updating tests after changes
- Ensuring adequate coverage

### 5. [Steering Rules](../.kiro/steering/tplyr-refactoring-rules.md)
**Purpose**: Agent steering rules for refactoring work

**Contents**:
- Critical principles (backward compatibility, traceability, test coverage)
- Code quality standards
- Testing requirements
- Refactoring strategies
- Specific Tplyr considerations
- Performance considerations
- Deprecation process
- Code review checklist
- Common pitfalls
- When to seek help

**Use When**:
- Starting any refactoring work
- Making decisions about changes
- Reviewing code changes
- Ensuring compliance with standards

## Quick Start Guide

### For Understanding the Codebase

1. Start with [Codebase Mapping](./codebase-mapping.md) - Executive Summary
2. Read the "Core Architecture" section
3. Review the "Module Breakdown" for areas you're working on
4. Check [Design Patterns](./design-patterns.md) for patterns in that code

### For Planning Refactoring

1. Review [Steering Rules](../.kiro/steering/tplyr-refactoring-rules.md) - Critical Principles
2. Identify affected modules in [Codebase Mapping](./codebase-mapping.md)
3. Check [Functional Requirements](./functional-requirements.md) for features to preserve
4. Plan testing approach using [Testing Strategy](./testing-strategy.md)

### For Implementing Refactoring

1. Follow [Steering Rules](../.kiro/steering/tplyr-refactoring-rules.md) - Refactoring Strategies
2. Reference [Design Patterns](./design-patterns.md) for implementation guidance
3. Use [Testing Strategy](./testing-strategy.md) - Testing Workflow
4. Validate against [Functional Requirements](./functional-requirements.md)

### For Reviewing Changes

1. Use [Steering Rules](../.kiro/steering/tplyr-refactoring-rules.md) - Code Review Checklist
2. Verify [Functional Requirements](./functional-requirements.md) are met
3. Check [Design Patterns](./design-patterns.md) are preserved
4. Confirm [Testing Strategy](./testing-strategy.md) was followed

## Key Takeaways

### About Tplyr

- **Mature Package**: v1.2.1, CRAN-published, production-ready
- **Regulated Use**: Used in pharmaceutical environments, has UAT documentation
- **Core Value**: Traceability from summary results to source data
- **Architecture**: Environment-based OOP with lazy evaluation
- **Dependencies**: Built on tidyverse (dplyr, tidyr, purrr, stringr, rlang)

### About Refactoring Tplyr

- **Backward Compatibility is Critical**: Users depend on existing API
- **Preserve Traceability**: Metadata generation is a core feature
- **Test Thoroughly**: High test coverage is essential
- **Understand Patterns**: Environment-based OOP and quosures are fundamental
- **Incremental Changes**: Small, testable changes are safer than large rewrites

### Critical Functions to Preserve

**User-Facing API**:
- `tplyr_table()`, `group_count()`, `group_desc()`, `group_shift()`
- `add_layer()`, `add_layers()`, `build()`
- `f_str()`, `set_format_strings()`
- All `set_*()` and `add_*()` modifier functions

**Internal Processing**:
- `process_summaries()`, `process_formatting()`, `process_metadata()`
- `num_fmt()` - numeric formatting engine
- Denominator calculation logic
- Precision calculation logic

### Common Pitfalls

1. Breaking lazy evaluation by processing data in constructors
2. Changing output structure (column names, ordering)
3. Mishandling quosures (evaluating too early or incorrectly)
4. Breaking S3 dispatch by changing method signatures
5. Ignoring edge cases (empty data, all NA, single group)

## Refactoring Workflow

### Phase 1: Preparation
1. Read all documentation in this directory
2. Run full test suite and document baseline
3. Identify scope of refactoring
4. Plan approach (incremental changes)

### Phase 2: Implementation
1. Make small, testable changes
2. Run tests frequently
3. Fix failures immediately
4. Commit working code frequently
5. Document changes in code comments

### Phase 3: Validation
1. Run full test suite
2. Run R CMD check
3. Check test coverage
4. Manual testing with vignette examples
5. Performance testing if relevant

### Phase 4: Documentation
1. Update function documentation
2. Update relevant vignettes
3. Update NEWS.md
4. Add examples for new features
5. Document any breaking changes

### Phase 5: Review
1. Self-review using Code Review Checklist
2. Verify all requirements are met
3. Confirm patterns are preserved
4. Check documentation is complete

## Additional Resources

### Internal Documentation
- Vignettes in `/vignettes/`: User-facing documentation
- Function documentation: See `?function_name` in R
- UAT documentation: `/uat/references/output/uat.pdf`

### External Resources
- [Advanced R - Environments](https://adv-r.hadley.nz/environments.html)
- [Advanced R - Metaprogramming](https://adv-r.hadley.nz/metaprogramming.html)
- [R Packages Book](https://r-pkgs.org/)
- [Tidyverse Style Guide](https://style.tidyverse.org/)
- [rlang Documentation](https://rlang.r-lib.org/)

### Tplyr Resources
- [GitHub Repository](https://github.com/atorus-research/Tplyr)
- [Package Website](https://atorus-research.github.io/Tplyr/)
- [CRAN Page](https://cran.r-project.org/package=Tplyr)
- [Cheat Sheet](https://atorus-research.github.io/Tplyr_cheatsheet.pdf)

## Document Maintenance

These documents should be updated when:
- Major refactoring is completed
- New features are added
- Architecture changes
- New patterns are introduced
- Requirements change

## Questions?

If you have questions about:
- **Codebase structure**: See [Codebase Mapping](./codebase-mapping.md)
- **What to preserve**: See [Functional Requirements](./functional-requirements.md)
- **Why code is structured a certain way**: See [Design Patterns](./design-patterns.md)
- **How to test**: See [Testing Strategy](./testing-strategy.md)
- **What rules to follow**: See [Steering Rules](../.kiro/steering/tplyr-refactoring-rules.md)

## Version History

- **v1.0** (2025-12-06): Initial documentation created
  - Comprehensive codebase mapping
  - Functional requirements catalog
  - Design patterns guide
  - Testing strategy
  - Steering rules for agents

## License

These documents are part of the Tplyr package and are subject to the same MIT license as the package itself.

---

**Remember**: The goal of refactoring is to improve code quality while maintaining functionality. When in doubt, prefer conservative changes that preserve existing behavior over aggressive refactoring that could introduce risk.

**Working code that users depend on is more valuable than perfect code that breaks their workflows.**
