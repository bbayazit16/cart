use crate::context::Span;
use crate::errors::TypeError;
use crate::hir::{BinaryOp, Type, TypeChecker};

impl TypeChecker {
    /// Report a type error, where the expected type does not match the found type.
    pub(super) fn report_type_error(&mut self, expected: &Type, found: &Type, span: Span) {
        self.errors.push(
            TypeError::IncorrectType {
                span,
                incorrect: found.to_string(),
                expected: expected.to_string(),
            }
            .into(),
        );
    }

    /// Report a type error, where a variable is undefined.
    pub(super) fn report_undefined_variable(&mut self, variable: String, span: Span) {
        self.errors
            .push(TypeError::UndefinedVariable { span, variable }.into());
    }

    /// Report a type error, where the function is not defined.
    pub(super) fn report_undefined_function(&mut self, function: String, span: Span) {
        self.errors
            .push(TypeError::UndefinedFunction { span, function }.into());
    }

    /// Report a type error, where a non-array type is indexed.
    pub(super) fn report_indexing_non_array(&mut self, found_type: String, span: Span) {
        self.errors
            .push(TypeError::IndexingNonArray { span, found_type }.into());
    }

    /// Report a binary operation error, where the types do not match.
    pub(super) fn report_binary_op_error(
        &mut self,
        op: &BinaryOp,
        left: &Type,
        right: &Type,
        span: Span,
    ) {
        self.errors.push(
            TypeError::BinaryOpError {
                op: op.to_string(),
                left: left.to_string(),
                right: right.to_string(),
                span,
            }
            .into(),
        );
    }

    /// Report a type error, where the unit type is tried to be assigned in a let statement.
    pub(super) fn report_unit_assignment(&mut self, span: Span) {
        self.errors.push(TypeError::UnitAssignment { span }.into());
    }

    /// Report an incorrectly constructed struct literal.
    pub(super) fn report_incorrect_struct_literal(&mut self, span: Span) {
        // TODO: Improve this error
        self.errors
            .push(TypeError::IncorrectStructLiteral { span }.into());
    }

    /// Report an unknown struct field.
    pub(super) fn report_unknown_struct_field(
        &mut self,
        field: String,
        struct_name: String,
        span: Span,
    ) {
        self.errors.push(
            TypeError::UnknownStructField {
                field,
                struct_name,
                span,
            }
            .into(),
        );
    }
    
    /// Report assignment to an immutable variable.
    pub(super) fn report_immutable_assignment(&mut self, variable: String, span: Span) {
        self.errors.push(TypeError::ImmutableAssignment { variable, span }.into());
    }
}
