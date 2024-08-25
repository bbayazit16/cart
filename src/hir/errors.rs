use crate::context::FilePointer;
use crate::errors::TypeError;
use crate::hir::{BinaryOp, Type, TypeChecker};

impl TypeChecker {
    /// Report a type error, where the expected type does not match the found type.
    pub(super) fn report_type_error(
        &mut self,
        expected: &Type,
        found: &Type,
        file_pointer: FilePointer,
    ) {
        self.errors.push(
            TypeError::IncorrectType {
                file_pointer,
                incorrect: found.to_string(),
                expected: expected.to_string(),
            }
            .into(),
        );
    }

    /// Report a type error, where a variable is undefined.
    pub(super) fn report_undefined_variable(
        &mut self,
        variable: String,
        file_pointer: FilePointer,
    ) {
        self.errors.push(
            TypeError::UndefinedVariable {
                file_pointer,
                variable,
            }
            .into(),
        );
    }
    
    /// Report a type error, where the function is not defined.
    pub(super) fn report_undefined_function(
        &mut self,
        function: String,
        file_pointer: FilePointer,
    ) {
        self.errors.push(
            TypeError::UndefinedFunction {
                file_pointer,
                function,
            }
            .into(),
        );
    }

    /// Report a type error, where a non-array type is indexed.
    pub(super) fn report_indexing_non_array(
        &mut self,
        found_type: String,
        file_pointer: FilePointer,
    ) {
        self.errors.push(
            TypeError::IndexingNonArray {
                file_pointer,
                found_type,
            }
            .into(),
        );
    }

    /// Report a binary operation error, where the types do not match.
    pub(super) fn report_binary_op_error(
        &mut self,
        op: &BinaryOp,
        left: &Type,
        right: &Type,
        file_pointer: FilePointer,
    ) {
        self.errors.push(
            TypeError::BinaryOpError {
                op: op.to_string(),
                left: left.to_string(),
                right: right.to_string(),
                file_pointer,
            }
            .into(),
        );
    }
    
    /// Report a type error, where the unit type is tried to be assigned in a let statement.
    pub(super) fn report_unit_assignment(
        &mut self,
        file_pointer: FilePointer,
    ) {
        self.errors.push(
            TypeError::UnitAssignment {
                file_pointer,
            }
            .into(),
        );
    }
}
