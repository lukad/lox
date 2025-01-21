use std::{cell::RefCell, rc::Rc};

use lox_ast::{Spanned, Stmt};

use crate::{
    env::Env,
    interpreter::{LoxError, LoxResult},
};

/// A value in the Lox language.
#[derive(Debug, PartialEq, Clone)]
pub enum Value<'a> {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Fun(FunValue<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunValue<'a> {
    pub name: Spanned<&'a str>,
    pub params: Vec<Spanned<&'a str>>,
    pub body: Vec<Spanned<Stmt<'a>>>,
    pub env: Rc<RefCell<Env<'a>>>,
}

impl<'a> Value<'a> {
    /// A value is truthy if it is not `nil` or `false`.
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Bool(false))
    }

    /// Check if two values are equal.
    pub fn eq(&self, rhs: &Self) -> LoxResult<'a, bool> {
        match (self, rhs) {
            (Value::Nil, Value::Nil) => Ok(true),
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(lhs == rhs),
            (Value::Number(lhs), Value::Number(rhs)) => Ok(lhs == rhs),
            (Value::String(lhs), Value::String(rhs)) => Ok(lhs == rhs),
            _ => Err(LoxError::TypeMismatch {
                op: "==".to_string(),
                lhs: self.clone(),
                rhs: rhs.clone(),
            }),
        }
    }

    /// Check if two values are not equal.
    pub fn ne(&self, rhs: &Self) -> LoxResult<'a, bool> {
        Ok(!self.eq(rhs)?)
    }

    /// Check if a value is less than another value.
    pub fn lt(&self, rhs: &Self) -> LoxResult<'a, bool> {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(lhs < rhs),
            _ => Err(LoxError::TypeMismatch {
                op: "<".to_string(),
                lhs: self.clone(),
                rhs: rhs.clone(),
            }),
        }
    }

    /// Check if a value is less than or equal to another value.
    pub fn le(&self, rhs: &Self) -> LoxResult<'a, bool> {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(lhs <= rhs),
            _ => Err(LoxError::TypeMismatch {
                op: "<=".to_string(),
                lhs: self.clone(),
                rhs: rhs.clone(),
            }),
        }
    }

    /// Check if a value is greater than another value.
    pub fn gt(&self, rhs: &Self) -> LoxResult<'a, bool> {
        Ok(!self.le(rhs)?)
    }

    /// Check if a value is greater than or equal to another value.
    pub fn ge(&self, rhs: &Self) -> LoxResult<'a, bool> {
        Ok(!self.lt(rhs)?)
    }
}

impl From<bool> for Value<'_> {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

impl From<f64> for Value<'_> {
    fn from(n: f64) -> Self {
        Value::Number(n)
    }
}

impl From<String> for Value<'_> {
    fn from(s: String) -> Self {
        Value::String(s)
    }
}

impl From<&str> for Value<'_> {
    fn from(s: &str) -> Self {
        Value::String(s.to_string())
    }
}

impl<'a> TryFrom<Value<'a>> for FunValue<'a> {
    type Error = LoxError<'a>;

    fn try_from(value: Value<'a>) -> Result<Self, Self::Error> {
        match value {
            Value::Fun(fun) => Ok(fun),
            _ => Err(LoxError::RuntimeError(format!(
                "expected function, got {:?}",
                value
            ))),
        }
    }
}

impl<'a> std::ops::Add for Value<'a> {
    type Output = LoxResult<'a, Value<'a>>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs + rhs)),
            (lhs, rhs) => Err(LoxError::TypeMismatch {
                op: "+".to_string(),
                lhs,
                rhs,
            }),
        }
    }
}

impl<'a> std::ops::Sub for Value<'a> {
    type Output = LoxResult<'a, Value<'a>>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs - rhs)),
            (lhs, rhs) => Err(LoxError::TypeMismatch {
                op: "-".to_string(),
                lhs,
                rhs,
            }),
        }
    }
}

impl<'a> std::ops::Mul for Value<'a> {
    type Output = LoxResult<'a, Value<'a>>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs * rhs)),
            (lhs, rhs) => Err(LoxError::TypeMismatch {
                op: "*".to_string(),
                lhs,
                rhs,
            }),
        }
    }
}

impl<'a> std::ops::Div for Value<'a> {
    type Output = LoxResult<'a, Value<'a>>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(_), Value::Number(0.0)) => Err(LoxError::DivisionByZero),
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs / rhs)),
            (lhs, rhs) => Err(LoxError::TypeMismatch {
                op: "/".to_string(),
                lhs,
                rhs,
            }),
        }
    }
}

impl<'a> std::ops::Neg for Value<'a> {
    type Output = LoxResult<'a, Value<'a>>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(n) => Ok(Value::Number(-n)),
            _ => Err(LoxError::TypeMismatch {
                op: "-".to_string(),
                lhs: self,
                rhs: Value::Nil,
            }),
        }
    }
}

impl<'a> std::ops::Not for Value<'a> {
    type Output = Value<'a>;

    fn not(self) -> Self::Output {
        Value::Bool(!self.is_truthy())
    }
}
