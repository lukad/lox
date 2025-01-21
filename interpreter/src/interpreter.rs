use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    env::Env,
    value::{FunValue, Value},
};
use lox_ast::*;
use thiserror::Error;

pub type LoxResult<'a, T> = Result<T, LoxError<'a>>;

#[derive(Debug, Error)]
#[allow(unused)]
pub enum LoxError<'a> {
    #[error("runtime error: {0}")]
    RuntimeError(String),
    #[error("type mismatch: cannot perform {op} on {lhs:?} and {rhs:?}")]
    TypeMismatch {
        op: String,
        lhs: Value<'a>,
        rhs: Value<'a>,
    },
    #[error("division by zero")]
    DivisionByZero,
    #[error("return {0:?}")]
    Return(Value<'a>),
}

pub struct Interpreter<'a> {
    env: Rc<RefCell<Env<'a>>>,
    globals: Rc<RefCell<Env<'a>>>,
    locals: HashMap<*const str, usize>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Env::new()));
        Self {
            env: Rc::clone(&globals),
            globals,
            locals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, program: &Program<'a>) -> LoxResult<'a, ()> {
        for stmt in program.iter() {
            self.visit_stmt(stmt)?;
        }
        Ok(())
    }

    pub(crate) fn resolve(&mut self, name: &str, depth: usize) {
        self.locals.insert(name, depth);
    }

    fn lookup_variable(&self, name: &str) -> LoxResult<'a, Value<'a>> {
        let maybe_value = if let Some(distance) = self.locals.get(&(name as *const str)) {
            self.env.borrow().get_at(*distance, name)
        } else {
            self.globals.borrow().get(name)
        };
        maybe_value.ok_or(LoxError::RuntimeError(format!(
            "undefined variable: {}",
            name
        )))
    }

    fn visit_stmt(&mut self, stmt: &Spanned<Stmt<'a>>) -> LoxResult<'a, ()> {
        match &stmt.1 {
            Stmt::Class(_class) => todo!("visit class"),
            Stmt::Var(var) => self.visit_var(var),
            Stmt::Fun(fun) => self.visit_fun(fun),
            Stmt::Expr(expr) => self.visit_expr(expr).map(|_| ()),
            Stmt::Print(expr) => self.visit_print(expr),
            Stmt::Block(block) => self.visit_block(block),
            Stmt::If(if_) => self.visit_if(if_),
            Stmt::While(while_) => self.visit_while(while_),
            Stmt::Return(expr) => self.visit_return(expr),
        }
    }

    fn visit_var(&mut self, var: &Spanned<VarStmt<'a>>) -> LoxResult<'a, ()> {
        let name = var.1.lhs.1;
        let value = var
            .1
            .rhs
            .as_ref()
            .map(|rhs| self.visit_expr(rhs))
            .unwrap_or(Ok(Value::Nil))?;

        self.env.borrow_mut().define(name, value)
    }

    fn visit_fun(&mut self, fun: &Spanned<FunStmt<'a>>) -> LoxResult<'a, ()> {
        let fun_value = Value::Fun(FunValue {
            name: fun.1.name.clone(),
            params: fun.1.params.clone(),
            body: fun.1.body.1.clone(),
            env: Rc::clone(&self.env),
        });

        self.env.borrow_mut().define(fun.1.name.1, fun_value)?;
        Ok(())
    }

    fn visit_expr(&mut self, expr: &Spanned<Expr<'a>>) -> LoxResult<'a, Value<'a>> {
        match &expr.1 {
            Expr::Primary(primary) => self.visit_primary(primary),
            Expr::Binary(binary) => self.visit_binary(binary),
            Expr::Grouping(grouping) => self.visit_expr(grouping),
            Expr::Unary(unary) => self.visit_unary(unary),
            Expr::Assign(assign) => self.visit_assign(assign),
            Expr::Call(call) => self.visit_call(call),
        }
    }

    fn visit_print(&mut self, expr: &Spanned<Expr<'a>>) -> LoxResult<'a, ()> {
        let value = self.visit_expr(expr)?;
        match value {
            Value::Fun(fun) => {
                println!("<fun {}>", fun.name.1);
            }
            Value::Nil => println!("nil"),
            Value::Bool(b) => println!("{}", b),
            Value::Number(n) => println!("{}", n),
            Value::String(s) => println!("{}", s),
        }
        Ok(())
    }

    fn visit_block(&mut self, block: &Vec<Spanned<Stmt<'a>>>) -> LoxResult<'a, ()> {
        let env = Rc::new(RefCell::new(Env::from(&self.env)));
        self.visit_block_with_env(block, env)?;
        Ok(())
    }

    fn visit_block_with_env(
        &mut self,
        block: &Vec<Spanned<Stmt<'a>>>,
        env: Rc<RefCell<Env<'a>>>,
    ) -> LoxResult<'a, ()> {
        let previous = self.env.clone();
        let execute = || -> LoxResult<'a, ()> {
            self.env = env;
            for stmt in block.iter() {
                self.visit_stmt(stmt)?;
            }
            Ok(())
        };
        let result = execute();
        self.env = previous;
        result
    }

    fn visit_if(&mut self, if_: &Spanned<If<'a>>) -> LoxResult<'a, ()> {
        let cond = self.visit_expr(&if_.1.cond)?;
        if cond.is_truthy() {
            self.visit_stmt(&if_.1.body)?;
        } else if let Some(else_body) = &if_.1.else_body {
            self.visit_stmt(else_body)?;
        }
        Ok(())
    }

    fn visit_while(&mut self, while_: &Spanned<While<'a>>) -> LoxResult<'a, ()> {
        while self.visit_expr(&while_.1.cond)?.is_truthy() {
            self.visit_stmt(&while_.1.body)?;
        }
        Ok(())
    }

    fn visit_return(&mut self, expr: &Option<Spanned<Expr<'a>>>) -> LoxResult<'a, ()> {
        match expr {
            Some(expr) => Err(LoxError::Return(self.visit_expr(expr)?)),
            None => Err(LoxError::Return(Value::Nil)),
        }
    }

    fn visit_primary(&mut self, primary: &Spanned<Primary<'a>>) -> LoxResult<'a, Value<'a>> {
        match primary.1 {
            Primary::Nil => Ok(Value::Nil),
            Primary::Bool(b) => Ok(Value::Bool(b)),
            Primary::Number(n) => Ok(Value::Number(n)),
            Primary::String(s) => Ok(Value::String(s.to_string())),
            Primary::Identifier(s) => self.lookup_variable(s),
            Primary::Super(_) => todo!(),
        }
    }

    fn visit_binary(&mut self, binary: &Spanned<Binary<'a>>) -> LoxResult<'a, Value<'a>> {
        let lhs = self.visit_expr(&binary.1.lhs)?;
        match binary.1.op.1 {
            BinaryOp::Add => lhs + self.visit_expr(&binary.1.rhs)?,
            BinaryOp::Sub => lhs - self.visit_expr(&binary.1.rhs)?,
            BinaryOp::Mul => lhs * self.visit_expr(&binary.1.rhs)?,
            BinaryOp::Div => lhs / self.visit_expr(&binary.1.rhs)?,
            BinaryOp::And => {
                if lhs.is_truthy() {
                    Ok(self.visit_expr(&binary.1.rhs)?)
                } else {
                    Ok(lhs)
                }
            }
            BinaryOp::Or => {
                if lhs.is_truthy() {
                    Ok(lhs)
                } else {
                    Ok(self.visit_expr(&binary.1.rhs)?)
                }
            }
            BinaryOp::Eq => lhs.eq(&self.visit_expr(&binary.1.rhs)?).map(Value::Bool),
            BinaryOp::Ne => lhs.ne(&self.visit_expr(&binary.1.rhs)?).map(Value::Bool),
            BinaryOp::Lt => lhs.lt(&self.visit_expr(&binary.1.rhs)?).map(Value::Bool),
            BinaryOp::Gt => lhs.gt(&self.visit_expr(&binary.1.rhs)?).map(Value::Bool),
            BinaryOp::Le => lhs.le(&self.visit_expr(&binary.1.rhs)?).map(Value::Bool),
            BinaryOp::Ge => lhs.ge(&self.visit_expr(&binary.1.rhs)?).map(Value::Bool),
        }
    }

    fn visit_unary(&mut self, unary: &Spanned<Unary<'a>>) -> LoxResult<'a, Value<'a>> {
        let rhs = self.visit_expr(&unary.1.rhs)?;
        match unary.1.op.1 {
            UnaryOp::Neg => -(rhs),
            UnaryOp::Not => Ok(!rhs),
        }
    }

    fn visit_assign(&mut self, assign: &Spanned<Assign<'a>>) -> LoxResult<'a, Value<'a>> {
        let value = self.visit_expr(&assign.1.rhs)?;

        match self.locals.get(&(assign.1.lhs.1 as *const str)) {
            Some(distance) => {
                self.env
                    .borrow_mut()
                    .assign_at(*distance, assign.1.lhs.1, value.clone())?;
            }
            None => {
                self.globals
                    .borrow_mut()
                    .assign(assign.1.lhs.1, value.clone())?;
            }
        }

        Ok(value)
    }

    fn visit_call(&mut self, call: &Spanned<Call<'a>>) -> LoxResult<'a, Value<'a>> {
        let callee = self.visit_expr(&call.1.callee)?;
        let fun: FunValue<'a> = callee.try_into()?;

        if fun.params.len() != call.1.args.len() {
            return Err(LoxError::RuntimeError(format!(
                "expected {} arguments, got {}",
                fun.params.len(),
                call.1.args.len()
            )));
        }

        let args = call
            .1
            .args
            .iter()
            .map(|arg| self.visit_expr(arg))
            .collect::<Result<Vec<_>, _>>()?;

        let previous = self.env.clone();
        let env = Rc::new(RefCell::new(Env::from(&fun.env)));

        for ((_, param), arg) in fun.params.iter().zip(args.iter()) {
            env.borrow_mut().define(param, arg.clone())?;
        }

        let result = match self.visit_block_with_env(&fun.body, env) {
            Ok(_) => Ok(Value::Nil),
            Err(LoxError::Return(value)) => Ok(value),
            Err(e) => Err(e),
        };
        self.env = previous;
        result
    }
}
