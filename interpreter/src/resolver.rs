use std::collections::HashMap;
use std::marker::PhantomData;

use lox_ast::*;

use crate::interpreter::{Interpreter, LoxError, LoxResult};

#[derive(Debug)]
enum FunType {
    Fun,
}

#[derive(Debug)]
struct Resolution<'a> {
    name: &'a str,
    distance: usize,
}

pub struct Resolver<'a> {
    scopes: Vec<HashMap<&'a str, bool>>,
    resolutions: Vec<Resolution<'a>>,
    fun_type: Option<FunType>,
    _phantom: PhantomData<&'a ()>,
}

impl<'a> Resolver<'a> {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            resolutions: vec![],
            fun_type: None,
            _phantom: PhantomData,
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &'a str) -> LoxResult<'a, ()> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name) {
                return Err(LoxError::RuntimeError(format!(
                    "Already defined variable: {}",
                    name
                )));
            }
            scope.insert(name, false);
        }
        Ok(())
    }

    fn define(&mut self, name: &'a str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, true);
        }
    }

    pub fn resolve(&mut self, program: &Program<'a>) -> LoxResult<'a, ()> {
        for stmt in program.iter() {
            self.resolve_stmt(stmt)?;
        }
        Ok(())
    }

    fn resolve_block(&mut self, block: &Vec<Spanned<Stmt<'a>>>) -> LoxResult<'a, ()> {
        self.begin_scope();
        for stmt in block {
            self.resolve_stmt(stmt)?;
        }
        self.end_scope();
        Ok(())
    }

    fn resolve_local(&mut self, name: &'a str) {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name) {
                self.resolutions.push(Resolution { name, distance: i });
                return;
            }
        }
    }

    fn resolve_stmt(&mut self, stmt: &Spanned<Stmt<'a>>) -> LoxResult<'a, ()> {
        match &stmt.1 {
            Stmt::Class(_class) => todo!("resolve class"),
            Stmt::Var(var) => self.resolve_var_stmt(var),
            Stmt::Fun(fun) => self.resolve_fun_stmt(fun, FunType::Fun),
            Stmt::Expr(expr) => self.resolve_expr(expr),
            Stmt::Block(block) => self.resolve_block(block),
            Stmt::Print(expr) => self.resolve_expr(expr),
            Stmt::Return(ret) => self.resolve_return(ret),
            Stmt::While(while_stmt) => self.resolve_while(while_stmt),
            Stmt::If(if_stmt) => self.resolve_if(if_stmt),
        }
    }

    fn resolve_var_stmt(&mut self, var: &Spanned<VarStmt<'a>>) -> LoxResult<'a, ()> {
        let name = var.1.lhs.1;
        self.declare(name)?;
        if let Some(expr) = &var.1.rhs {
            self.resolve_expr(expr)?;
        }
        self.define(name);
        Ok(())
    }

    fn resolve_fun_stmt(
        &mut self,
        fun: &Spanned<FunStmt<'a>>,
        fun_type: FunType,
    ) -> LoxResult<'a, ()> {
        self.declare(fun.1.name.1)?;
        self.define(fun.1.name.1);

        let enclosing_fun_type = self.fun_type.take();
        self.fun_type = Some(fun_type);

        self.begin_scope();
        for param in &fun.1.params {
            self.declare(param.1)?;
            self.define(param.1);
        }
        for stmt in &fun.1.body.1 {
            self.resolve_stmt(stmt)?;
        }
        self.end_scope();

        self.fun_type = enclosing_fun_type;
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Spanned<Expr<'a>>) -> LoxResult<'a, ()> {
        match &expr.1 {
            Expr::Primary(primary) => self.resolve_primary(primary),
            Expr::Assign(assign) => self.resolve_assign(assign),
            Expr::Binary(binary) => self.resolve_binary(binary),
            Expr::Call(call) => self.resolve_call(call),
            Expr::Unary(unary) => self.resolve_expr(&unary.1.rhs),
            Expr::Grouping(grouping) => self.resolve_expr(grouping),
        }
    }

    fn resolve_return(&mut self, ret: &Option<Spanned<Expr<'a>>>) -> LoxResult<'a, ()> {
        if self.fun_type.is_none() {
            return Err(LoxError::RuntimeError(
                "Cannot return from top-level code".to_string(),
            ));
        }
        if let Some(expr) = ret {
            self.resolve_expr(expr)?;
        }
        Ok(())
    }

    fn resolve_while(&mut self, while_stmt: &Spanned<While<'a>>) -> LoxResult<'a, ()> {
        self.resolve_expr(&while_stmt.1.cond)?;
        self.resolve_stmt(&while_stmt.1.body)?;
        Ok(())
    }

    fn resolve_if(&mut self, if_stmt: &Spanned<If<'a>>) -> LoxResult<'a, ()> {
        self.resolve_expr(&if_stmt.1.cond)?;
        self.resolve_stmt(&if_stmt.1.body)?;
        if let Some(else_body) = &if_stmt.1.else_body {
            self.resolve_stmt(else_body)?;
        }
        Ok(())
    }

    fn resolve_primary(&mut self, primary: &Spanned<Primary<'a>>) -> LoxResult<'a, ()> {
        match primary.1 {
            Primary::Identifier(identifier) => {
                self.resolve_identifier(&(primary.0.clone(), identifier))
            }
            Primary::String(_) | Primary::Number(_) | Primary::Bool(_) | Primary::Nil => Ok(()),
            Primary::Super(_) => todo!(),
        }
    }

    fn resolve_assign(&mut self, assign: &Spanned<Assign<'a>>) -> LoxResult<'a, ()> {
        self.resolve_expr(&assign.1.rhs)?;
        self.resolve_local(assign.1.lhs.1);
        Ok(())
    }

    fn resolve_binary(&mut self, binary: &Spanned<Binary<'a>>) -> LoxResult<'a, ()> {
        self.resolve_expr(&binary.1.lhs)?;
        self.resolve_expr(&binary.1.rhs)
    }

    fn resolve_call(&mut self, call: &Spanned<Call<'a>>) -> LoxResult<'a, ()> {
        self.resolve_expr(&call.1.callee)?;
        for arg in &call.1.args {
            self.resolve_expr(arg)?;
        }
        Ok(())
    }

    fn resolve_identifier(&mut self, identifier: &Spanned<&'a str>) -> LoxResult<'a, ()> {
        if let Some(scope) = self.scopes.last() {
            if let Some(false) = scope.get(identifier.1) {
                return Err(LoxError::RuntimeError(format!(
                    "Cannot read local variable in its own initializer: {}",
                    identifier.1
                )));
            }
        }
        self.resolve_local(identifier.1);
        Ok(())
    }

    pub fn apply_resolutions(self, interpreter: &mut Interpreter<'a>) {
        for resolution in self.resolutions {
            interpreter.resolve(resolution.name, resolution.distance);
        }
    }
}
