use crate::*;

pub(crate) fn stmt_to_string(stmt: &Stmt, s: &mut String) {
    match stmt {
        Stmt::Class(_class) => todo!("class"),
        Stmt::Fun(fun) => fun_to_string(&fun.1, s),
        Stmt::Var(var) => var_to_string(&var.1, s),
        Stmt::Expr(expr) => expr_to_string(&expr.1, s),
        Stmt::If(if_) => if_to_string(&if_.1, s),
        Stmt::Block(block) => block_to_string(block, s),
        Stmt::Print(print) => print_to_string(&print.1, s),
        Stmt::Return(expr) => return_to_string(expr.as_ref().map(|e| &e.1), s),
        Stmt::While(while_) => while_to_string(&while_.1, s),
    }
}

fn fun_to_string(fun: &FunStmt, s: &mut String) {
    s.push_str("($fun ");
    s.push_str(fun.name.1);
    s.push_str(" (");
    for (i, param) in fun.params.iter().enumerate() {
        if i > 0 {
            s.push(' ');
        }
        s.push_str(param.1);
    }
    s.push(')');
    s.push_str(" (");
    for (i, stmt) in fun.body.1.iter().enumerate() {
        if i > 0 {
            s.push(' ');
        }
        stmt_to_string(&stmt.1, s);
    }
    s.push_str("))");
}

fn var_to_string(var: &VarStmt, s: &mut String) {
    s.push_str("($var ");
    s.push_str(var.lhs.1);
    if let Some(rhs) = &var.rhs {
        s.push(' ');
        expr_to_string(&rhs.1, s);
    }
    s.push(')');
}

fn expr_to_string(expr: &Expr, s: &mut String) {
    match expr {
        Expr::Binary((_, Binary { op, lhs, rhs })) => {
            s.push('(');
            binop_to_string(&op.1, s);
            s.push(' ');
            expr_to_string(&lhs.1, s);
            s.push(' ');
            expr_to_string(&rhs.1, s);
            s.push(')');
        }
        Expr::Unary((_, Unary { op, rhs })) => {
            s.push('(');
            unaryop_to_string(&op.1, s);
            s.push(' ');
            expr_to_string(&rhs.1, s);
            s.push(')');
        }
        Expr::Grouping(group) => {
            expr_to_string(&group.1, s);
        }
        Expr::Primary(primary) => primary_to_string(&primary.1, s),
        Expr::Call((_, Call { callee, args })) => {
            s.push_str("($call ");
            expr_to_string(&callee.1, s);
            s.push_str(" (");
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    s.push(' ');
                }
                expr_to_string(&arg.1, s);
            }
            s.push_str("))");
        }
        Expr::Assign((_, assign)) => {
            s.push_str("($assign ");
            s.push_str(assign.lhs.1);
            s.push(' ');
            expr_to_string(&assign.rhs.1, s);
            s.push(')');
        }
    }
}

fn primary_to_string(primary: &Primary, s: &mut String) {
    match primary {
        Primary::Nil => s.push_str("nil"),
        Primary::Bool(b) => s.push_str(&b.to_string()),
        Primary::Number(n) => s.push_str(&n.to_string()),
        Primary::String(str) => s.push_str(str),
        Primary::Identifier(ident) => s.push_str(ident),
        Primary::Super(ident) => s.push_str(&format!("(super {})", ident)),
    }
}

fn binop_to_string(op: &BinaryOp, s: &mut String) {
    match op {
        BinaryOp::Add => s.push('+'),
        BinaryOp::Sub => s.push('-'),
        BinaryOp::Mul => s.push('*'),
        BinaryOp::Div => s.push('/'),
        BinaryOp::Eq => s.push_str("=="),
        BinaryOp::Ne => s.push_str("!="),
        BinaryOp::Lt => s.push('<'),
        BinaryOp::Gt => s.push('>'),
        BinaryOp::Le => s.push_str("<="),
        BinaryOp::Ge => s.push_str(">="),
        BinaryOp::And => s.push_str("and"),
        BinaryOp::Or => s.push_str("or"),
    }
}

fn unaryop_to_string(op: &UnaryOp, s: &mut String) {
    match op {
        UnaryOp::Neg => s.push('-'),
        UnaryOp::Not => s.push('!'),
    }
}

fn if_to_string(if_: &If, s: &mut String) {
    s.push_str("($if ");
    expr_to_string(&if_.cond.1, s);
    s.push(' ');
    stmt_to_string(&if_.body.1, s);
    if let Some(else_body) = &if_.else_body {
        s.push(' ');
        stmt_to_string(&else_body.1, s);
    }
    s.push(')');
}

fn block_to_string(block: &Vec<Spanned<Stmt>>, s: &mut String) {
    s.push_str("($block ");
    for (i, stmt) in block.iter().enumerate() {
        if i > 0 {
            s.push(' ');
        }
        stmt_to_string(&stmt.1, s);
    }
    s.push(')');
}

fn print_to_string(expr: &Expr, s: &mut String) {
    s.push_str("($print ");
    expr_to_string(expr, s);
    s.push(')');
}

fn return_to_string(expr: Option<&Expr>, s: &mut String) {
    if let Some(expr) = expr {
        s.push_str("($return ");
        expr_to_string(expr, s);
        s.push(')');
    } else {
        s.push_str("$return");
    }
}

fn while_to_string(while_: &While, s: &mut String) {
    s.push_str("($while ");
    expr_to_string(&while_.cond.1, s);
    s.push(' ');
    stmt_to_string(&while_.body.1, s);
    s.push(')');
}
