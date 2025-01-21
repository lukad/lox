use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::rc::Rc;

use crate::interpreter::{LoxError, LoxResult};
use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Env<'a> {
    pub(crate) vars: HashMap<String, Value<'a>>,
    parent: Option<Rc<RefCell<Env<'a>>>>,
}

impl<'a> Env<'a> {
    pub(crate) fn new() -> Self {
        Self {
            vars: HashMap::new(),
            parent: None,
        }
    }

    pub(crate) fn from(env: &Rc<RefCell<Env<'a>>>) -> Self {
        Self {
            vars: HashMap::new(),
            parent: Some(Rc::clone(env)),
        }
    }

    pub(crate) fn define(&mut self, name: &str, value: Value<'a>) -> LoxResult<'a, ()> {
        self.vars.insert(name.to_string(), value);
        Ok(())
    }

    pub(crate) fn get(&self, name: &str) -> Option<Value<'a>> {
        self.vars
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|p| p.borrow().get(name)))
    }

    pub(crate) fn get_at(&self, distance: usize, name: &str) -> Option<Value<'a>> {
        if distance == 0 {
            self.vars.get(name).cloned()
        } else {
            self.parent
                .as_ref()
                .and_then(|p| p.borrow().get_at(distance - 1, name))
        }
    }

    pub(crate) fn assign_at(
        &mut self,
        distance: usize,
        name: &str,
        value: Value<'a>,
    ) -> LoxResult<'a, ()> {
        if distance == 0 {
            self.assign(name, value)
        } else {
            self.parent
                .as_ref()
                .expect("no parent environment")
                .borrow_mut()
                .assign_at(distance - 1, name, value)
        }
    }

    pub(crate) fn assign(&mut self, name: &str, value: Value<'a>) -> LoxResult<'a, ()> {
        match self.vars.entry(name.to_string()) {
            Entry::Occupied(_) => {
                self.vars.insert(name.to_string(), value);
                Ok(())
            }
            Entry::Vacant(_) => {
                if let Some(parent) = &self.parent {
                    parent.borrow_mut().assign(name, value)?;
                    Ok(())
                } else {
                    Err(LoxError::RuntimeError(format!(
                        "undefined variable: {}",
                        name
                    )))
                }
            }
        }
    }
}
