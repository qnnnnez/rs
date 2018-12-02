use std::rc::Rc;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

use sexpr::{SExpr, SAtom};

#[derive(Debug)]
pub enum Value {
    None,
    Integer(i64),
    Bool(bool),
    Char(char),
    Callable(Box<CallableValue>),
}

pub trait CallableValue {
    fn get_name(&self) -> Option<&str>;
    fn invoke(&self, &mut Interpreter, Vec<ValueRef>) -> Result<ValueRef, String>;
}

impl fmt::Debug for CallableValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.get_name() {
            Some(name) => {
                write!(f, "<callable {}>", name)
            },
            None => {
                write!(f, "<callable>")
            },
        }
    }
}

type ValueRef = Rc<Value>;

pub struct ValueFactory {
    none_value: ValueRef,
    true_value: ValueRef,
    false_value: ValueRef,
}

impl ValueFactory {
    pub fn new() -> ValueFactory {
        ValueFactory {
            none_value: ValueRef::new(Value::None),
            true_value: ValueRef::new(Value::Bool(true)),
            false_value: ValueRef::new(Value::Bool(false)),
        }
    }

    pub fn create_none(&mut self) -> ValueRef {
        self.none_value.clone()
    }

    pub fn create_bool(&mut self, value: bool) -> ValueRef {
        if value == true {
            self.true_value.clone()
        } else {
            self.false_value.clone()
        } 
    }

    pub fn create_integer(&mut self, value: i64) -> ValueRef {
        ValueRef::new(Value::Integer(value))
    }

    pub fn create_char(&mut self, value: char) -> ValueRef {
        ValueRef::new(Value::Char(value))
    }

    pub fn create_callable(&mut self, value: Box<CallableValue>) -> ValueRef {
        ValueRef::new(Value::Callable(value))
    }

    pub fn clone_value(&mut self, other: ValueRef) -> ValueRef {
        let atom_or_immutable = match other.as_ref() {
            Value::None => true,
            Value::Bool(_) => true,
            Value::Integer(_) => true,
            Value::Char(_) => true,
            Value::Callable(_) => true,
        };
        if atom_or_immutable {
            other.clone()
        } else {
            unreachable!()
        }
    }
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    CreateValue(ValueRef),
    CreateBinding{bindings: Vec<(u32, ASTNode)>, body: Vec<ASTNode>},    // (index, init-expr)
    DerefName(u32, String),
    FunctionCall(Vec<ASTNode>),
    Conditional{condition: Box<ASTNode>, then: Box<ASTNode>, or: Option<Box<ASTNode>>},
    CreateLambda{args: Vec<u32>, body: Vec<ASTNode>, closure: Vec<u32>},
}

pub struct Interpreter {
    value_factory: ValueFactory,
    name_map: BTreeMap<String, Vec<u32>>,    // var name => stack of index
    next_index: u32,
    locals: BTreeMap<u32, ValueRef>,
    globals: BTreeMap<String, ValueRef>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut result = Interpreter {
            value_factory: ValueFactory::new(),
            name_map: BTreeMap::new(),
            next_index: 1,
            locals: BTreeMap::new(),
            globals: BTreeMap::new(),
        };
        result.add_builtins();
        result
    }

    pub fn parse_sexpr(&mut self, sexpr: &SExpr, closure: &mut BTreeSet<u32>) -> Result<ASTNode, String> {
        match sexpr {
            SExpr::Atom(atom) => self.parse_satom(atom, closure),
            SExpr::List(list) => self.parse_sexpr_list(list, closure),
            SExpr::Quote(_) => {unimplemented!()}
        }
    }

    fn parse_satom(&mut self, atom: &SAtom, closure: &mut BTreeSet<u32>) -> Result<ASTNode, String> {
        match atom {
            SAtom::Symbol(name) => {
                let index = self.get_binding(name.as_ref());
                closure.insert(index);
                Ok(ASTNode::DerefName(index, name.clone()))
            },
            SAtom::Integer(value) => {
                Ok(ASTNode::CreateValue(self.value_factory.create_integer(*value)))
            },
            SAtom::Bool(value) => {
                Ok(ASTNode::CreateValue(self.value_factory.create_bool(*value)))
            },
            SAtom::Char(value) => {
                Ok(ASTNode::CreateValue(self.value_factory.create_char(*value)))
            }
        }
    }

    fn parse_sexpr_list(&mut self, list: &Vec<SExpr>, closure: &mut BTreeSet<u32>) -> Result<ASTNode, String> {
        if list.is_empty() {
            return Err("syntax error: missing procedure expression".to_string());
        }

        match &list[0] {
            SExpr::Atom(SAtom::Symbol(sym)) => {
                match sym.as_ref() {
                    "if" => {return self.parse_sexpr_if(list, closure)},
                    "let" => {return self.parse_sexpr_let(list, closure)},
                    "lambda" => {return self.parse_sexpr_lambda(list, closure)},
                    _ => { }
                } 
            },
            _ => { }
        }

        let mut result = vec![];
        for sexpr in list.into_iter() {
            result.push(self.parse_sexpr(sexpr, closure)?);
        }
        Ok(ASTNode::FunctionCall(result))
    }

    fn parse_sexpr_if(&mut self, list: &Vec<SExpr>, closure: &mut BTreeSet<u32>) -> Result<ASTNode, String> {
        if list.len() !=3 && list.len() != 4 {
            return Err("syntax error: if".to_string());
        }
        
        let or = if list.len() == 4 {
            Some(Box::new(self.parse_sexpr(&list[3], closure)?))
        } else {None};

        Ok(ASTNode::Conditional{
            condition: Box::new(self.parse_sexpr(&list[1], closure)?),
            then: Box::new(self.parse_sexpr(&list[2], closure)?),
            or: or,
        })
    }

    fn parse_sexpr_let(&mut self, list: &Vec<SExpr>, closure: &mut BTreeSet<u32>) -> Result<ASTNode, String> {
        if list.len() < 3 {
            return Err("syntax error: let".to_string());
        }

        let mut bindings = vec![];
        let mut names = vec![];
        match &list[1] {
            SExpr::List(sbindings) => {
                for binding in sbindings.into_iter() {
                    if let SExpr::List(binding) = binding {
                        if binding.len() != 2 {
                            return Err("syntax error: let".to_string());
                        }

                        let index = if let SExpr::Atom(SAtom::Symbol(name)) = &binding[0] {
                            names.push(name.clone());
                            self.create_binding(name.clone())
                        } else {
                            return Err("syntax error: let".to_string());
                        };
                        
                        bindings.push((index, self.parse_sexpr(&binding[1], closure)?));
                    } else {
                        return Err("syntax error: let".to_string());
                    }
                }
            },
            _ => {
                return Err("syntax error: let".to_string());
            }
        }

        let mut body = vec![];
        let mut it = list.into_iter();
        it.next();
        it.next();
        for sexpr in it {
            body.push(self.parse_sexpr(sexpr, closure)?);
        }

        let result = Ok(ASTNode::CreateBinding {bindings: bindings, body: body});

        for name in names {
            self.remove_binding(name.as_ref());
        }

        result
    }

    fn parse_sexpr_lambda(&mut self, list: &Vec<SExpr>, closure: &mut BTreeSet<u32>) -> Result<ASTNode, String> {
        if list.len() < 3 {
            return Err("syntax error: lambda".to_string());
        }

        let mut args = vec![];
        let mut names = vec![];
        if let SExpr::List(sargs) = &list[1] {
            for arg in sargs.into_iter() {
                if let SExpr::Atom(SAtom::Symbol(name)) = arg {
                    names.push(name.clone());
                    args.push(self.create_binding(name.clone()));
                } else {
                    return Err("syntax error: lambda".to_string());
                }
            }
        } else {
            return Err("syntax error: lambda".to_string());
        }

        let mut body = vec![];
        let mut it = list.into_iter();
        it.next();
        it.next();
        for sexpr in it {
            body.push(self.parse_sexpr(sexpr, closure)?);
        }

        let mut closure_list = vec![];
        for index in closure.iter() {
            closure_list.push(*index);
        }
        let result = Ok(ASTNode::CreateLambda {args: args, body: body, closure: closure_list});

        for name in names {
            self.remove_binding(name.as_ref());
        }

        result
    }

    fn get_binding(&mut self, name: &str) -> u32 {
        match self.name_map.get(name) {
            Some(stack) => {
                if stack.is_empty() {0} else {stack[stack.len() - 1]}
            },
            None => 0
        }
    }

    fn create_binding(&mut self, name: String) -> u32 {
        let stack = self.name_map.entry(name).or_insert(vec![]);
        let index = self.next_index;
        self.next_index += 1;
        stack.push(index);
        index
    }

    fn remove_binding(&mut self, name: &str) {
        let stack = self.name_map.get_mut(name).unwrap();
        stack.pop().unwrap();
    }

    pub fn eval_ast(&mut self, ast: &ASTNode) -> Result<ValueRef, String> {
        match ast {
            ASTNode::CreateValue(template_ref) => {
                Ok(self.value_factory.clone_value(template_ref.clone()))
            },
            ASTNode::DerefName(index, name) => {
                if *index == 0u32 {
                    if let Some(global_ref) = self.globals.get(name) {
                        Ok(global_ref.clone())
                    } else {
                        Err(format!("unknown symbol {}", name))
                    }
                } else {
                    Ok(self.locals.get(index).unwrap().clone())
                }
            },
            ASTNode::Conditional{condition, then, or} => {
                if let Value::Bool(result) = self.eval_ast(condition)?.as_ref() {
                    if *result {
                        self.eval_ast(then)
                    } else if let Some(or) = or {
                        self.eval_ast(or)
                    } else {
                        Ok(self.value_factory.create_none())
                    }
                } else {
                    Err("if condition is not bool type".to_string())
                }
            },
            ASTNode::FunctionCall(call) => {
                if call.is_empty() {
                    Err("empty function call".to_string())
                } else {
                    let mut it = call.into_iter();
                    let mut func = self.eval_ast(it.next().unwrap())?;
                    match func.as_ref() {
                        Value::Callable(callable) => {
                            let mut args = vec![];
                            for arg in it {
                                args.push(self.eval_ast(arg)?);
                            }
                            callable.invoke(self, args)
                        }
                        _ => Err("value is not callable".to_string())
                    }
                }
            },
            ASTNode::CreateBinding{bindings, body} => {
                for (index, ast) in bindings.into_iter() {
                    let value_ref = self.eval_ast(ast)?;
                    self.locals.insert(*index, value_ref);
                }

                let mut result = self.value_factory.create_none();
                for ast in body.into_iter() {
                    result = self.eval_ast(ast)?;
                }

                for (index, _) in bindings.into_iter() {
                    self.locals.remove(index);
                }

                Ok(result)
            }
            ASTNode::CreateLambda{args, body, closure} => {
                let lambda_value = self.create_lambda(args, body, closure);
                Ok(self.value_factory.create_callable(lambda_value))
            },
        }
    }

    fn create_lambda(&mut self, args: &Vec<u32>, body: &Vec<ASTNode>, closure: &Vec<u32>) -> Box<CallableValue> {
        struct LambdaCallable {
            args: Vec<u32>,
            body: Vec<ASTNode>,
            closure: Vec<(u32, ValueRef)>,
        }

        impl CallableValue for LambdaCallable {
            fn get_name(&self) -> Option<&str> {
                None
            }

            fn invoke(&self, interpreter: &mut Interpreter, arg_values: Vec<ValueRef>) -> Result<ValueRef, String> {
                if self.args.len() != arg_values.len() {
                    return Err("lambda argument count not match".to_string());
                }

                // inject arguments
                for i in 0..self.args.len() {
                    interpreter.locals.insert(self.args[i], arg_values[i].clone());
                }

                // inject closure values
                let mut injected_closure = vec![];
                for (index, value_ref) in self.closure.iter() {
                    if let None = interpreter.locals.get(index) {
                        interpreter.locals.insert(*index, value_ref.clone());
                        injected_closure.push(*index);
                    }
                }

                // eval body
                let mut result = interpreter.value_factory.create_none();
                for ast in self.body.iter() {
                    result = interpreter.eval_ast(&ast)?;
                }

                // clean up injected closure
                for index in injected_closure.into_iter() {
                    interpreter.locals.remove(&index);
                }

                // clean up injected arguments
                for i in 0..self.args.len()-1 {
                    interpreter.locals.remove(&self.args[i]);
                }

                Ok(result)
            }
        }

        let mut closure_captures = vec![];
        for index in closure.iter() {
            if let Some(value_ref) = self.locals.get(index) {
                closure_captures.push((*index, value_ref.clone()));
            }
        }

        Box::new(LambdaCallable {
            args: args.clone(),
            body: body.clone(),
            closure: closure_captures,
        })
    }

    fn add_builtins(&mut self) {
        struct BuiltinCallable {
            callable: Box<Fn(&mut Interpreter, Vec<ValueRef>) -> Result<ValueRef, String>>,
            name: String,
        }

        impl CallableValue for BuiltinCallable {
            fn get_name(&self) -> Option<&str> {
                Some(self.name.as_ref())
            }

            fn invoke(&self, interpreter: &mut Interpreter, arg_values: Vec<ValueRef>) -> Result<ValueRef, String> {
                (self.callable.as_ref())(interpreter, arg_values)
            }
        }

        fn add(interpreter: &mut Interpreter, arg_values: Vec<ValueRef>) -> Result<ValueRef, String> {
            let mut result = 0i64;
            for value_ref in arg_values.iter() {
                match value_ref.as_ref() {
                    Value::Integer(i) => {
                        result += *i;
                    },
                    _ => {
                        return Err("+: argument is not integer".to_string());
                    }
                }
            }
            Ok(interpreter.value_factory.create_integer(result))
        }

        let mut inject = |name: &str, f: Box<Fn(&mut Interpreter, Vec<ValueRef>) -> Result<ValueRef, String>>| {
            self.globals.insert(name.to_string(), self.value_factory.create_callable(Box::new(BuiltinCallable{callable: f, name: name.to_string()})));
        };

        inject("+", Box::new(add));
    }
}