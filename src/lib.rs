use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Program {
    instrs: Vec<Instr>,
    labels: HashMap<Label, usize>,
}

#[derive(Clone, Debug)]
enum Instr {
    Mov(Register, Expr),
    Add(Register, Expr),
    Out,
    Jif(Label),
    Jmp(Label),
    Cge(Expr, Expr),
}

#[derive(Clone, Copy, Debug)]
enum Expr {
    Literal(u8),
    Register(Register),
}

#[derive(Clone, Copy, Debug)]
enum Register {
    A,
    B,
    I,
    J,
}
impl Register {
    pub fn to_number(&self) -> usize {
        match self {
            Self::A => 0,
            Self::B => 1,
            Self::I => 8,
            Self::J => 9,
            _ => 17,
        }
    }
}

#[derive(Clone, Copy, Debug, Default)]
struct Registers([u8; 16]);

impl Registers {
    pub fn get(&self, register: &Register) -> u8 {
        self.0[register.to_number()]
    }
    pub fn get_mut(&mut self, register: &Register) -> &mut u8 {
        &mut self.0[register.to_number()]
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Label(String);

pub fn run_program(program: Program) {
    let Program { instrs, labels } = program;

    let mut registers = Registers::default();

    fn evaluate(expr: &Expr, registers: &Registers) -> u8 {
        match expr {
            Expr::Literal(value) => *value,
            Expr::Register(reg) => registers.get(reg),
        }
    }

    macro_rules! eval {
        ( $expr:expr ) => {
            evaluate($expr, &registers)
        };
    }

    macro_rules! get {
        ( $reg:expr ) => {
            registers.get($reg)
        };
        ( mut $reg:expr ) => {
            registers.get_mut($reg)
        };
        ( const $reg:ident ) => {
            registers.get(&Register::$reg)
        };
        ( const mut $reg:ident ) => {
            registers.get_mut(&Register::$reg)
        };
    }

    let mut i: usize = 0;
    loop {
        let Some(instr) = instrs.get(i) else {
            break;
        };
        // println!("{:?}", instr);
        match instr {
            Instr::Mov(reg, a) => *get!(mut reg) = eval!(a),
            Instr::Add(reg, a) => *get!(mut reg) += eval!(a),
            Instr::Out => println!("{}", get!(const B)),
            Instr::Jmp(label) => {
                i = *labels.get(label).unwrap();
                continue;
            }
            Instr::Jif(label) => {
                if get!(const A) > 0 {
                    i = *labels.get(label).unwrap();
                    continue;
                }
            }
            Instr::Cge(a, b) => *get!(const mut A) = if eval!(a) >= eval!(b) { 1 } else { 0 },
            // _ => (),
        }
        i += 1;
    }
}

pub fn parse_program(file: &str) -> Program {
    let mut instrs = Vec::new();
    let mut labels = HashMap::new();
    let mut line_number = 0;

    for line in file.lines() {
        let mut words = split_line(line).into_iter();
        let Some(instr_name) = words.next() else {
            continue;
        };
        line_number += 1;

        if instr_name.starts_with(':') {
            let Some(label) = parse_label(instr_name) else {
                panic!("expected label, found `{}`", instr_name);
            };
            if labels.contains_key(&label) {
                panic!("label already exists: `{}`", label.0);
            }
            labels.insert(label, line_number - 1);
            continue;
        }

        macro_rules! next {
            (register) => {
                match words.next() {
                    Some(word) => match parse_register(word) {
                        Some(register) => register,
                        None => panic!("expected register, found `{}`", word),
                    },
                    None => panic!("expected register, found end of statement"),
                }
            };
            (expr) => {
                match words.next() {
                    Some(word) => match parse_expr(word) {
                        Some(expr) => expr,
                        None => panic!("expected expression, found `{}`", word),
                    },
                    None => panic!("expected expression, found end of statement"),
                }
            };
            (label) => {
                match words.next() {
                    Some(word) => match parse_label(word) {
                        Some(label) => label,
                        None => panic!("expected label, found `{}`", word),
                    },
                    None => panic!("expected label, found end of statement"),
                }
            };
        }

        let instr = match instr_name {
            "mov" => Instr::Mov(next!(register), next!(expr)),
            "add" => Instr::Add(next!(register), next!(expr)),
            "out" => Instr::Out,
            "jmp" => Instr::Jmp(next!(label)),
            "jif" => Instr::Jif(next!(label)),
            "cge" => Instr::Cge(next!(expr), next!(expr)),
            // _ => continue,
            _ => panic!("unknown instruction `{}`", instr_name),
        };

        if let Some(word) = words.next() {
            panic!("expected end of statement, found `{}`", word);
        }

        instrs.push(instr);
    }

    for instr in &instrs {
        let label = match instr {
            Instr::Jmp(label) => label,
            Instr::Jif(label) => label,
            _ => continue,
        };
        if !labels.contains_key(label) {
            panic!("label not found `{}`", label.0);
        }
    }

    Program { instrs, labels }
}

fn parse_label(string: &str) -> Option<Label> {
    let mut chars = string.chars();
    if chars.next() != Some(':') {
        return None;
    }
    let label = chars.as_str().to_string();
    Some(Label(label))
}

fn parse_expr(string: &str) -> Option<Expr> {
    if string.starts_with('\'') {
        let register = parse_register(string)?;
        return Some(Expr::Register(register));
    }
    if let Ok(number) = string.parse() {
        return Some(Expr::Literal(number));
    }
    None
}

fn parse_register(string: &str) -> Option<Register> {
    let mut chars = string.chars();
    if chars.next() != Some('\'') {
        return None;
    }
    Some(match chars.as_str() {
        "a" => Register::A,
        "b" => Register::B,
        "i" => Register::I,
        "j" => Register::J,
        _ => return None,
    })
}

fn split_line(line: &str) -> Vec<&str> {
    let line = line.trim();
    if line.is_empty() {
        return Vec::new();
    }

    let mut tokens = Vec::new();
    let mut start = 0;
    let mut in_quote = false;

    for (i, ch) in line.char_indices() {
        if ch == '"' {
            in_quote = !in_quote;
        }
        if in_quote {
            continue;
        }

        if ch == ' ' {
            if i > start {
                tokens.push(&line[start..i]);
            }
            start = i + 1;
        } else if ch == '.' {
            if i > start {
                tokens.push(&line[start..i]);
            }
            return tokens;
        }
    }
    tokens.push(&line[start..]);
    tokens
}
