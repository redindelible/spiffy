use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use parser::Parser;
use crate::ir::{BlockId};

mod stream;
mod lexer;
mod parser;
mod ast;
mod ir;


#[derive(Copy, Clone)]
struct InstructionId {
    idx: u64
}

impl InstructionId {
    fn is_before(&self, other: &InstructionId) -> bool {
        self.idx <= other.idx
    }
}

struct ValueInfo {
    lives_across_call: bool,
    num_uses: usize
}

struct Linearized {
    block: ir::Block,
    value_info: HashMap<ir::Value, ValueInfo>
}


struct Linearize {
    instruction_id: u64,
    value_counter: u64,
    block_counter: u64,

    value_info: HashMap<ir::Value, ValueInfo>,
    live_values: HashSet<ir::Value>
}

impl Linearize {
    fn new() -> Linearize {
        Linearize {
            instruction_id: 0,
            value_counter: 0,
            block_counter: 0,

            value_info: HashMap::new(),
            live_values: HashSet::new()
        }
    }

    fn new_value(&mut self) -> ir::Value {
        let key = self.value_counter;
        self.value_counter += 1;
        let value = ir::Value::new(key);
        self.value_info.insert(value, ValueInfo {
            lives_across_call: false,
            num_uses: 0
        });
        value
    }

    fn next_id(&mut self) -> InstructionId {
        let idx = self.instruction_id;
        self.instruction_id += 1;
        InstructionId { idx }
    }

    fn next_block(&mut self) -> BlockId {
        let key = self.block_counter;
        self.block_counter += 1;
        BlockId { key }
    }

    fn start_lifetime(&mut self, value: ir::Value, at: InstructionId) {
        self.live_values.insert(value);
    }

    fn mark_use(&mut self, value: ir::Value, at: InstructionId) {
        self.live_values.remove(&value);
        self.value_info.get_mut(&value).unwrap().num_uses += 1;
        // self.last_use.entry(value).and_modify(|old_at| *old_at = at).or_insert(at);
    }
}

struct LinearizeContext<'a> {
    linearize: &'a RefCell<Linearize>,
    parent: Option<&'a mut LinearizeContext<'a>>,
    instructions: Vec<ir::Instruction>
}

impl<'a> LinearizeContext<'a> {
    fn from(linearize: &'a RefCell<Linearize>) -> LinearizeContext<'a> {
        LinearizeContext {
            linearize,
            parent: None,
            instructions: Vec::new()
        }
    }

    fn child(&'a mut self) -> LinearizeContext<'a> {
        LinearizeContext {
            linearize: self.linearize,
            parent: Some(self),
            instructions: Vec::new()
        }
    }

    fn new_value(&mut self) -> ir::Value {
        self.linearize.borrow_mut().new_value()
    }

    fn next_block(&mut self) -> BlockId {
        self.linearize.borrow_mut().next_block()
    }

    fn add_instruction(&mut self, instr: ir::Instruction) -> InstructionId {
        let id = self.linearize.borrow_mut().next_id();
        if let Some(out_value) = instr.written() {
            self.linearize.borrow_mut().start_lifetime(out_value, id);
        }
        for in_value in instr.used() {
            self.linearize.borrow_mut().mark_use(in_value, id);
        }

        self.instructions.push(instr);
        id
    }
}


fn linearize(ast: &ast::Function) -> Linearized {
    let linearize = RefCell::new(Linearize::new());
    let mut ctxt = LinearizeContext::from(&linearize);

    let block = linearize_block(&ast.block, &mut ctxt);

    Linearized {
        block,
        value_info: linearize.into_inner().value_info
    }
}

fn linearize_block<'a>(block: &ast::Block, ir: &'a mut LinearizeContext<'a>) -> ir::Block {
    let id = ir.next_block();
    let mut child = ir.child();

    for stmt in &block.stmts {
        linearize_stmt(stmt, &mut child);
    }

    let instructions = child.instructions;
    ir::Block { id, instructions }
}

fn linearize_stmt(stmt: &ast::Stmt, ir: &mut LinearizeContext) {
    match stmt {
        ast::Stmt::Expr(expr) => {
            linearize_expr(expr, ir);
        }
        ast::Stmt::Return(expr) => {
            let value = linearize_expr(expr, ir);
            ir.add_instruction(ir::Instruction::Return(value));
        }
    }
}

fn linearize_expr(expr: &ast::Expr, ir: &mut LinearizeContext) -> ir::Value {
    match expr {
        ast::Expr::Number(number) => {
            let out = ir.new_value();
            ir.add_instruction(ir::Instruction::LoadI32(out, *number));
            out
        }
    }
}


#[derive(Copy, Clone, Eq, PartialEq, Hash)]
enum Reg {
    SP,
    A0, A1, A2, A3,
    T0, T1, T2, T3, T4, T5, T6, T7,
    S0, S1, S2, S3, S4, S5, S6, S7,
}

impl Reg {
    fn t(n: usize) -> Reg {
        use Reg::*;

        match n {
            0 => T0,
            1 => T1,
            2 => T2,
            3 => T3,
            4 => T4,
            5 => T5,
            6 => T6,
            7 => T7,
            _ => panic!()
        }
    }

    fn s(n: usize) -> Reg {
        use Reg::*;

        match n {
            0 => S0,
            1 => S1,
            2 => S2,
            3 => S3,
            4 => S4,
            5 => S5,
            6 => S6,
            7 => S7,
            _ => panic!()
        }
    }

    fn n(&self) -> usize {
        use Reg::*;

        match self {
            S0 | T0 => 0,
            S1 | T1 => 1,
            S2 | T2 => 2,
            S3 | T3 => 3,
            S4 | T4 => 4,
            S5 | T5 => 5,
            S6 | T6 => 6,
            S7 | T7 => 7,
            _ => panic!()
        }
    }

    fn is_temp_reg(&self) -> bool {
        use Reg::*;

        match self {
            T0 | T1 | T2 | T3 | T4 | T5 | T6 | T7 => true,
            _ => false
        }
    }

    fn is_saved_reg(&self) -> bool {
        use Reg::*;

        match self {
            S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 => true,
            _ => false
        }
    }
}


impl Display for Reg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Reg::SP => "$sp",
            Reg::A0 => "$a0",
            Reg::A1 => "$a1",
            Reg::A2 => "$a2",
            Reg::A3 => "$a3",
            Reg::T0 => "$t0",
            Reg::T1 => "$t1",
            Reg::T2 => "$t2",
            Reg::T3 => "$t3",
            Reg::T4 => "$t4",
            Reg::T5 => "$t5",
            Reg::T6 => "$t6",
            Reg::T7 => "$t7",
            Reg::S0 => "$s0",
            Reg::S1 => "$s1",
            Reg::S2 => "$s2",
            Reg::S3 => "$s3",
            Reg::S4 => "$s4",
            Reg::S5 => "$s5",
            Reg::S6 => "$s6",
            Reg::S7 => "$s7",
        })
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
enum Location {
    Reg(Reg),
    Stack(u32)
}

impl Location {
    fn format(&self, r: impl FnOnce(Reg) -> String, s: impl FnOnce(u32) -> String) -> String {
        match self {
            Location::Reg(reg) => r(*reg),
            Location::Stack(slot) => s(*slot)
        }
    }
}

struct Assembler {
    data: String,
    text: String,
}

impl Assembler {
    fn assemble(file: &ast::File) -> String {
        let mut assembler = Assembler {
            data: String::new(),
            text: String::new(),
        };

        for top_level in &file.top_levels {
            match top_level {
                ast::TopLevel::Function(func) => {
                    AssembleFunction::assemble(&func, &mut assembler);
                }
            }
        }

        format!(".data\n{}\n.text\n{}", assembler.data, assembler.text)
    }
}

struct AssembleFunction {
    name: String,

    info: HashMap<ir::Value, ValueInfo>,
    text: Vec<String>,

    available_temporaries: [bool; 7],
    available_saved: [bool; 8],

    num_saved_used: usize,

    allocated: HashMap<ir::Value, Location>,
    remaining_uses: HashMap<ir::Value, usize>,
    stack: Vec<bool>
}

impl AssembleFunction {
    fn assemble(func: &ast::Function, assembler: &mut Assembler) {
        let linearized = linearize(func);

        let mut assemble_function = AssembleFunction {
            name: func.name.clone(),
            info: linearized.value_info,
            text: Vec::new(),

            available_temporaries: [true; 7],
            available_saved: [true; 8],

            num_saved_used: 0,

            allocated: HashMap::new(),
            remaining_uses: HashMap::new(),
            stack: Vec::new()
        };

        assemble_function.text.push(format!(".globl {}", &func.name));
        assemble_function.text.push(format!("{}:", &func.name));

        assemble_function.generate_block(&linearized.block);

        let total_stack_slots_used = assemble_function.stack.len() + assemble_function.num_saved_used + 1;
        let mut header = Vec::new();
        header.push(format!("  sub $sp, $sp, {}", total_stack_slots_used * 4));
        header.push(format!("  sw $ra, {}($sp)", (total_stack_slots_used  - 1) * 4));

        assemble_function.text.splice(2..2, header);

        let mut footer = Vec::new();
        footer.push(format!("return_{}:", &func.name));
        footer.push(format!("  lw $ra, {}($sp)", (total_stack_slots_used  - 1) * 4));
        footer.push(format!("  add $sp, $sp, {}", total_stack_slots_used * 4));
        footer.push(format!("  jr $ra"));

        assemble_function.text.extend_from_slice(&footer);

        for line in assemble_function.text {
            assembler.text.push_str(&line);
            assembler.text.push('\n');
        }
    }


    fn new_label(&mut self, label: impl AsRef<str>) {
        let label = label.as_ref().to_owned();
        self.text.push(format!("{}:", &label));
        // self.labels.insert(label);
    }

    fn instruction(&mut self, instr: impl AsRef<str>) {
        self.text.push(format!("  {}", instr.as_ref()));
    }

    fn allocate_stack(&mut self) -> u32 {
        for (i, slot) in self.stack.iter().enumerate() {
            if *slot {
                self.stack[i] = false;
                return i as u32 * 4;
            }
        }
        self.stack.push(false);
        return (self.stack.len() as u32 - 1) *4;
    }

    fn allocate_value(&mut self, value: &ir::Value) -> Location {
        let location = if self.info[value].lives_across_call {
            if let Some((i, _)) = self.available_saved.iter().enumerate().find(|(_, is_used)| **is_used) {
                if i > self.num_saved_used {
                    self.num_saved_used = i;
                }

                self.available_saved[i] = false;
                Location::Reg(Reg::s(i))
            } else {
                Location::Stack(self.allocate_stack())
            }
        } else {
            if let Some((i, _)) = self.available_temporaries.iter().enumerate().find(|(_, is_used)| **is_used) {
                self.available_temporaries[i] = false;
                Location::Reg(Reg::t(i))
            } else {
                Location::Stack(self.allocate_stack())
            }
        };
        self.allocated.insert(*value, location);
        self.remaining_uses.insert(*value, self.info[value].num_uses);
        location
    }

    fn use_value(&mut self, value: &ir::Value) -> Location {
        let location = *self.allocated.get(value).unwrap();
        let uses = self.remaining_uses.get_mut(value).unwrap();
        *uses -= 1;
        if *uses == 0 {
            self.allocated.remove(value);
            self.remaining_uses.remove(value);

            match location {
                Location::Reg(reg) => {
                    if reg.is_saved_reg() {
                        self.available_saved[reg.n()] = true;
                    } else {
                        self.available_temporaries[reg.n()] = true;
                    }
                }
                Location::Stack(offset) => {
                    self.stack[offset as usize] = true;
                }
            }
        }
        location
    }
    //
    // fn load_location(&mut self, location: Location) -> Reg {
    //     match location {
    //         Location::Reg(reg) => reg,
    //         Location::Stack(offset) => {
    //             self.instruction(format!("lw $t7, {}($sp)", offset));
    //             Reg::T7
    //         }
    //     }
    // }

    fn generate_block(&mut self, block: &ir::Block) {
        for instr in &block.instructions {
            self.generate_instr(instr)
        }
    }

    fn generate_instr(&mut self, instr: &ir::Instruction) {
        match instr {
            ir::Instruction::LoadI32(out, number) => {
                let loc = self.allocate_value(out);
                match loc {
                    Location::Reg(reg) => {
                        self.instruction(format!("li {}, {}", reg, number))
                    },
                    Location::Stack(slot) => {
                        self.instruction(format!("li $t7, {}", number));
                        self.instruction(format!("sw $t7, {}($sp)", slot));
                    }
                }
            }
            ir::Instruction::Return(value) => {
                let loc = self.use_value(value);
                match loc {
                    Location::Reg(reg) => {
                        self.instruction(format!("move $v0, {}", reg));
                    }
                    Location::Stack(offset) => {
                        self.instruction(format!("lw $v0, {}($sp)", offset));
                    }
                }
                self.instruction(format!("j return_{}", &self.name));
            }
        }
    }
}


fn main() {
    let file = Parser::parse("fn main() { return 0; }");

    print!("{}", Assembler::assemble(&file));
}
