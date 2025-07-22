#ifndef STATEMENTS_COMP_H
#define STATEMENTS_COMP_H

#include <string>
#include "../decafcomp.cc"  // Include the base definitions (decafAST, decafStmtList, getString)

class IfAST : public decafAST {
    decafAST* Cond;
    decafAST* Then;
    decafAST* Else;
public:
    IfAST(decafAST* cond, decafAST* thenB, decafAST* elseB) : Cond(cond), Then(thenB), Else(elseB) {}
    ~IfAST() { delete Cond; delete Then; if (Else) delete Else; }
    string str() override {
        return "IfStmt(" + getString(Cond) + "," + getString(Then) + "," + (Else ? getString(Else) : "None") + ")";
    }

    llvm::Value* Codegen(CodegenContext& ctx) override {
        llvm::Value* condVal = Cond->Codegen(ctx);
        
        // Convert the condition to a boolean (i1) by comparing against 0
        condVal = ctx.builder.CreateICmpNE(
            condVal, 
            llvm::ConstantInt::get(condVal->getType(), 0), 
            "ifcond"
        );

        // Get the current function of which we are appending basic blocks
        llvm::Function* function = ctx.builder.GetInsertBlock()->getParent();

        // Create basic blocks for the 'then' branch, 
        // optional 'else' branch, 
        // and the merge (continue) point
        llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(ctx.llvmContext, "then", function);
        llvm::BasicBlock* elseBB = Else ? llvm::BasicBlock::Create(ctx.llvmContext, "else", function) : nullptr;
        
        // The merge block is when the control flow rejoins/merged together.
        // For example:
        //      if (cond) {
        //          then block
        //      } else {
        //          else block
        //      }
        //      merge block (code here runs after the if/else)
        llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(ctx.llvmContext, "ifcont", function);

        // Create conditional branch based on the condition's value
        if (Else)
            ctx.builder.CreateCondBr(condVal, thenBB, elseBB);
        else
            ctx.builder.CreateCondBr(condVal, thenBB, mergeBB);

        // Generate 'then' block
        ctx.builder.SetInsertPoint(thenBB);
        Then->Codegen(ctx);

        // If 'then' block didn't end with a terminator (like return/br), 
        // add a branch to merge block
        if (!thenBB->getTerminator())
            ctx.builder.CreateBr(mergeBB);

        // Generate 'else' block if exists
        if (Else) {
            ctx.builder.SetInsertPoint(elseBB);
            Else->Codegen(ctx);
            if (!elseBB->getTerminator())
                ctx.builder.CreateBr(mergeBB);
        }

        // Continue code generation at the merge point after the if-else
        ctx.builder.SetInsertPoint(mergeBB);

        // An if statement doesn't return a value
        return nullptr;
    }
};

class WhileAST : public decafAST {
    decafAST* Cond;
    decafAST* Body;
public:
    WhileAST(decafAST* cond, decafAST* body) : Cond(cond), Body(body) {}
    ~WhileAST() { delete Cond; delete Body; }
    string str() override {
        return "WhileStmt(" + getString(Cond) + "," + getString(Body) + ")";
    }

    llvm::Value* Codegen(CodegenContext& ctx) override {
        // Get the function that contains the current basic block
        llvm::Function* function = ctx.builder.GetInsertBlock()->getParent();

        // Create three basic blocks for the loop:
        // condBB: evaluates the loop condition
        // bodyBB: executes the loop body
        // afterBB: continues execution after the loop ends
        llvm::BasicBlock* condBB = llvm::BasicBlock::Create(ctx.llvmContext, "while.cond", function);
        llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(ctx.llvmContext, "while.body", function);
        llvm::BasicBlock* afterBB = llvm::BasicBlock::Create(ctx.llvmContext, "while.end", function);

        // Unconditional branch to the loop condition block
        ctx.builder.CreateBr(condBB);

        // Start inserting instructions into the condition block
        ctx.builder.SetInsertPoint(condBB);

        // Generate code for the loop condition expression
        llvm::Value* condVal = Cond->Codegen(ctx);

        // Convert the result to a boolean by checking if it is not equal to 0
        condVal = ctx.builder.CreateICmpNE(
            condVal,
            llvm::ConstantInt::get(condVal->getType(), 0), 
            "whilecond"
        );

        // Conditional branch based on the evaluated condition:
        // if true, jump to the loop body
        // if false, jump to the block after the loop
        ctx.builder.CreateCondBr(condVal, bodyBB, afterBB);

        // Push loop control targets onto the stack
        // Used for handling break (afterBB) and continue (condBB) statements
        ctx.loopContinueStack.push(condBB);
        ctx.loopBreakStack.push(afterBB);

        // Generate code in the body block
        ctx.builder.SetInsertPoint(bodyBB);
        Body->Codegen(ctx);

        // If the body block doesn't already end in a terminator, 
        // loop back to the condition
        if (!ctx.builder.GetInsertBlock()->getTerminator())
            ctx.builder.CreateBr(condBB);

        // Pop the loop control stacks after finishing the loop
        ctx.loopContinueStack.pop();
        ctx.loopBreakStack.pop();

        // Set insertion point to the block after the loop, for subsequent code
        ctx.builder.SetInsertPoint(afterBB);

        // A while loop doesn’t return a value
        return nullptr;
    }
};

class ForAST : public decafAST {
    decafAST* Init;
    decafAST* Cond;
    decafAST* Update;
    decafAST* Body;
public:
    ForAST(decafAST* init, decafAST* cond, decafAST* update, decafAST* body)
        : Init(init), Cond(cond), Update(update), Body(body) {}
    ~ForAST() { delete Init; delete Cond; delete Update; delete Body; }
    string str() override {
        return "ForStmt(" + getString(Init) + "," + getString(Cond) + "," + getString(Update) + "," + getString(Body) + ")";
    }

    llvm::Value* Codegen(CodegenContext& ctx) override {
        // Get the function that contains the current basic block
        llvm::Function* function = ctx.builder.GetInsertBlock()->getParent();

        // Create basic blocks for:
        // condBB: evaluates the loop condition
        // bodyBB: contains the loop body
        // stepBB: updates the loop variable (e.g., i++)
        // afterBB: the block after the loop
        llvm::BasicBlock* condBB = llvm::BasicBlock::Create(ctx.llvmContext, "for.cond", function);
        llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(ctx.llvmContext, "for.body", function);
        llvm::BasicBlock* stepBB = llvm::BasicBlock::Create(ctx.llvmContext, "for.step", function);
        llvm::BasicBlock* afterBB = llvm::BasicBlock::Create(ctx.llvmContext, "for.end", function);

        // Generate code for the loop initializer (e.g., int i = 0;)
        Init->Codegen(ctx);

        // Unconditionally branch (go-to condition) to the condition evaluation block
        // For example:
        //      i = 0;     
        //      goto condition;
        // 
        //      condition:
        //          if (i < 10)
        //              goto body;
        //          else
        //              goto end;
        ctx.builder.CreateBr(condBB);

        // Generate code for the condition block
        ctx.builder.SetInsertPoint(condBB);

        // Generate code for the loop condition expression
        llvm::Value* condVal = Cond->Codegen(ctx);

        // Convert the condition result to a boolean (i1)
        condVal = ctx.builder.CreateICmpNE(
            condVal,
            llvm::ConstantInt::get(condVal->getType(), 0),
            "forcond"
        );

        // If the condition is true, go to bodyBB; otherwise, exit to afterBB
        ctx.builder.CreateCondBr(condVal, bodyBB, afterBB);

        // Push the continue and break targets for this loop onto the context stacks
        // continue -> goes to stepBB
        // break -> goes to afterBB
        ctx.loopContinueStack.push(stepBB);
        ctx.loopBreakStack.push(afterBB);

        // Generate code for the loop body
        ctx.builder.SetInsertPoint(bodyBB);
        Body->Codegen(ctx);

        // If the loop body doesn't already terminate (e.g., with return/break), 
        // go to step block
        if (!ctx.builder.GetInsertBlock()->getTerminator())
            ctx.builder.CreateBr(stepBB);

        // Step block: generate the update expression (e.g., i++)
        ctx.builder.SetInsertPoint(stepBB);
        Update->Codegen(ctx);

        // After update, jump back to evaluate the condition again
        ctx.builder.CreateBr(condBB);

        // Pop the loop control stacks (exiting the loop scope)
        ctx.loopContinueStack.pop();
        ctx.loopBreakStack.pop();

        // Continue code generation after the loop
        ctx.builder.SetInsertPoint(afterBB);

        // For loops do not return a value
        return nullptr;
    }
};

class ReturnAST : public decafAST {
    decafAST* Expr;
public:
    ReturnAST(decafAST* e) : Expr(e) {}
    ~ReturnAST() { if (Expr) delete Expr; }
    string str() override {
        return "ReturnStmt(" + getString(Expr) + ")";
    }

    llvm::Value* Codegen(CodegenContext& ctx) override {
        if (Expr) {
            llvm::Value* val = Expr->Codegen(ctx);
            return ctx.builder.CreateRet(val);
        } else {
            return ctx.builder.CreateRetVoid();
        }
    }
};

class BreakAST : public decafAST {
public:
    string str() override { 
        return "BreakStmt"; 
    }

    llvm::Value* Codegen(CodegenContext& ctx) override {
        if (ctx.loopBreakStack.empty())
            throw std::runtime_error("Break outside of loop");
        return ctx.builder.CreateBr(ctx.loopBreakStack.top());
    }
};

class ContinueAST : public decafAST {
public:
    string str() override { 
        return "ContinueStmt"; 
    }

    llvm::Value* Codegen(CodegenContext& ctx) override {
        if (ctx.loopContinueStack.empty())
            throw std::runtime_error("Continue outside of loop");
        return ctx.builder.CreateBr(ctx.loopContinueStack.top());
    }
};

#endif // STATEMENTS_COMP_H