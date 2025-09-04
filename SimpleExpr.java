// File Intro/SimpleExpr.java
// Java representation of expressions as in lecture 1
// sestoft@itu.dk * 2010-08-29

import java.util.Map;
import java.util.HashMap;


abstract class Expr {
    abstract public int eval(Map<String, Integer> env);
    abstract public Expr Simplify();
}

class CstI extends Expr {
    protected final int i;

    public CstI(int i) {
        this.i = i;
    }

    public int eval(Map<String, Integer> env) {
        return i;
    }

    @Override
    public Expr Simplify() {
        return this;
    }

    @Override
    public String toString() {
        return i + "";
    }
}

class Var extends Expr {
    protected final String name;

    public Var(String name) {
        this.name = name;
    }

    public int eval(Map<String, Integer> env) {
        return env.get(name);
    }

    @Override
    public Expr Simplify() {
        return this;
    }

    @Override
    public String toString() {
        return name;
    }
}

abstract class Binop extends Expr {
    protected final Expr e1, e2;

    Binop(Expr e1, Expr e2) {
        this.e1 = e1;
        this.e2 = e2;
    }
}

class Add extends Binop {
    public Add(Expr e1, Expr e2) {
        super(e1, e2);
    }

    @Override
    public Expr Simplify() {
        if (this.e1 instanceof CstI && ((CstI) this.e1).i == 0) {
            return e2.Simplify();
        } else if (this.e2 instanceof CstI && ((CstI) this.e2).i == 0)  {
            return e1.Simplify();
        } else  {
            return new Add(e1.Simplify(), e2.Simplify());
        }
    }

    @Override
    public int eval(Map<String, Integer> env) {
        return e1.eval(env) + e2.eval(env);
    }
    @Override
    public String toString() {
        return "(" + e1.toString() + " + " + e2.toString() + ")";
    }
}

class Mul extends Binop {
    public Mul(Expr e1, Expr e2) {
        super(e1, e2);
    }

    @Override
    public int eval(Map<String, Integer> env) {
        return e1.eval(env) * e2.eval(env);
    }
    @Override
    public String toString() {
        return "(" + e1.toString() + " * " + e2.toString() + ")";
    }

    @Override
    public Expr Simplify() {
        if (this.e1 instanceof CstI && ((CstI) this.e1).i == 0) {
            return new CstI(0);
        } else if (this.e2 instanceof CstI && ((CstI) this.e2).i == 0)  {
            return new CstI(0);
        } else if (this.e1 instanceof CstI && ((CstI) this.e1).i == 1) {
            return this.e2.Simplify();
        } else if (this.e2 instanceof CstI && ((CstI) this.e2).i == 1)  {
            return this.e1.Simplify();
        }
        else  {
            return new Mul(e1.Simplify(), e2.Simplify());
        }
    }
}

class Sub extends Binop {
    public Sub(Expr e1, Expr e2) {
        super(e1, e2);
    }

    @Override
    public int eval(Map<String, Integer> env) {
        return e1.eval(env) - e2.eval(env);
    }
    @Override
    public String toString() {
        return "(" + e1.toString() + " - " + e2.toString() + ")";
    }
    @Override
    public Expr Simplify() {
        if(this.e1 instanceof CstI && this.e2 instanceof CstI && ((CstI) this.e1).i == ((CstI) this.e2).i ) {
            return new CstI(0);
        }
        else if(this.e1 instanceof Var && this.e2 instanceof Var && ((Var) this.e1).name.equals(((Var) this.e2).name)) {
            return new CstI(0);
        } else {
            return new Sub(e1.Simplify(), e2.Simplify());
        }
    }
}


public class SimpleExpr {
    public static void main(String[] args) {
        Expr e = new Add(new CstI(17), new Var("z"));
        System.out.println(e.toString());
        Expr e1 = new Mul(new Mul(new CstI(17), new Var("n")), new Var("z"));
        System.out.println(e1.toString());
        Expr e2 = new Sub(new Add(new CstI(17), new Var("p")), new Var("z"));
        System.out.println(e2.toString());


        Expr a = new Sub (new Var ("x"), new Var ("x"));
        System.out.println(a.toString());
        System.out.println(a.Simplify().toString());
    }
}
