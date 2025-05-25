use std::mem::take;

use swc_atoms::Atom;
use swc_common::Spanned;
use swc_ecma_ast::{
    ArrowExpr, AssignExpr, AssignTarget, CallExpr, Callee, Expr, ExprOrSpread, Ident, IdentName,
    Lit, MemberExpr, SimpleAssignTarget, Str,
};
use swc_ecma_visit::{VisitMut, VisitMutWith};

pub struct Tipper {
    pub haven: Ident,
}
impl VisitMut for Tipper {
    fn visit_mut_expr(&mut self, e: &mut Expr) {
        *e = match take(e) {
            Expr::Member(m) => {
                let mut o = m.obj;
                o.visit_mut_with(self);
                let mut a = match m.prop {
                    swc_ecma_ast::MemberProp::Ident(ident_name) => Expr::Lit(Lit::Str(Str {
                        span: ident_name.span,
                        value: ident_name.sym,
                        raw: None,
                    })),
                    swc_ecma_ast::MemberProp::PrivateName(private_name) => todo!(),
                    swc_ecma_ast::MemberProp::Computed(computed_prop_name) => {
                        *computed_prop_name.expr
                    }
                };
                a.visit_mut_with(self);
                Expr::Call(CallExpr {
                    span: m.span,
                    ctxt: Default::default(),
                    callee: swc_ecma_ast::Callee::Expr(Box::new(
                        match Expr::Ident(self.haven.clone()) {
                            c => match c.span() {
                                s => Expr::Member(MemberExpr {
                                    span: c.span(),
                                    obj: Box::new(c),
                                    prop: swc_ecma_ast::MemberProp::Ident(IdentName {
                                        span: s,
                                        sym: Atom::new("get"),
                                    }),
                                }),
                            },
                        },
                    )),
                    args: [o, Box::new(a)]
                        .into_iter()
                        .map(|a| ExprOrSpread {
                            expr: a,
                            spread: None,
                        })
                        .collect(),
                    type_args: None,
                })
            }
            Expr::Assign(AssignExpr {
                span,
                op,
                mut right,
                left: AssignTarget::Simple(SimpleAssignTarget::Member(m)),
            }) => {
                right.visit_mut_with(self);
                let mut o = m.obj;
                o.visit_mut_with(self);
                let mut a = match m.prop {
                    swc_ecma_ast::MemberProp::Ident(ident_name) => Expr::Lit(Lit::Str(Str {
                        span: ident_name.span,
                        value: ident_name.sym,
                        raw: None,
                    })),
                    swc_ecma_ast::MemberProp::PrivateName(private_name) => todo!(),
                    swc_ecma_ast::MemberProp::Computed(computed_prop_name) => {
                        *computed_prop_name.expr
                    }
                };
                a.visit_mut_with(self);
                Expr::Call(CallExpr {
                    span: m.span,
                    ctxt: Default::default(),
                    callee: swc_ecma_ast::Callee::Expr(Box::new(
                        match Expr::Ident(self.haven.clone()) {
                            c => match c.span() {
                                s => Expr::Member(MemberExpr {
                                    span: c.span(),
                                    obj: Box::new(c),
                                    prop: swc_ecma_ast::MemberProp::Ident(IdentName {
                                        span: s,
                                        sym: Atom::new("set"),
                                    }),
                                }),
                            },
                        },
                    )),
                    args: [o, Box::new(a), right]
                        .into_iter()
                        .chain(op.to_update().iter().cloned().map(|a| {
                            Box::new(Expr::Lit(Lit::Str(Str {
                                span: span,
                                value: Atom::new(a.to_string()),
                                raw: None,
                            })))
                        }))
                        .map(|a| ExprOrSpread {
                            expr: a,
                            spread: None,
                        })
                        .collect(),
                    type_args: None,
                })
            }

            Expr::Call(CallExpr {
                span,
                ctxt,
                callee,
                mut args,
                type_args,
            }) => {
                args.visit_mut_with(self);
                match callee {
                    Callee::Expr(mut e) => {
                        let mut e = *e;
                        match e {
                            Expr::Member(mut m) => {
                                m.visit_mut_with(self);
                                let mut a = match m.prop {
                                    swc_ecma_ast::MemberProp::Ident(ident_name) => {
                                        Expr::Lit(Lit::Str(Str {
                                            span: ident_name.span,
                                            value: ident_name.sym,
                                            raw: None,
                                        }))
                                    }
                                    swc_ecma_ast::MemberProp::PrivateName(private_name) => todo!(),
                                    swc_ecma_ast::MemberProp::Computed(computed_prop_name) => {
                                        *computed_prop_name.expr
                                    }
                                };
                                Expr::Call(CallExpr {
                                    span: m.span,
                                    ctxt: Default::default(),
                                    callee: swc_ecma_ast::Callee::Expr(Box::new(
                                        match Expr::Ident(self.haven.clone()) {
                                            c => match c.span() {
                                                s => Expr::Member(MemberExpr {
                                                    span: c.span(),
                                                    obj: Box::new(c),
                                                    prop: swc_ecma_ast::MemberProp::Ident(
                                                        IdentName {
                                                            span: s,
                                                            sym: Atom::new("call_member"),
                                                        },
                                                    ),
                                                }),
                                            },
                                        },
                                    )),
                                    args: [
                                        ExprOrSpread {
                                            expr: m.obj,
                                            spread: None,
                                        },
                                        ExprOrSpread {
                                            expr: Box::new(a),
                                            spread: None,
                                        },
                                    ]
                                    .into_iter()
                                    .chain(args)
                                    .collect(),
                                    type_args: None,
                                })
                            }
                            Expr::Ident(Ident {
                                span,
                                ctxt,
                                sym,
                                optional,
                            }) if sym.as_str() == "eval" => Expr::Call(CallExpr {
                                span,
                                ctxt,
                                callee: Callee::Expr(Box::new(Expr::Ident(Ident {
                                    span,
                                    ctxt,
                                    sym,
                                    optional,
                                }))),
                                args,
                                type_args: None,
                            }),
                            mut e => {
                                e.visit_mut_with(self);
                                Expr::Call(CallExpr {
                                    span: e.span(),
                                    ctxt: Default::default(),
                                    callee: swc_ecma_ast::Callee::Expr(Box::new(
                                        match Expr::Ident(self.haven.clone()) {
                                            c => match c.span() {
                                                s => Expr::Member(MemberExpr {
                                                    span: c.span(),
                                                    obj: Box::new(c),
                                                    prop: swc_ecma_ast::MemberProp::Ident(
                                                        IdentName {
                                                            span: s,
                                                            sym: Atom::new("call"),
                                                        },
                                                    ),
                                                }),
                                            },
                                        },
                                    )),
                                    args: [ExprOrSpread {
                                        expr: Box::new(e),
                                        spread: None,
                                    }]
                                    .into_iter()
                                    .chain(args)
                                    .collect(),
                                    type_args: None,
                                })
                            }
                        }
                    }
                    _ => todo!(),
                }
            }
            mut e => {
                e.visit_mut_children_with(self);
                e
            }
        }
    }
}
