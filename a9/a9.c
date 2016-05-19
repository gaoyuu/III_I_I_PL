#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "a9.h"

void *ktr_emptyr__m__k(void *dismount) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _emptyr__m__k_kt;
  _data->u._emptyr__m__k._dismount = dismount;
  return (void *)_data;
}

void *ktr_innerr__m__multr__m__k(void *vr__ex__, void *returnr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _innerr__m__multr__m__k_kt;
  _data->u._innerr__m__multr__m__k._vr__ex__ = vr__ex__;
  _data->u._innerr__m__multr__m__k._returnr__ex__ = returnr__ex__;
  return (void *)_data;
}

void *ktr_outerr__m__multr__m__k(void *e, void *env, void *returnr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _outerr__m__multr__m__k_kt;
  _data->u._outerr__m__multr__m__k._e = e;
  _data->u._outerr__m__multr__m__k._env = env;
  _data->u._outerr__m__multr__m__k._returnr__ex__ = returnr__ex__;
  return (void *)_data;
}

void *ktr_constructorr__m__subr1(void *returnr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _constructorr__m__subr1_kt;
  _data->u._constructorr__m__subr1._returnr__ex__ = returnr__ex__;
  return (void *)_data;
}

void *ktr_constructorr__m__zero(void *returnr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _constructorr__m__zero_kt;
  _data->u._constructorr__m__zero._returnr__ex__ = returnr__ex__;
  return (void *)_data;
}

void *ktr_constructorr__m__if(void *conseq, void *alt, void *env, void *returnr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _constructorr__m__if_kt;
  _data->u._constructorr__m__if._conseq = conseq;
  _data->u._constructorr__m__if._alt = alt;
  _data->u._constructorr__m__if._env = env;
  _data->u._constructorr__m__if._returnr__ex__ = returnr__ex__;
  return (void *)_data;
}

void *ktr_constructorr__m__let(void *body, void *env, void *returnr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _constructorr__m__let_kt;
  _data->u._constructorr__m__let._body = body;
  _data->u._constructorr__m__let._env = env;
  _data->u._constructorr__m__let._returnr__ex__ = returnr__ex__;
  return (void *)_data;
}

void *ktr_innerr__m__throwr__m__k(void *er1) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _innerr__m__throwr__m__k_kt;
  _data->u._innerr__m__throwr__m__k._er1 = er1;
  return (void *)_data;
}

void *ktr_outerr__m__throwr__m__k(void *er0, void *env) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _outerr__m__throwr__m__k_kt;
  _data->u._outerr__m__throwr__m__k._er0 = er0;
  _data->u._outerr__m__throwr__m__k._env = env;
  return (void *)_data;
}

void *ktr_innerr__m__appr__m__k(void *vr__ex__, void *returnr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _innerr__m__appr__m__k_kt;
  _data->u._innerr__m__appr__m__k._vr__ex__ = vr__ex__;
  _data->u._innerr__m__appr__m__k._returnr__ex__ = returnr__ex__;
  return (void *)_data;
}

void *ktr_outerr__m__appr__m__k(void *rand, void *env, void *returnr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _outerr__m__appr__m__k_kt;
  _data->u._outerr__m__appr__m__k._rand = rand;
  _data->u._outerr__m__appr__m__k._env = env;
  _data->u._outerr__m__appr__m__k._returnr__ex__ = returnr__ex__;
  return (void *)_data;
}

void *closr_closure(void *body, void *envr__m__cps) {
clos* _data = (clos*)malloc(sizeof(clos));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _closure_clos;
  _data->u._closure._body = body;
  _data->u._closure._envr__m__cps = envr__m__cps;
  return (void *)_data;
}

void *envrr_emptyr__m__env() {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _emptyr__m__env_envr;
  return (void *)_data;
}

void *envrr_extendr__m__env(void *a, void *envr__m__cps) {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _extendr__m__env_envr;
  _data->u._extendr__m__env._a = a;
  _data->u._extendr__m__env._envr__m__cps = envr__m__cps;
  return (void *)_data;
}

void *exprr_const(void *cexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _const_expr;
  _data->u._const._cexp = cexp;
  return (void *)_data;
}

void *exprr_var(void *n) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _var_expr;
  _data->u._var._n = n;
  return (void *)_data;
}

void *exprr_if(void *test, void *conseq, void *alt) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _if_expr;
  _data->u._if._test = test;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  return (void *)_data;
}

void *exprr_mult(void *nexpr1, void *nexpr2) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _mult_expr;
  _data->u._mult._nexpr1 = nexpr1;
  _data->u._mult._nexpr2 = nexpr2;
  return (void *)_data;
}

void *exprr_subr1(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_expr;
  _data->u._subr1._nexp = nexp;
  return (void *)_data;
}

void *exprr_zero(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zero_expr;
  _data->u._zero._nexp = nexp;
  return (void *)_data;
}

void *exprr_letcc(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letcc_expr;
  _data->u._letcc._body = body;
  return (void *)_data;
}

void *exprr_throw(void *kexp, void *vexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throw_expr;
  _data->u._throw._kexp = kexp;
  _data->u._throw._vexp = vexp;
  return (void *)_data;
}

void *exprr_let(void *exp, void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _let_expr;
  _data->u._let._exp = exp;
  _data->u._let._body = body;
  return (void *)_data;
}

void *exprr_lambda(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _lambda_expr;
  _data->u._lambda._body = body;
  return (void *)_data;
}

void *exprr_app(void *rator, void *rand) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _app_expr;
  _data->u._app._rator = rator;
  _data->u._app._rand = rand;
  return (void *)_data;
}

int main()
{
expr__t__ = (void *)exprr_let(exprr_lambda(exprr_lambda(exprr_if(exprr_zero(exprr_var((void *)0)),exprr_const((void *)1),exprr_mult(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_subr1(exprr_var((void *)0))))))),exprr_mult(exprr_letcc(exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_throw(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_const((void *)4))))),exprr_const((void *)5)));
envr__m__cpsr__t__ = (void *)envrr_emptyr__m__env();
pcr__t__ = &valuer__m__ofr__m__cps;
mount_tram();
printf("Fact 5: %d\n", (int)vr__t__);}

void applyr__m__k()
{
kt* _c = (kt*)returnr__t__;
switch (_c->tag) {
case _emptyr__m__k_kt: {
void *dismount = _c->u._emptyr__m__k._dismount;
_trstr *trstr = (_trstr *)dismount;
longjmp(*trstr->jmpbuf, 1);
break; }
case _innerr__m__multr__m__k_kt: {
void *vr__ex__ = _c->u._innerr__m__multr__m__k._vr__ex__;
void *returnr__ex__ = _c->u._innerr__m__multr__m__k._returnr__ex__;
returnr__t__ = (void *)returnr__ex__;
vr__t__ = (void *)(void *)((int)vr__ex__ * (int)vr__t__);
pcr__t__ = &applyr__m__k;
break; }
case _outerr__m__multr__m__k_kt: {
void *e = _c->u._outerr__m__multr__m__k._e;
void *env = _c->u._outerr__m__multr__m__k._env;
void *returnr__ex__ = _c->u._outerr__m__multr__m__k._returnr__ex__;
expr__t__ = (void *)e;
envr__m__cpsr__t__ = (void *)env;
returnr__t__ = (void *)ktr_innerr__m__multr__m__k(vr__t__,returnr__ex__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _constructorr__m__subr1_kt: {
void *returnr__ex__ = _c->u._constructorr__m__subr1._returnr__ex__;
returnr__t__ = (void *)returnr__ex__;
vr__t__ = (void *)(void *)((int)vr__t__ - 1);
pcr__t__ = &applyr__m__k;
break; }
case _constructorr__m__zero_kt: {
void *returnr__ex__ = _c->u._constructorr__m__zero._returnr__ex__;
returnr__t__ = (void *)returnr__ex__;
vr__t__ = (void *)(vr__t__ == 0);
pcr__t__ = &applyr__m__k;
break; }
case _constructorr__m__if_kt: {
void *conseq = _c->u._constructorr__m__if._conseq;
void *alt = _c->u._constructorr__m__if._alt;
void *env = _c->u._constructorr__m__if._env;
void *returnr__ex__ = _c->u._constructorr__m__if._returnr__ex__;
if(vr__t__) {
  expr__t__ = (void *)conseq;
envr__m__cpsr__t__ = (void *)env;
returnr__t__ = (void *)returnr__ex__;
pcr__t__ = &valuer__m__ofr__m__cps;

} else {
  expr__t__ = (void *)alt;
envr__m__cpsr__t__ = (void *)env;
returnr__t__ = (void *)returnr__ex__;
pcr__t__ = &valuer__m__ofr__m__cps;

}
break; }
case _constructorr__m__let_kt: {
void *body = _c->u._constructorr__m__let._body;
void *env = _c->u._constructorr__m__let._env;
void *returnr__ex__ = _c->u._constructorr__m__let._returnr__ex__;
expr__t__ = (void *)body;
envr__m__cpsr__t__ = (void *)envrr_extendr__m__env(vr__t__,env);
returnr__t__ = (void *)returnr__ex__;
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _innerr__m__throwr__m__k_kt: {
void *er1 = _c->u._innerr__m__throwr__m__k._er1;
returnr__t__ = (void *)er1;
pcr__t__ = &applyr__m__k;
break; }
case _outerr__m__throwr__m__k_kt: {
void *er0 = _c->u._outerr__m__throwr__m__k._er0;
void *env = _c->u._outerr__m__throwr__m__k._env;
expr__t__ = (void *)er0;
envr__m__cpsr__t__ = (void *)env;
returnr__t__ = (void *)ktr_innerr__m__throwr__m__k(vr__t__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _innerr__m__appr__m__k_kt: {
void *vr__ex__ = _c->u._innerr__m__appr__m__k._vr__ex__;
void *returnr__ex__ = _c->u._innerr__m__appr__m__k._returnr__ex__;
clor__t__ = (void *)vr__ex__;
ar__t__ = (void *)vr__t__;
returnr__t__ = (void *)returnr__ex__;
pcr__t__ = &applyr__m__closure;
break; }
case _outerr__m__appr__m__k_kt: {
void *rand = _c->u._outerr__m__appr__m__k._rand;
void *env = _c->u._outerr__m__appr__m__k._env;
void *returnr__ex__ = _c->u._outerr__m__appr__m__k._returnr__ex__;
expr__t__ = (void *)rand;
envr__m__cpsr__t__ = (void *)env;
returnr__t__ = (void *)ktr_innerr__m__appr__m__k(vr__t__,returnr__ex__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
}
}

void applyr__m__closure()
{
clos* _c = (clos*)clor__t__;
switch (_c->tag) {
case _closure_clos: {
void *body = _c->u._closure._body;
void *envr__m__cps = _c->u._closure._envr__m__cps;
expr__t__ = (void *)body;
envr__m__cpsr__t__ = (void *)envrr_extendr__m__env(ar__t__,envr__m__cps);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
}
}

void applyr__m__env()
{
envr* _c = (envr*)envr__m__cpsr__t__;
switch (_c->tag) {
case _emptyr__m__env_envr: {
fprintf(stderr, "unbound identifier ~s");
 exit(1);
break; }
case _extendr__m__env_envr: {
void *a = _c->u._extendr__m__env._a;
void *envr__m__cps = _c->u._extendr__m__env._envr__m__cps;
if((yr__t__ == 0)) {
  vr__t__ = (void *)a;
pcr__t__ = &applyr__m__k;

} else {
  envr__m__cpsr__t__ = (void *)envr__m__cps;
yr__t__ = (void *)(void *)((int)yr__t__ - 1);
pcr__t__ = &applyr__m__env;

}
break; }
}
}

void valuer__m__ofr__m__cps()
{
expr* _c = (expr*)expr__t__;
switch (_c->tag) {
case _const_expr: {
void *cexp = _c->u._const._cexp;
vr__t__ = (void *)cexp;
pcr__t__ = &applyr__m__k;
break; }
case _var_expr: {
void *n = _c->u._var._n;
yr__t__ = (void *)n;
pcr__t__ = &applyr__m__env;
break; }
case _mult_expr: {
void *er0 = _c->u._mult._nexpr1;
void *er1 = _c->u._mult._nexpr2;
expr__t__ = (void *)er0;
returnr__t__ = (void *)ktr_outerr__m__multr__m__k(er1,envr__m__cpsr__t__,returnr__t__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _subr1_expr: {
void *e = _c->u._subr1._nexp;
expr__t__ = (void *)e;
returnr__t__ = (void *)ktr_constructorr__m__subr1(returnr__t__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _zero_expr: {
void *e = _c->u._zero._nexp;
expr__t__ = (void *)e;
returnr__t__ = (void *)ktr_constructorr__m__zero(returnr__t__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _if_expr: {
void *test = _c->u._if._test;
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
expr__t__ = (void *)test;
returnr__t__ = (void *)ktr_constructorr__m__if(conseq,alt,envr__m__cpsr__t__,returnr__t__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _letcc_expr: {
void *e = _c->u._letcc._body;
expr__t__ = (void *)e;
envr__m__cpsr__t__ = (void *)envrr_extendr__m__env(returnr__t__,envr__m__cpsr__t__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _throw_expr: {
void *er0 = _c->u._throw._kexp;
void *er1 = _c->u._throw._vexp;
expr__t__ = (void *)er0;
returnr__t__ = (void *)ktr_outerr__m__throwr__m__k(er1,envr__m__cpsr__t__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _let_expr: {
void *er0 = _c->u._let._exp;
void *er1 = _c->u._let._body;
expr__t__ = (void *)er0;
returnr__t__ = (void *)ktr_constructorr__m__let(er1,envr__m__cpsr__t__,returnr__t__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _lambda_expr: {
void *e = _c->u._lambda._body;
vr__t__ = (void *)closr_closure(e,envr__m__cpsr__t__);
pcr__t__ = &applyr__m__k;
break; }
case _app_expr: {
void *rator = _c->u._app._rator;
void *rand = _c->u._app._rand;
expr__t__ = (void *)rator;
returnr__t__ = (void *)ktr_outerr__m__appr__m__k(rand,envr__m__cpsr__t__,returnr__t__);
pcr__t__ = &valuer__m__ofr__m__cps;
break; }
}
}

int mount_tram ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
returnr__t__= (void *)ktr_emptyr__m__k(dismount);
for(;;) {
pcr__t__();
}
}
return 0;
}
