void (*pcr__t__)();

void *returnr__t__, *vr__t__, *expr__t__, *envr__m__cpsr__t__, *clor__t__, *ar__t__, *yr__t__;

struct expr;
typedef struct expr expr;
struct expr {
  enum {
    _const_expr,
    _var_expr,
    _if_expr,
    _mult_expr,
    _subr1_expr,
    _zero_expr,
    _letcc_expr,
    _throw_expr,
    _let_expr,
    _lambda_expr,
    _app_expr
  } tag;
  union {
    struct { void *_cexp; } _const;
    struct { void *_n; } _var;
    struct { void *_test; void *_conseq; void *_alt; } _if;
    struct { void *_nexpr1; void *_nexpr2; } _mult;
    struct { void *_nexp; } _subr1;
    struct { void *_nexp; } _zero;
    struct { void *_body; } _letcc;
    struct { void *_kexp; void *_vexp; } _throw;
    struct { void *_exp; void *_body; } _let;
    struct { void *_body; } _lambda;
    struct { void *_rator; void *_rand; } _app;
  } u;
};

void *exprr_const(void *cexp);
void *exprr_var(void *n);
void *exprr_if(void *test, void *conseq, void *alt);
void *exprr_mult(void *nexpr1, void *nexpr2);
void *exprr_subr1(void *nexp);
void *exprr_zero(void *nexp);
void *exprr_letcc(void *body);
void *exprr_throw(void *kexp, void *vexp);
void *exprr_let(void *exp, void *body);
void *exprr_lambda(void *body);
void *exprr_app(void *rator, void *rand);

void valuer__m__ofr__m__cps();
struct envr;
typedef struct envr envr;
struct envr {
  enum {
    _emptyr__m__env_envr,
    _extendr__m__env_envr
  } tag;
  union {
    struct { char dummy; } _emptyr__m__env;
    struct { void *_a; void *_envr__m__cps; } _extendr__m__env;
  } u;
};

void *envrr_emptyr__m__env();
void *envrr_extendr__m__env(void *a, void *envr__m__cps);

void applyr__m__env();
struct clos;
typedef struct clos clos;
struct clos {
  enum {
    _closure_clos
  } tag;
  union {
    struct { void *_body; void *_envr__m__cps; } _closure;
  } u;
};

void *closr_closure(void *body, void *envr__m__cps);

void applyr__m__closure();
struct kt;
typedef struct kt kt;
struct kt {
  enum {
    _emptyr__m__k_kt,
    _innerr__m__multr__m__k_kt,
    _outerr__m__multr__m__k_kt,
    _constructorr__m__subr1_kt,
    _constructorr__m__zero_kt,
    _constructorr__m__if_kt,
    _constructorr__m__let_kt,
    _innerr__m__throwr__m__k_kt,
    _outerr__m__throwr__m__k_kt,
    _innerr__m__appr__m__k_kt,
    _outerr__m__appr__m__k_kt
  } tag;
  union {
    struct { void *_dismount; } _emptyr__m__k;
    struct { void *_vr__ex__; void *_returnr__ex__; } _innerr__m__multr__m__k;
    struct { void *_e; void *_env; void *_returnr__ex__; } _outerr__m__multr__m__k;
    struct { void *_returnr__ex__; } _constructorr__m__subr1;
    struct { void *_returnr__ex__; } _constructorr__m__zero;
    struct { void *_conseq; void *_alt; void *_env; void *_returnr__ex__; } _constructorr__m__if;
    struct { void *_body; void *_env; void *_returnr__ex__; } _constructorr__m__let;
    struct { void *_er1; } _innerr__m__throwr__m__k;
    struct { void *_er0; void *_env; } _outerr__m__throwr__m__k;
    struct { void *_vr__ex__; void *_returnr__ex__; } _innerr__m__appr__m__k;
    struct { void *_rand; void *_env; void *_returnr__ex__; } _outerr__m__appr__m__k;
  } u;
};

void *ktr_emptyr__m__k(void *dismount);
void *ktr_innerr__m__multr__m__k(void *vr__ex__, void *returnr__ex__);
void *ktr_outerr__m__multr__m__k(void *e, void *env, void *returnr__ex__);
void *ktr_constructorr__m__subr1(void *returnr__ex__);
void *ktr_constructorr__m__zero(void *returnr__ex__);
void *ktr_constructorr__m__if(void *conseq, void *alt, void *env, void *returnr__ex__);
void *ktr_constructorr__m__let(void *body, void *env, void *returnr__ex__);
void *ktr_innerr__m__throwr__m__k(void *er1);
void *ktr_outerr__m__throwr__m__k(void *er0, void *env);
void *ktr_innerr__m__appr__m__k(void *vr__ex__, void *returnr__ex__);
void *ktr_outerr__m__appr__m__k(void *rand, void *env, void *returnr__ex__);

void applyr__m__k();
int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

