typedef unsigned u;
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum { FORWARD = 27, REDUCING = 9 };

void die(char *s) {
  fprintf(stderr, "Error: %s\n", s);
  exit(1);
}

enum { TOP = 1 << 23, TABMAX = 1 << 10, BUFMAX = 1 << 20 };
u arena[2][TOP];
char input[BUFMAX] = {0};
u *mem, *altmem, *sp, *spTop, hp, tab[TABMAX], tabn;
u gc_count;
void stats() {
  printf("[HP = %u, SP = %p, GC called %u time%s]\n", hp, (void*)(spTop - sp),
         gc_count, gc_count == 1 ? "" : "s");
}

#define isAddr(n) (n >= 128)

u copy(u n) {
  if (!isAddr(n))
    return n;
  u x = mem[n];
  while (isAddr(x) && mem[x] == 'T') {
    mem[n] = mem[n + 1];
    mem[n + 1] = mem[x + 1];
    x = mem[n];
  }
  if (isAddr(x) && mem[x] == 'K') {
    mem[n + 1] = mem[x + 1];
    x = mem[n] = 'I';
  }
  u y = mem[n + 1];
  switch (x) {
  case FORWARD:
    return y;
  case REDUCING:
    if (hp >= TOP - 2)
      die("OOM");
    mem[n] = FORWARD;
    mem[n + 1] = hp;
    hp += 2;
    return mem[n + 1];
  case 'I':
    mem[n] = REDUCING;
    y = copy(y);
    if (mem[n] == FORWARD) {
      altmem[mem[n + 1]] = 'I';
      altmem[mem[n + 1] + 1] = y;
    } else {
      mem[n] = FORWARD;
      mem[n + 1] = y;
    }
    return mem[n + 1];
  default:
    break;
  }
  if (hp >= TOP - 2)
    die("OOM");
  u z = hp;
  hp += 2;
  mem[n] = FORWARD;
  mem[n + 1] = z;
  altmem[z] = copy(x);
  altmem[z + 1] = x == 'a' || x == '#' ? y : copy(y);
  return z;
}

/* Garbage collection */
void gc() {
  /* Reset the heap pointer */
  hp = 128;
  /* Set the stack pointer to point to the top of altmem */
  sp = altmem + TOP - 1;
  /* Run copy starting from the top of the old stack */
  *sp = copy(*spTop);
  spTop = sp;
  /* Swap the addresses of mem and altmem */
  u *tmp = mem;
  mem = altmem;
  altmem = tmp;
  gc_count++;
}

/* An application of two nodes is represented by adjacent memory */
/* locations. */
u app(u f, u x) {
  mem[hp] = f;
  mem[hp + 1] = x;
  hp += 2;
  return hp - 2;
}

u tab[TABMAX], tabn;

u parseTerm(u (*get)()) {
  u n, c;
  do
    c = get();
  while (c == '\n');
  switch (c) {
  case '`':
    c = parseTerm(get);
    return app(c, parseTerm(get));
  case '#':
    return app('#', get());
  case '@':
    return tab[get() - ' '];
  case '(':
    n = 0;
    while ((c = get()) != ')')
      n = 10 * n + c - '0';
    return app('#', n);
  case '[':
    n = 0;
    while ((c = get()) != ']')
      n = 10 * n + c - '0';
    return tab[n];
  default:
    return c;
  }
}

void reset(u root) { *(sp = spTop) = root; }

void parseMore(u (*get)()) {
  for (;;) {
    u c = parseTerm(get);
    if (!c) {
      reset(app(app(app(tab[tabn - 1], app('0', '?')), '.'), app('T', '1')));
      return;
    }
    if (tabn == TABMAX)
      die("Table overflow");
    tab[tabn++] = c;
    if (get() != ';')
      die("Expected ';'");
  }
}

char *str;
u str_get() { return *(unsigned char *)str++; }

void parse(char *s) {
  hp = 128;
  tabn = 0;
  str = s;
  parseMore(str_get);
}

/* Since we store application nodes as [f, x] in adjacent memory */
/* locations, we can get the nth argument by: */
u arg(u n) { return mem[sp[n] + 1]; }

/* If the argument is a number, then we call arg to get the pointer to */
/* the number in memory, then find its value by indexing into mem. */
u num(u n) { return mem[arg(n) + 1]; }

void lazy(u height, u f, u x) {
  u *p = mem + sp[height];
  *p = f;
  *++p = x;
  sp += height;
}

u apparg(u i, u j) { return app(arg(i), arg(j)); }

void run(u (*get)(), void (*put)(u)) {
  u c;
  for (;;) {
    /* static int ctr; if (++ctr == (1<<25)) stats(), ctr = 0; */
    if (mem + hp > sp - 8)
      gc();
    u x = *sp;
    if (isAddr(x))
      *--sp = mem[x];
    else
      switch (x) {
      case FORWARD:
        stats();
        die("stray forwarding pointer");
      case '.':
        return;
      case 'Y':
        /* fix */
        /* Y x = x (x (x ...)) */
        lazy(1, arg(1), sp[1]);
        break;
      case 'S':
        /* ap */
        /* S x y z = x z (y z) */
        lazy(3, apparg(1, 3), apparg(2, 3));
        break;
      case 'B':
        /* (.) */
        /* B x y z = x (y z) */
        lazy(3, arg(1), apparg(2, 3));
        break;
      case 'C':
        /* flip */
        /* C x y z = x z y */
        lazy(3, apparg(1, 3), arg(2));
        break;
      case 'R':
        /* flip flip */
        /* R x y z = y z x */
        lazy(3, apparg(2, 3), arg(1));
        break;
      case 'I':
        /* id */
        /* I x = x */
        sp[1] = arg(1);
        sp++;
        break;
      case 'T':
        /* (&) */
        /* T x y = y x */
        lazy(2, arg(2), arg(1));
        break;
      case 'K':
        /* K x y = x */
        lazy(2, 'I', arg(1));
        break;
      case ':':
        /* "cons" */
        /* : a b c d = (d a) b */
        lazy(4, apparg(4, 1), arg(2));
        break;
      case '0':
        /* Read a character c from the input */
        /* If c == 0, then I K (represents nil) */
        /* else : (# c) (0 ?)  (represents a list of the first */
        /*                      character and the rest of the input) */
        c = get();
        !c ? lazy(1, 'I', 'K') : lazy(1, app(':', app('#', c)), app('0', '?'));
        break;
      case '#':
        /* reduce # n f to f (# n) */
        lazy(2, arg(2), sp[1]);
        break;
      case '1':
        put(num(1));
        lazy(2, app(arg(2), '.'), app('T', '1'));
        break;
      case '=':
        num(1) == num(2) ? lazy(2, 'I', 'K') : lazy(2, 'K', 'I');
        break;
      case 'L':
        num(1) <= num(2) ? lazy(2, 'I', 'K') : lazy(2, 'K', 'I');
        break;
      case '*':
        lazy(2, '#', num(1) * num(2));
        break;
      case '/':
        lazy(2, '#', num(1) / num(2));
        break;
      case '%':
        lazy(2, '#', num(1) % num(2));
        break;
      case '+':
        lazy(2, '#', num(1) + num(2));
        break;
      case '-':
        lazy(2, '#', num(1) - num(2));
        break;
      case '|':
        lazy(2, '#', num(1) | num(2));
        break;
      case '&':
        lazy(2, '#', num(1) & num(2));
        break;
      case '^':
        lazy(2, '#', num(1) ^ num(2));        
      case 'G':
        /* getc k w = k n w */
        /* Where k is the continuation, w is the "world" */
        lazy(2, app(arg(1), getchar()), arg(2));
        break;
      case 'P':
        /* putc n k w = k w */
        /* k is the continuation, w is the "world" */
        putchar(num(1));
        lazy(3, arg(2),arg(3));
        break;
      default:
        printf("?%u\n", x);
        die("Unknown combinator");
      }
  }
}

char buf[BUFMAX];
char *bufptr, *buf_end;
void buf_reset() { bufptr = buf; }
void buf_put(u c) {
  if (bufptr == buf_end)
    die("Buffer overflow");
  *bufptr++ = c;
}

FILE *file;
void pcFile(u c) {
  putc(c, file);
  fflush(file);
}
FILE *fp;
void fp_reset(const char *f) {
  fp = fopen(f, "r");
  if (!fp) {
    fprintf(stderr, "Error: File %s not found!\n", f);
    exit(1);
  }
}

u fp_get() {
  u c = fgetc(fp);
  return c == EOF ? fclose(fp), 0 : c;
}

void pc(u c) {
  putchar(c);
  fflush(stdout);
}

void lvlup(char *prog) {
  parse(buf);
  str = prog;
  buf_reset();
  run(str_get, buf_put);
  *bufptr = 0;
}

void lvlup_file(const char *filename) {
  printf("Loading %s...\n", filename);
  parse(buf);
  fp_reset(filename);
  buf_reset();
  run(fp_get, buf_put);
  *bufptr = 0;
}

u getc_stdin() { return getchar(); }

void init_vm() {
  mem = arena[0];
  altmem = arena[1];
  buf_end = buf + BUFMAX;
  spTop = mem + TOP - 1;
  strcpy(buf, "I;");
  bufptr = buf + 2;
  gc_count = 0;
}

void run_with_input(char *prog, char *input) {
  buf_reset();
  str = input;
  strncpy(buf, prog, strlen(prog));
  parse(buf);
  run(str_get, pc);
}

void get_input() {
  input[0] = 0;
  do {
    if (fgets(input, sizeof(input), stdin) == 0) {
      exit(0);
    }
  } while (input[0] == '\0');
}
int main(int argc, const char **argv) {
  if (argc != 4) {
    printf("Usage: ./blynn <binary> <input> <output>\n");
    exit(1);
  }
  FILE *f = fopen(argv[2], "r");
  if (!f) {
    printf("File not found: %s\n", argv[1]);
    exit(1);
  }
  fclose(f);
  f = fopen(argv[3], "a");
  if (!f) {
    printf("File not found: %s\n", argv[2]);
    exit(1);
  }
  fclose(f);
  init_vm();

  /* Load lvlup_file into memory */
  lvlup_file(argv[1]);
  stats();
  /* Parse the lvlup_file. (upgrades the compiler from ION assembly to */
  /* a subset of Haskell) */
  parse(buf);

  /* Reset the input file to be argv[1], and open as writable. */
  fp_reset(argv[2]);
  file = fopen(argv[3], "w");
  /* Run it!  Input comes from the file argv[1], output goes to the */
  /* file argv[2]. */
  run(fp_get, pcFile);
  fclose(file);
  printf("Input file compiled.\n");
  printf("Run binary on input from stdin? [Y/n] ");
  fflush(stdout);

  get_input();
  if (input[0] == '\n' || input[0] == 'y') {
    input[0] = 0;
    init_vm();
    lvlup_file(argv[3]);
    parse(buf);
    printf("Input: ");
    fflush(stdout);
    get_input();
    str = input;
    run(str_get, pc);
    puts("");
    stats();
  }
  return 0;
}
