
typedef enum { false, true } bool;
char *bool__to_chars(bool *this);

char *int__to_chars(int *this);

typedef struct { const char* value; } Integer;
char *Integer__to_chars(Integer *this);

typedef const char* String;
char *String__to_chars(String *this);

typedef struct { String name; int arity; } Functor;
char *Functor__to_chars(Functor *this);

#define DECLARE_LIST(T)\
	typedef struct T##Node T##Node;\
	struct T##Node\
	{\
		T *m_elem;\
		T##Node *m_next;\
	};\
	typedef struct { T##Node *m_first; } T##List;\
	typedef struct { T##Node *m_current; } T##ListIterator;\
	T##List const T##List__nil;\
	T##ListIterator T##List__insert(T##List *this, T##ListIterator *it, T elem);\
    void T##List__clear(T##List *this);\
    bool T##List__empty(T##List *this);\
    T##ListIterator T##List__begin(T##List *this);\
    T##ListIterator T##List__end(T##List *this);\
    char *T##List__to_chars(T##List *this);

DECLARE_LIST(String);
DECLARE_LIST(Functor);

typedef struct Term Term;
DECLARE_LIST(Term);
struct Term
{
	Functor functor;
	TermList args;
};
char *Term__to_chars(Term *this);

typedef struct { String name; String type; } Datatype__Constructor__Accessor;
char *Datatype__Constructor__Accessor__to_chars(Datatype__Constructor__Accessor *this);
DECLARE_LIST(Datatype__Constructor__Accessor);

typedef struct { String name; Datatype__Constructor__AccessorList accs; } Datatype__Constructor;
char *Datatype__Constructor__to_chars(Datatype__Constructor *this);
DECLARE_LIST(Datatype__Constructor);

typedef struct { String name; Datatype__ConstructorList ctors; } Datatype;
char *Datatype__to_chars(Datatype *this);
DECLARE_LIST(Datatype);

#define typename(V) _Generic((V),\
	bool: "bool",\
	int: "int",\
	Integer: "Integer",\
	String: "String",\
	Functor: "Functor",\
	Term: "Term",\
	Datatype: "Datatype",\
	\
	StringList: "String[]",\
	FunctorList: "Functor[]",\
	TermList: "Term[]",\
	DatatypeList: "Datatype[]")

#define to_chars(V) _Generic((*V),\
	bool: bool__to_chars,\
	int: int__to_chars,\
	Integer: Integer__to_chars,\
	String: String__to_chars,\
	Functor: Functor__to_chars,\
	Term: Term__to_chars,\
	Datatype: Datatype__to_chars,\
	\
	StringList: StringList__to_chars,\
	FunctorList: FunctorList__to_chars,\
	TermList: TermList__to_chars,\
	DatatypeList: DatatypeList__to_chars)(V)
