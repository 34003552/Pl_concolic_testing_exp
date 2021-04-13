#include "ctypes.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *bool__to_chars(bool *this)
{
	return strdup(*this ? "true" : "false");
}
char *int__to_chars(int *this)
{
	size_t len = 20 + 1;
	char *str = malloc(len);
	snprintf(str, len, "%d", *this);
	return str;
}
char *Integer__to_chars(Integer *this)
{
	return strdup(this->value);
}
char *String__to_chars(String *this)
{
	size_t len = 1 + strlen(*this) + 1 + 1;
	char *str = malloc(len);
	sprintf(str, "\"%s\"", *this);
	return str;
}
char *Functor__to_chars(Functor *this)
{
	size_t len = strlen(this->name) + 1 + 2 + 1; // null-terminated
	char *str = malloc(len);
	snprintf(str, len, "%s/%d", this->name, this->arity);
	return str;
}

#define DEFINE_LIST(T)\
	T##List const T##List__nil = { .m_first = NULL };\
	T##ListIterator T##List__insert(T##List *this, T##ListIterator *it, T elem) {\
		T *e = malloc(sizeof(T));\
		memcpy(e, &elem, sizeof(T));\
		T##Node *bk = it->m_current;\
		T##Node *next;\
        if (it->m_current == NULL)\
        {\
        	next = NULL;\
            it->m_current = malloc(sizeof(struct T##Node));\
            if (this->m_first == NULL) this->m_first = it->m_current;\
        }\
        else\
        {\
        	next = it->m_current->m_next;\
            it->m_current->m_next = malloc(sizeof(struct T##Node));\
            it->m_current = it->m_current->m_next;\
        }\
        it->m_current->m_elem = e;\
        it->m_current->m_next = next;\
        return (T##ListIterator) { bk };\
    }\
    void T##List__clear(T##List *this) {\
    	T##ListIterator li = { .m_current = this->m_first };\
	    while (li.m_current != NULL)\
	    {\
	        T##ListIterator li2 = { .m_current = li.m_current->m_next };\
	        free(li.m_current);\
	        li = li2;\
	    }\
    }\
    bool T##List__empty(T##List *this)\
    {\
    	return this->m_first == NULL;\
    }\
    T##ListIterator T##List__begin(T##List *this)\
    {\
    	return (T##ListIterator) { .m_current = this->m_first };\
    }\
    T##ListIterator T##List__end(T##List *this)\
    {\
    	return (T##ListIterator) { .m_current = NULL };\
    }\
    char *T##List__to_chars(T##List *this) {\
    	if (this->m_first == NULL) return strdup("{}");\
    	T##ListIterator li = T##List__begin(this);\
    	char *sstr = T##__to_chars(li.m_current->m_elem);\
    	size_t len = 2 + strlen(sstr) + 2 + 1;\
    	char *str = malloc(len);\
    	strcpy(str, "{ ");\
		strcat(str, sstr);\
		li = (T##ListIterator) { .m_current = li.m_current->m_next };\
		while (li.m_current != NULL)\
		{\
			sstr = T##__to_chars(li.m_current->m_elem);\
			len += 2 + strlen(sstr) + 1;\
    		str = realloc(str, len);\
			strcat(str, ", ");\
			strcat(str, sstr);\
			li = (T##ListIterator) { .m_current = li.m_current->m_next };\
		}\
		strcat(str, " }");\
    	return str;\
    }

DEFINE_LIST(String);
DEFINE_LIST(Functor);

DEFINE_LIST(Term);
char *Term__to_chars(Term *this)
{
	char *name = (char*)this->functor.name;

	size_t len = strlen(name) + 1;
	char *str = malloc(len);
	strcpy(str, name);

	TermListIterator tli = { .m_current = this->args.m_first };
	if (tli.m_current != NULL)
	{
		strcat(str, "(");
		char *sstr = Term__to_chars(tli.m_current->m_elem);
		len += 1 + strlen(sstr) + 1;
		str = realloc(str, len);
		strcat(str, sstr);
		tli = (TermListIterator) { .m_current = tli.m_current->m_next };
		while (tli.m_current != NULL)
		{
			sstr = Term__to_chars(tli.m_current->m_elem);
			len += 2 + strlen(sstr);
			str = realloc(str, len);
			strcat(str, ", ");
			strcat(str, sstr);
			tli = (TermListIterator) { .m_current = tli.m_current->m_next };
		}
		strcat(str, ")");
	}
	return str;
}

char *Datatype__Constructor__Accessor__to_chars(Datatype__Constructor__Accessor *this)
{
	size_t len = 10 + (1 + strlen(this->name) + 1) + 10 + (1 + strlen(this->type) + 1) + 2 + 1;
	char *str = malloc(len);
	sprintf(str, "{ .name = %s, .type = %s }", String__to_chars(&this->name), String__to_chars(&this->type));
	return str;
}
DEFINE_LIST(Datatype__Constructor__Accessor);

char *Datatype__Constructor__to_chars(Datatype__Constructor *this)
{
	char *sstr = Datatype__Constructor__AccessorList__to_chars(&this->accs);
	size_t len = 10 + (1 + strlen(this->name) + 1) + 10 + (1 + strlen(sstr) + 1) + 2 + 1;
	char *str = malloc(len);
	sprintf(str, "{ .name = %s, .accs = %s }", String__to_chars(&this->name), sstr);
	return str;
}
DEFINE_LIST(Datatype__Constructor);

char *Datatype__to_chars(Datatype *this)
{
	char *sstr = Datatype__ConstructorList__to_chars(&this->ctors);
	size_t len = 10 + (1 + strlen(this->name) + 1) + 11 + (1 + strlen(sstr) + 1) + 2 + 1;
	char *str = malloc(len);
	sprintf(str, "{ .name = %s, .ctors = %s }", String__to_chars(&this->name), sstr);
	return str;
}
DEFINE_LIST(Datatype);