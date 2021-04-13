
#pragma once

#include <iterator>
#include <list>

struct Integer
{
	const char *value;
};

typedef const char *String;

struct Functor
{
	const char *name;
	int arity;
};

template<typename T>
class List
{
	struct Node
	{
		T *m_elem;
		Node *m_next;
	};
	Node *m_first;

	constexpr List(Node *first) : m_first(first) {}
public:
	class iterator : std::iterator<std::forward_iterator_tag, T>
	{
		friend class List;
		Node *m_current;
	public:
		iterator(Node *current) : m_current(current) {}
		T& operator*() { return *m_current->m_elem; }
		bool operator!=(iterator it) { return m_current != it.m_current; }
		iterator& operator++() { m_current = m_current->m_next; return *this; }
	};

	constexpr static List nil = List { nullptr };

	bool empty() { return m_first == nullptr; }

	iterator begin() { return { m_first }; }
	iterator end() { return { nullptr }; }

	iterator insert(iterator& it, T elem)
	{
        T *e = new T(elem);
        Node *bk = it.m_current;
		if (it.m_current == nullptr)
        {
            it.m_current = new Node { e, nullptr };
            if (this->m_first == NULL) this->m_first = it.m_current;
        }
        else
        {
            it.m_current->m_next = new Node { e, it.m_current->m_next };
            it.m_current = it.m_current->m_next;
        }
        return { bk };
	}

	/*
	List(std::list<T> l) : m_first(nullptr) {
		iterator it = begin();
		for (T& e : l) insert(it, e);
	}
	operator std::list<T>() {
		std::list<T> l;
		for (T& e : *this) l.push_back(e);
		return l;
	}
	*/
};

struct Datatype
{
	String name;
	struct Constructor
	{
		String name;
		struct Accessor
		{
			String name;
			String type;
		};
		List<Accessor> accs;
	};
	List<Constructor> ctors;
};

struct Term
{
	Functor functor;
	List<Term> args;
};