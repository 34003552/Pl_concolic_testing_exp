#ifndef SCOPEDCONTAINER_H
#define SCOPEDCONTAINER_H

#include <vector>
#include <list>
#include <deque>
#include <map>
#include <unordered_map>
#include <optional>
#include <iterator>
#include <type_traits>

template<class Container>
class ScopedBaseContainer {
protected:
    std::list<Container> m_containers;
public:
    using value_type = typename Container::value_type;

    class iterator : public std::iterator<std::forward_iterator_tag, value_type> {
        using lc_iterator = typename std::list<Container>::iterator;
        using oc_iterator = std::optional<typename Container::iterator>;

        ScopedBaseContainer *m_owner;
        lc_iterator m_lci;
        oc_iterator m_ci;
    public:
        iterator(ScopedBaseContainer *owner, lc_iterator lci, oc_iterator ci) :
        m_owner(owner), m_lci(lci), m_ci(ci) {}
        bool operator!=(const iterator& other) {
            return m_lci != other.m_lci || m_ci != other.m_ci;
        }
        value_type& operator*() {
            return *m_ci.value();
        }
        iterator operator++() {
            ++m_ci.value();
            if (m_ci == m_lci->end()) do {
                ++m_lci;
                if (m_lci == m_owner->m_containers.end()) {
                    m_ci = std::nullopt;
                    break;
                }
                else if (!m_lci->empty()) {
                    m_ci = m_lci->begin();
                    break;
                }
            } while (true);
            return *this;
        }
    };
    class const_iterator : public iterator {
    public:
        explicit const_iterator(iterator it) : iterator(it) {}
        const value_type& operator*() {
            return this->iterator::operator*();
        }
    };

    ScopedBaseContainer() : m_containers({ {} }) {}

    bool empty() const {
        for (auto& container : m_containers) if (!container.empty()) return false;
        return true;
    }

    void push_scope() {
        m_containers.push_back({});
    }
    void pop_scope(unsigned s) {
        auto it = m_containers.begin();
        for (unsigned i = 0; i <= s && it != m_containers.end(); i++) ++it;
        m_containers.erase(it, m_containers.end());
    }

    iterator begin() {
        auto it = this->m_containers.begin();
        while (it->empty()) {
            ++it;
            if (it == this->m_containers.end()) {
                return iterator(this, it, std::nullopt);
            }
        }
        return iterator(this, it, it->begin());
    }
    const_iterator cbegin() const {
        return const_iterator(const_cast<ScopedBaseContainer*>(this)->begin());
    }
    const_iterator begin() const {
        return cbegin();
    }

    iterator end() {
        auto it = this->m_containers.end();
        return iterator(this, it, std::nullopt);
    }
    const_iterator cend() const {
        return const_iterator(const_cast<ScopedBaseContainer*>(this)->end());
    }
    const_iterator end() const {
        return cend();
    }
};
template<class Container, class Enable = void>
class ScopedContainer;
template<class Container>
class ScopedContainer<Container,
    std::enable_if_t<
        std::is_same_v<Container, std::vector<typename Container::value_type>>  ||
        std::is_same_v<Container, std::list<typename Container::value_type>>    ||
        std::is_same_v<Container, std::deque<typename Container::value_type>>
    >> : public ScopedBaseContainer<Container> {
public:
    using value_type = typename Container::value_type;

    template<typename T>
    void push_back(T&& val) {
        this->m_containers.back().push_back(std::forward<T>(val));
    }
    value_type& back() {
        return this->m_containers.back().back();
    }
};
template<class Container>
class ScopedContainer<Container,
    std::enable_if_t<
        std::is_same_v<Container, std::map<typename Container::key_type, typename Container::mapped_type>>          ||
        std::is_same_v<Container, std::unordered_map<typename Container::key_type, typename Container::mapped_type>>
    >> : public ScopedBaseContainer<Container> {
public:
    using key_type = typename Container::key_type;
    using mapped_type = typename Container::mapped_type;
    using iterator = typename ScopedBaseContainer<Container>::iterator;

    template<typename... Args>
    std::pair<iterator,bool> emplace(Args&&... args) {
        auto lci = std::prev(this->m_containers.end());
        auto [ci, ok] = lci->emplace(std::forward<Args>(args)...);
        return { iterator(this, lci, ci), ok };
    }
    template<typename K>
    iterator find(const K& key) {
        for (auto lci = this->m_containers.begin(); lci != this->m_containers.end(); ++lci) {
            if (auto ci = lci->find(key); ci != lci->end()) {
                return iterator(this, lci, ci);
            }
        }
        return this->end();
    }
    mapped_type& at(const key_type& key) {
        for (auto& container : this->m_containers) {
            if (auto ci = container.find(key); ci != container.end()) return ci->second;
        }
        throw std::out_of_range("map::at");
    }
};

#endif