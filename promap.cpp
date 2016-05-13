#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <map>

template<class T>
T id(T x) {return x;}

// Using "has a", instead of "is a", to make immutability enforcement easier.
template<
    class Key,
    class T
> class promap
{
  public:
    // I'm only providing initial and copy constructors.
    promap( const std::map<Key, T>* the_map )
    {
        _keys.clear();
        my_map = the_map;
        for(auto elem : *(this->my_map))
            _keys.push_back(elem.first);
    }

    // Piggy-backing on other's my_map should be okay, since I'm enforcing immutability.
    promap( const promap& other )
    {
        _keys.clear();
        my_map = other.my_map;
        for(auto elem : *(this->my_map))
            _keys.push_back(elem.first);
    }

    // Access is read-only, of course.
    T operator[](const Key& key) const
    {
        return my_map->at(key);
    }

    // Fetching keys is handy.
    const std::vector<Key>& keys() const { return _keys; }

    // Need '==' definition, for testing.
    bool operator==(const promap& other) const
    {
        if(other.my_map->size() != this->my_map->size())
            return false;

        for(auto key : keys())
            if(other[key] != (*this)[key])
                return false;

        return true;
    }

  protected:
    const std::map<Key, T>* my_map=nullptr;

  private:
    // Holds the keys in the underlying map, for fetching via keys().
    std::vector<Key> _keys;

    template<class Key1, class A, class B>
    friend const promap<Key1, B>* rmap(B (*g)(A), const promap<Key1, A>& f);
};

// rmap
template<class Key1, class A, class B>
const promap<Key1, B>* rmap(B (*g)(A), const promap<Key1, A>& f)
{
    auto new_map = new std::map<Key1, B>(*(f.my_map));
    for(auto key : f.keys())
        new_map->operator[](key) = g(f[key]);
    auto res = new promap<Key1, B> (new_map);
    return res;
}

int g(int x) {return(x + 1);}

int h(int x) {return(x * 2);}

int g_comp_h(int x) {return x * 2 + 1;}

int main(int argc, char *argv[])
{
    std::vector<int> xs;
    xs.clear();
    xs.push_back(0);
    xs.push_back(1);
    xs.push_back(2);

    std::map<int, int> the_map;
    the_map.clear();
    int ix = 0;
    for(auto x : xs)
        the_map[ix++] = x;

    promap<int, int> my_promap(&the_map);

    // Test Functor identity law.
    const promap<int, int>* new_promap = rmap<int, int, int>(id, my_promap);
    if(*new_promap == id(my_promap))
        printf("Functor identity law held.\n");
    else
        printf("Functor identity law did not hold!\n");

    // Test Functor composition law.
    const promap<int, int>* tmp_promap  = rmap<int, int, int> (h, my_promap);
    const promap<int, int>* new_promap2 = rmap<int, int, int> (g, *tmp_promap);
    const promap<int, int>* new_promap3 = rmap<int, int, int> (g_comp_h, my_promap);
    if(*new_promap2 == *new_promap3)
        printf("Functor composition law held.\n");
    else
        printf("Functor composition law did not hold!\n");
}

