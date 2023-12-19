#ifndef UNIQUE_PTR
#define UNIQUE_PTR

#include <functional>

template <typename T>
class SharedPtr {
public:
    SharedPtr()
    {
        ptr = nullptr;
        count = nullptr;
        return;
    }
    explicit SharedPtr(T* pointer)
    {
        ptr = pointer;
        if (ptr != nullptr)
            count = new size_t(1);
        return;
    }
    ~SharedPtr()
    {
        del();
        return;
    }
    void del()
    {
        if (ptr != nullptr) {
            (*count)--;
            if (*count == 0) {
                delete ptr;
                delete count;
            }
            ptr = nullptr;
            count = nullptr;
        }
        return;
    }

    SharedPtr(const SharedPtr& other)
    {
        ptr = other.ptr;
        count = other.count;
        if (ptr != nullptr)
            (*count)++;
        return;
    }
    SharedPtr& operator=(const SharedPtr& other)
    {
        if (ptr != other.ptr) {
            del();
            ptr = other.ptr;
            count = other.count;
            if (ptr != nullptr)
                (*count)++;
        }
        return *this;
    }

    operator bool() const
    {
        return ptr;
    }
    size_t use_count() const
    {
        if (count == nullptr)
            return 0;
        return *count;
    }
    T* get() const
    {
        return ptr;
    }
    T& operator*() const
    {
        return *ptr;
    }
    T* operator->() const
    {
        return ptr;
    }
    void reset()
    {
        del();
        return;
    }
    void reset(T* new_pointer)
    {
        if (ptr != new_pointer) {
            del();
            ptr = new_pointer;
            if (ptr != nullptr)
                count = new size_t(1);
        }
        return;
    }

private:
    T* ptr;
    size_t* count;
};

template <typename T, typename... argv>
SharedPtr<T> make_shared(argv&&... val)
{
    return SharedPtr<T>(new T(std::forward<argv>(val)...));
}

#endif // SHARED_PTR