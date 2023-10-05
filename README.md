[![CMake MSVC Build and Test](https://github.com/RedSkittleFox/stacktree/actions/workflows/cmake-msvc-build.yml/badge.svg)](https://github.com/RedSkittleFox/stacktree/actions/workflows/cmake-msvc-build.yml)
# stacktree
A way of storing and representing stacktrace's as a tree with attached data!

Features:
* Follows STL design - provides iterators and custom allocator support. 
* Documentation is available in code.
* Header only.
* Fully tested.

# [Example](https://github.com/RedSkittleFox/stacktree/blob/main/sample/main.cpp)
```cpp
#include <iostream>
#include <fstream>
#include <string>
#include <thread>

#include <fox/stack_tree.hpp>

// Just a dummy
struct global_allocator
{
	struct allocation
	{
		size_t allocation_size;
		std::stacktrace st;
	};

	std::vector<allocation> allocations;

	[[nodiscard]] void* allocate(size_t size)
	{
		auto st = std::stacktrace::current(1);
		allocations.emplace_back(size, std::move(st));
		return nullptr;
	}

	void deallocate(void* ptr)
	{
		(void)ptr;
	}

	struct allocation_size
	{
		size_t size = {};
	};

	[[nodiscard]] fox::stacktree<allocation_size> get_allocation_telemetry() const
	{
		fox::stacktree<allocation_size> out;

		for(const auto& a : allocations)
		{
			out.insert(a.st, [&](auto& v) { v.value().size += a.allocation_size; });
		}

		return out;
	}
} alloc;

std::ostream& operator<<(std::ostream& os, global_allocator::allocation_size s)
{
	return os << s.size << " Bytes";
}

void do_something_funny()
{
	for(auto i : std::views::iota(0, 10))
	{
		auto ptr = alloc.allocate(i * sizeof(std::string));
		alloc.deallocate(ptr);
	}
}

void do_something_not_funny()
{
	auto ptr = alloc.allocate(1024 * 1024);
	alloc.deallocate(ptr);
}

int main()
{
	do_something_funny();
	do_something_not_funny();

	const auto tree = alloc.get_allocation_telemetry();

	std::cout << tree << '\n';

	return 0;
}
```

```
ntdll!RtlUserThreadStart+0x28 : 1050376 Bytes
  KERNEL32!BaseThreadInitThunk+0x1D : 1050376 Bytes
    stacktree_demo!mainCRTStartup+0xE : 1050376 Bytes
      stacktree_demo!__scrt_common_main+0xE : 1050376 Bytes
        stacktree_demo!__scrt_common_main_seh+0x12E : 1050376 Bytes
          stacktree_demo!invoke_main+0x39 : 1050376 Bytes
            ...\stacktree\sample\main.cpp(73): stacktree_demo!main+0x1C : 1800 Bytes
              ...\stacktree\sample\main.cpp(59): stacktree_demo!do_something_funny+0xC7 : 1800 Bytes
            ...\stacktree\sample\main.cpp(75): stacktree_demo!main+0x21 : 1048576 Bytes
              ...\stacktree\sample\main.cpp(66): stacktree_demo!do_something_not_funny+0x17 : 1048576 Bytes
```

# Requirement
* C++23 or higher 
* `<stacktrace>` support. Right now this library only supports MSVC until GCC and clang provide support for the C++23's `<stacktrace>` library.
* No external dependencies are requires, stacktree is a header-only library.

# Installation
```
git clone https://github.com/RedSkittleFox/stacktree.git
mkdir stacktree/output
cd stakctree/output
cmake -Dsample=ON -Dtest=on .. 


```cmake
include(FetchContent)
FetchContent_Declare(
    stacktree
    GIT https://github.com/RedSkittleFox/stacktree.git
)

FetchContent_MakeAvailable(stacktree)

...

target_link_libraries(your_library PUBLIC fox::stacktree)
```
